state_levels <- c("knowledge", "lean correct", "midpoint", "lean incorrect", "misinformation")

# Ratings run from 0 (definitely false) to 10 (definitely true). Flipping them
# for false propositions puts every item on one scale where 10 is the right end.
toward_truth <- \(rating, truth) dplyr::if_else(truth, rating, 10 - rating)

# Strict scoring takes only the scale's endpoints as confident belief; lenient
# scoring also takes the next point in.
belief_state <- function(rating, truth, lenient = FALSE) {
  score <- toward_truth(rating, truth)
  edge <- if (lenient) 1 else 0
  state <- dplyr::case_when(
    score >= 10 - edge ~ "knowledge",
    score <= edge ~ "misinformation",
    score > 5 ~ "lean correct",
    score < 5 ~ "lean incorrect",
    score == 5 ~ "midpoint"
  )
  factor(state, levels = state_levels)
}

scale_ratings <- function(frame, propositions) {
  kept <- dplyr::filter(propositions, keep)
  frame |>
    dplyr::filter(arm == "scale") |>
    dplyr::select(study, respondent, party, interest, dplyr::any_of(unique(kept$column))) |>
    tidyr::pivot_longer(-c(study, respondent, party, interest), names_to = "column", values_to = "rating") |>
    dplyr::inner_join(kept, by = c("study", "column"), relationship = "many-to-one") |>
    dplyr::mutate(
      strict = belief_state(rating, truth),
      lenient = belief_state(rating, truth, lenient = TRUE),
      # A skipped or "not applicable" rating is the scale's don't-know.
      misinformed_strict = dplyr::coalesce(strict == "misinformation", FALSE),
      misinformed_lenient = dplyr::coalesce(lenient == "misinformation", FALSE),
      knows_strict = dplyr::coalesce(strict == "knowledge", FALSE)
    ) |>
    assertr::assert(assertr::within_bounds(0, 10), rating)
}

classify_mc <- function(response, options) {
  text <- normalize_text(response)
  hits <- purrr::map(options$pattern, \(p) stringr::str_detect(text, p))
  matched <- purrr::map_int(seq_along(text), \(i) {
    which_hit <- which(purrr::map_lgl(hits, \(h) isTRUE(h[i])))
    if (length(which_hit) > 1) stop("Response matches more than one option: ", response[i])
    if (length(which_hit) == 0) NA_integer_ else which_hit
  })
  unmatched <- !is.na(text) & is.na(matched)
  if (any(unmatched)) stop("Unmatched responses: ", paste(unique(response[unmatched]), collapse = " | "))
  dplyr::case_when(
    is.na(text) ~ "no answer",
    is.na(options$correct[matched]) ~ "don't know",
    options$correct[matched] ~ "correct",
    .default = "incorrect"
  )
}

mc_responses <- function(frame, mc_options) {
  frame |>
    dplyr::filter(arm %in% c("mc", "mc_media")) |>
    dplyr::select(study, respondent, arm, party, interest, dplyr::starts_with("mc_")) |>
    tidyr::pivot_longer(dplyr::starts_with("mc_"), names_to = "question", values_to = "response") |>
    dplyr::mutate(question = stringr::str_remove(question, "^mc_")) |>
    dplyr::semi_join(mc_options, by = c("study", "question")) |>
    dplyr::group_by(study, question) |>
    dplyr::group_modify(\(d, key) {
      options <- dplyr::semi_join(mc_options, key, by = c("study", "question"))
      dplyr::mutate(d, outcome = classify_mc(response, options))
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      incorrect = outcome == "incorrect",
      correct = outcome == "correct",
      dont_know = outcome %in% c("don't know", "no answer")
    )
}

# On the scale, a respondent holds misinformation relevant to a question when
# any of its propositions gets a confidently wrong rating. With several
# propositions per question this errs toward finding misinformation.
scale_by_question <- function(ratings) {
  ratings |>
    dplyr::group_by(study, question, respondent, party, interest) |>
    dplyr::summarise(
      misinformed_strict = any(misinformed_strict),
      misinformed_lenient = any(misinformed_lenient),
      propositions = dplyr::n(),
      .groups = "drop"
    )
}
