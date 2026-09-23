study_labels <- c(
  alumni = "Alumni 2010",
  staff = "Staff 2010",
  mturk_march = "MTurk March 2017",
  mturk_july = "MTurk July 2017"
)

# Wilson intervals stay inside [0, 1] when shares are near zero, as most are.
wilson <- function(successes, n, z = qnorm(0.975)) {
  p <- successes / n
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  tibble::tibble(estimate = p, lower = centre - half, upper = centre + half)
}

share_with_ci <- function(data, outcome) {
  data |>
    dplyr::summarise(
      n = dplyr::n(),
      successes = sum({{ outcome }}),
      .groups = "drop"
    ) |>
    dplyr::mutate(wilson(successes, n))
}

belief_states <- function(ratings) {
  ratings |>
    dplyr::filter(!is.na(strict)) |>
    dplyr::count(study, prop_id, text, truth, congenial, strict, name = "count") |>
    tidyr::complete(tidyr::nesting(study, prop_id, text, truth, congenial), strict, fill = list(count = 0)) |>
    dplyr::group_by(study, prop_id) |>
    dplyr::mutate(share = count / sum(count), rated = sum(count)) |>
    dplyr::ungroup()
}

prevalence <- function(ratings) {
  strict <- ratings |>
    dplyr::group_by(study, prop_id, text, truth, congenial) |>
    share_with_ci(misinformed_strict) |>
    dplyr::mutate(scoring = "strict")
  lenient <- ratings |>
    dplyr::group_by(study, prop_id, text, truth, congenial) |>
    share_with_ci(misinformed_lenient) |>
    dplyr::mutate(scoring = "lenient")
  skipped <- ratings |>
    dplyr::group_by(study, prop_id) |>
    dplyr::summarise(skipped = mean(is.na(rating)), .groups = "drop")
  dplyr::bind_rows(strict, lenient) |>
    dplyr::left_join(skipped, by = c("study", "prop_id"))
}

format_comparison <- function(mc, by_question) {
  compared <- dplyr::distinct(mc, study, question)
  mc_rows <- mc |>
    dplyr::group_by(study, question, measure = dplyr::if_else(arm == "mc", "mc", "mc_media")) |>
    share_with_ci(incorrect)
  scale_rows <- by_question |>
    dplyr::semi_join(compared, by = c("study", "question")) |>
    tidyr::pivot_longer(c(misinformed_strict, misinformed_lenient), names_to = "measure", values_to = "value") |>
    dplyr::mutate(measure = stringr::str_replace(measure, "misinformed_", "scale_")) |>
    dplyr::group_by(study, question, measure) |>
    share_with_ci(value)
  dplyr::bind_rows(mc_rows, scale_rows)
}

# Differences in shares between randomly assigned arms, with the unpooled
# binomial standard error.
format_differences <- function(comparison) {
  mc <- dplyr::filter(comparison, measure == "mc")
  comparison |>
    dplyr::filter(measure != "mc") |>
    dplyr::inner_join(mc, by = c("study", "question"), suffix = c("", "_mc")) |>
    dplyr::mutate(
      difference = estimate - estimate_mc,
      std_error = sqrt(estimate * (1 - estimate) / n + estimate_mc * (1 - estimate_mc) / n_mc)
    ) |>
    dplyr::transmute(
      study, question, measure,
      estimate = difference,
      std_error,
      lower = difference - qnorm(0.975) * std_error,
      upper = difference + qnorm(0.975) * std_error
    )
}

# Pooled over questions, the format effect is the coefficient on the scale arm
# in a regression with question fixed effects, clustered by respondent.
pooled_format_effect <- function(mc, by_question, measure) {
  stacked <- dplyr::bind_rows(
    mc |>
      dplyr::filter(arm == "mc") |>
      dplyr::transmute(study, question, respondent, scale = 0, outcome = as.numeric(incorrect)),
    by_question |>
      dplyr::semi_join(dplyr::distinct(mc, study, question), by = c("study", "question")) |>
      dplyr::transmute(study, question, respondent, scale = 1, outcome = as.numeric(.data[[measure]]))
  ) |>
    dplyr::mutate(key = paste(study, question))
  fit_one <- function(data) {
    fit <- lm(outcome ~ scale + key, data = data)
    se <- sqrt(sandwich::vcovCL(fit, cluster = data$respondent, type = "HC1")["scale", "scale"])
    tibble::tibble(estimate = coef(fit)[["scale"]], std_error = se, n = nrow(data))
  }
  dplyr::bind_rows(
    stacked |> dplyr::group_by(study) |> dplyr::group_modify(\(d, k) fit_one(d)) |> dplyr::ungroup(),
    fit_one(stacked) |> dplyr::mutate(study = "pooled")
  ) |>
    dplyr::mutate(
      measure = measure,
      lower = estimate - qnorm(0.975) * std_error,
      upper = estimate + qnorm(0.975) * std_error
    )
}

cluster_mean_difference <- function(data, outcome, group) {
  fit <- lm(reformulate(group, outcome), data = data)
  se <- sqrt(diag(sandwich::vcovCL(fit, cluster = data$respondent, type = "HC1")))
  tibble::tibble(
    term = names(coef(fit)), estimate = unname(coef(fit)), std_error = unname(se)
  )
}

# Invention is confident belief in a falsehood, denial confident disbelief in a
# truth. The scale tells them apart; a multiple-choice answer does not.
invention_denial <- function(ratings) {
  ratings |>
    dplyr::mutate(kind = dplyr::if_else(truth, "denial", "invention")) |>
    dplyr::group_by(study, kind) |>
    dplyr::summarise(
      propositions = dplyr::n_distinct(prop_id),
      strict = mean(misinformed_strict),
      lenient = mean(misinformed_lenient),
      .groups = "drop"
    )
}

partisan_gaps <- function(data, outcome, keys) {
  data |>
    dplyr::filter(party %in% c("Democrat", "Republican")) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys)), party) |>
    dplyr::summarise(p = mean(.data[[outcome]]), n = dplyr::n(), .groups = "drop") |>
    tidyr::pivot_wider(names_from = party, values_from = c(p, n)) |>
    dplyr::mutate(
      estimate = p_Republican - p_Democrat,
      std_error = sqrt(p_Republican * (1 - p_Republican) / n_Republican + p_Democrat * (1 - p_Democrat) / n_Democrat),
      lower = estimate - qnorm(0.975) * std_error,
      upper = estimate + qnorm(0.975) * std_error
    )
}

# Within the scale arms: does political interest go with knowing, and with
# being misinformed? Proposition fixed effects; clustered by respondent.
interest_gradient <- function(ratings) {
  data <- ratings |>
    dplyr::filter(!is.na(interest)) |>
    dplyr::mutate(interest = interest / 10, key = paste(study, prop_id))
  purrr::map(c(knows_strict = "knows_strict", misinformed_strict = "misinformed_strict"), \(outcome) {
    fit <- lm(reformulate(c("interest", "key"), outcome), data = data)
    se <- sqrt(sandwich::vcovCL(fit, cluster = data$respondent, type = "HC1")["interest", "interest"])
    tibble::tibble(
      outcome = outcome, estimate = coef(fit)[["interest"]], std_error = se,
      lower = estimate - qnorm(0.975) * se, upper = estimate + qnorm(0.975) * se,
      respondents = dplyr::n_distinct(data$respondent), mean = mean(data[[outcome]])
    )
  }) |>
    purrr::list_rbind()
}

balance <- function(frame) {
  frame |>
    dplyr::filter(arm %in% c("mc", "scale", "mc_media")) |>
    dplyr::group_by(study, arm) |>
    dplyr::summarise(
      respondents = dplyr::n(),
      democrat = mean(party == "Democrat", na.rm = TRUE),
      republican = mean(party == "Republican", na.rm = TRUE),
      interest = mean(interest, na.rm = TRUE),
      .groups = "drop"
    )
}
