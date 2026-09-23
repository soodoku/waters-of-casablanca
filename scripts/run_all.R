purrr::walk(list.files("R", full.names = TRUE), source)

verify_sources()
frame <- dplyr::bind_rows(read_alumni(), read_staff(), read_mturk_march(), read_mturk_july())
propositions <- read_propositions()
ratings <- scale_ratings(frame, propositions)
mc <- mc_responses(frame, read_mc_options())
by_question <- scale_by_question(ratings)

dir.create("tabs", showWarnings = FALSE)
write_output <- \(x, name) readr::write_csv(x, file.path("tabs", name), na = "")

write_output(balance(frame), "balance.csv")
write_output(belief_states(ratings), "belief_states.csv")
write_output(prevalence(ratings), "prevalence.csv")

comparison <- format_comparison(mc, by_question)
write_output(comparison, "format_comparison.csv")
write_output(format_differences(comparison), "format_differences.csv")
c("misinformed_strict", "misinformed_lenient") |>
  purrr::map(\(m) pooled_format_effect(mc, by_question, m)) |>
  purrr::list_rbind() |>
  write_output("format_pooled.csv")

mc |>
  dplyr::count(study, arm, question, outcome) |>
  write_output("mc_outcomes.csv")

write_output(invention_denial(ratings), "invention_denial.csv")
ratings |>
  dplyr::mutate(kind = dplyr::if_else(truth, "denial", "invention")) |>
  dplyr::group_by(study) |>
  dplyr::group_modify(\(d, k) cluster_mean_difference(d, "misinformed_strict", "kind")) |>
  dplyr::ungroup() |>
  write_output("invention_denial_test.csv")

mturk <- c("mturk_march", "mturk_july")
dplyr::bind_rows(
  ratings |>
    dplyr::filter(study %in% mturk) |>
    partisan_gaps("misinformed_strict", c("study", "prop_id", "question", "text", "short", "truth", "congenial")) |>
    dplyr::mutate(measure = "scale_strict"),
  ratings |>
    dplyr::filter(study %in% mturk) |>
    partisan_gaps("misinformed_lenient", c("study", "prop_id", "question", "text", "short", "truth", "congenial")) |>
    dplyr::mutate(measure = "scale_lenient"),
  mc |>
    dplyr::filter(study == "mturk_july") |>
    dplyr::mutate(measure = dplyr::if_else(arm == "mc", "mc", "mc_media")) |>
    partisan_gaps("incorrect", c("study", "question", "measure")) |>
    dplyr::left_join(
      dplyr::filter(propositions, study == "mturk_july") |>
        dplyr::select(study, question, prop_id, text, short, truth, congenial),
      by = c("study", "question")
    )
) |>
  write_output("partisan_gaps.csv")

write_output(interest_gradient(ratings), "interest_gradient.csv")
