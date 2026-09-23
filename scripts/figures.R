purrr::walk(list.files("R", full.names = TRUE), source)

read_tab <- \(name) readr::read_csv(file.path("tabs", name), show_col_types = FALSE)
dir.create("figs", showWarnings = FALSE)

question_labels <- c(
  aca1 = "ACA: coverage, taxes, screening", aca2 = "ACA: Medicare",
  birth = "Obama born in the U.S.", religion = "Obama is a Muslim",
  illegal = "ACA helps illegal immigrants buy insurance", death = "ACA creates death panels",
  increase = "Warming is human-caused", science = "Most scientists doubt warming",
  fraud = "Trump won most legal votes", mmr = "MMR vaccine causes autism",
  deficit = "Deficit has risen since 2012"
)
measure_labels <- c(
  mc_media = "Media-poll wording, no DK",
  mc = "Multiple choice with DK",
  scale_lenient = "0-10 scale, lenient",
  scale_strict = "0-10 scale, strict"
)
measure_colours <- c(
  mc_media = "grey55", mc = "grey20", scale_lenient = "#6BAED6", scale_strict = "#08519C"
)

comparison <- read_tab("format_comparison.csv") |>
  dplyr::mutate(
    study = factor(study_labels[study], levels = study_labels),
    question = factor(question_labels[question], levels = rev(question_labels)),
    measure = factor(measure, levels = names(measure_labels))
  )

format_plot <- ggplot2::ggplot(
  comparison, ggplot2::aes(estimate, question, colour = measure)
) +
  geom_estimate(position = ggplot2::position_dodge(width = 0.7)) +
  ggplot2::facet_wrap(~study, ncol = 1, scales = "free_y", space = "free_y") +
  ggplot2::scale_colour_manual(values = measure_colours, labels = measure_labels, name = NULL) +
  ggplot2::scale_x_continuous(labels = scales::label_percent(), limits = c(0, 0.95), expand = c(0, 0)) +
  ggplot2::labs(x = "Share of respondents misinformed (or answering incorrectly)", y = NULL) +
  theme_evidence() +
  ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0)) +
  ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2, reverse = TRUE)) +
  ggplot2::theme(legend.position = "top", legend.justification = "left")
save_evidence(format_plot, "figs/format", width = 6.5, height = 6.2)

congenial_labels <- c(
  R = "error congenial to Republicans", D = "error congenial to Democrats", none = "congenial to neither"
)
gaps <- read_tab("partisan_gaps.csv") |>
  dplyr::filter(measure %in% c("mc_media", "mc", "scale_strict")) |>
  dplyr::mutate(
    label = paste0(short, dplyr::if_else(truth, " (T)", " (F)")),
    group = factor(congenial_labels[congenial], levels = congenial_labels),
    measure = factor(measure, levels = names(measure_labels)),
    panel = factor(
      paste0(study_labels[study], ": ", congenial_labels[congenial]),
      levels = as.vector(outer(study_labels[c("mturk_march", "mturk_july")], congenial_labels, paste, sep = ": "))
    ) |>
      forcats::fct_drop()
  ) |>
  dplyr::group_by(label) |>
  dplyr::mutate(order = estimate[measure == "scale_strict"][1]) |>
  dplyr::ungroup() |>
  dplyr::mutate(label = forcats::fct_reorder(label, order))

gap_plot <- ggplot2::ggplot(gaps, ggplot2::aes(estimate, label, colour = measure)) +
  geom_zero() +
  geom_estimate(position = ggplot2::position_dodge(width = 0.6)) +
  ggplot2::facet_wrap(~panel, ncol = 1, scales = "free_y", space = "free_y") +
  ggplot2::scale_colour_manual(values = measure_colours, labels = measure_labels, name = NULL) +
  ggplot2::scale_x_continuous(breaks = seq(-0.25, 0.75, 0.25), labels = \(x) sprintf("%+d", round(100 * x))) +
  ggplot2::labs(x = "Republican minus Democratic share (percentage points)", y = NULL) +
  theme_evidence() +
  ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0)) +
  ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2, reverse = TRUE)) +
  ggplot2::theme(legend.position = "top", legend.justification = "left")
save_evidence(gap_plot, "figs/partisan_gaps", width = 6.5, height = 7.6)
