purrr::walk(list.files("R", full.names = TRUE), source)

balance_tab <- read_tab("balance.csv")
comparison <- read_tab("format_comparison.csv")
pooled <- read_tab("format_pooled.csv")
states <- read_tab("belief_states.csv")
prev <- read_tab("prevalence.csv")
inv <- read_tab("invention_denial.csv")
inv_test <- read_tab("invention_denial_test.csv")
gaps <- read_tab("partisan_gaps.csv")
interest <- read_tab("interest_gradient.csv")
propositions <- read_propositions()

n_arm <- \(s, a) balance_tab$respondents[balance_tab$study == s & balance_tab$arm == a]
share <- \(s, q, m) comparison$estimate[comparison$study == s & comparison$question == q & comparison$measure == m]
pooled_row <- \(s, m) pooled[pooled$study == s & pooled$measure == m, ]
gap <- \(q, m) gaps$estimate[gaps$study == "mturk_july" & gaps$question == q & gaps$measure == m]
inv_share <- \(s, k) inv$strict[inv$study == s & inv$kind == k]
inv_diff <- \(s) inv_test[inv_test$study == s & inv_test$term == "kindinvention", ]
grad <- \(o) interest[interest$outcome == o, ]
rated <- states |>
  dplyr::group_by(strict) |>
  dplyr::summarise(count = sum(count))
state_share <- \(s) rated$count[rated$strict == s] / sum(rated$count)
july_skip <- prev |> dplyr::filter(study == "mturk_july", scoring == "strict")

values <- c(
  nAlumniMc = n_arm("alumni", "mc"), nAlumniScale = n_arm("alumni", "scale"),
  nStaffMc = n_arm("staff", "mc"), nStaffScale = n_arm("staff", "scale"),
  nMarchMc = count(n_arm("mturk_march", "mc")), nMarchScale = n_arm("mturk_march", "scale"),
  nJulyMc = n_arm("mturk_july", "mc"), nJulyMedia = n_arm("mturk_july", "mc_media"),
  nJulyScale = n_arm("mturk_july", "scale"),
  nTotal = count(sum(balance_tab$respondents)),
  staffRepMc = pct(balance_tab$republican[balance_tab$study == "staff" & balance_tab$arm == "mc"]),
  staffRepScale = pct(balance_tab$republican[balance_tab$study == "staff" & balance_tab$arm == "scale"]),
  nPropositions = dplyr::n_distinct(propositions$prop_id[propositions$keep]),
  nDropped = dplyr::n_distinct(propositions$prop_id[!propositions$keep]),
  nMarchPropositions = sum(propositions$study == "mturk_march"),
  shareKnowledge = pct(state_share("knowledge")),
  shareMisinformation = pct(state_share("misinformation")),
  shareMiddle = pct(1 - state_share("knowledge") - state_share("misinformation")),
  shareMidpoint = pct(state_share("midpoint")),
  julySkipMin = pct(min(july_skip$skipped)), julySkipMax = pct(max(july_skip$skipped)),
  muslimMedia = pct(share("mturk_july", "religion", "mc_media")),
  muslimMc = pct(share("mturk_july", "religion", "mc")),
  muslimScale = pct(share("mturk_july", "religion", "scale_strict")),
  illegalMedia = pct(share("mturk_july", "illegal", "mc_media")),
  illegalMc = pct(share("mturk_july", "illegal", "mc")),
  illegalScale = pct(share("mturk_july", "illegal", "scale_strict")),
  fraudMedia = pct(share("mturk_july", "fraud", "mc_media")),
  fraudMc = pct(share("mturk_july", "fraud", "mc")),
  fraudScale = pct(share("mturk_july", "fraud", "scale_strict")),
  deficitMedia = pct(share("mturk_july", "deficit", "mc_media")),
  deficitMc = pct(share("mturk_july", "deficit", "mc")),
  deficitScale = pct(share("mturk_july", "deficit", "scale_strict")),
  pooledStrict = pts(-pooled_row("pooled", "misinformed_strict")$estimate),
  pooledStrictLower = pts(-pooled_row("pooled", "misinformed_strict")$upper),
  pooledStrictUpper = pts(-pooled_row("pooled", "misinformed_strict")$lower),
  pooledLenient = pts(-pooled_row("pooled", "misinformed_lenient")$estimate),
  julyStrict = pts(-pooled_row("mturk_july", "misinformed_strict")$estimate),
  julyStrictLower = pts(-pooled_row("mturk_july", "misinformed_strict")$upper),
  julyStrictUpper = pts(-pooled_row("mturk_july", "misinformed_strict")$lower),
  marchStrict = pts(-pooled_row("mturk_march", "misinformed_strict")$estimate),
  marchLenient = pts(pooled_row("mturk_march", "misinformed_lenient")$estimate),
  alumniStrict = pts(pooled_row("alumni", "misinformed_strict")$estimate),
  staffStrict = pts(-pooled_row("staff", "misinformed_strict")$estimate),
  denialAlumni = pct(inv_share("alumni", "denial")), inventionAlumni = pct(inv_share("alumni", "invention")),
  denialStaff = pct(inv_share("staff", "denial")), inventionStaff = pct(inv_share("staff", "invention")),
  denialMarch = pct(inv_share("mturk_march", "denial")), inventionMarch = pct(inv_share("mturk_march", "invention")),
  denialJuly = pct(inv_share("mturk_july", "denial")), inventionJuly = pct(inv_share("mturk_july", "invention")),
  inventionMinusDenialJuly = pts(inv_diff("mturk_july")$estimate),
  inventionMinusDenialJulySe = pts(inv_diff("mturk_july")$std_error),
  gapMuslimMedia = pts(gap("religion", "mc_media")), gapMuslimMc = pts(gap("religion", "mc")),
  gapMuslimScale = pts(gap("religion", "scale_strict")),
  gapFraudMedia = pts(gap("fraud", "mc_media")), gapFraudMc = pts(gap("fraud", "mc")),
  gapFraudScale = pts(gap("fraud", "scale_strict")),
  gapWarmingMc = pts(gap("increase", "mc")), gapWarmingScale = pts(gap("increase", "scale_strict")),
  gapScienceMc = pts(gap("science", "mc")), gapScienceScale = pts(gap("science", "scale_strict")),
  gapDeficitScale = pts(gap("deficit", "scale_strict")),
  interestKnows = pts(grad("knows_strict")$estimate),
  interestKnowsSe = pts(grad("knows_strict")$std_error),
  interestMisinformed = pts(grad("misinformed_strict")$estimate),
  interestMisinformedLower = pts(grad("misinformed_strict")$lower),
  interestMisinformedUpper = pts(grad("misinformed_strict")$upper),
  meanKnows = pct(grad("knows_strict")$mean), meanMisinformed = pct(grad("misinformed_strict")$mean)
)
strict_gaps <- dplyr::filter(gaps, measure == "scale_strict")
r_gaps <- dplyr::filter(strict_gaps, congenial == "R")
d_gaps <- dplyr::filter(strict_gaps, congenial == "D")
words <- c("no", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
as_word <- \(n) if (n < length(words)) words[n + 1] else as.character(n)
values <- c(
  values,
  nRcongenial = nrow(r_gaps), nRpositive = sum(r_gaps$estimate > 0), nRclear = sum(r_gaps$lower > 0),
  nDcongenial = as_word(nrow(d_gaps)), nDnegative = sum(d_gaps$estimate < 0)
)
if (sum(d_gaps$estimate < 0) != nrow(d_gaps)) stop("Text says every Democrat-congenial gap is negative")
write_macros(values, "tabs/macros.tex")

state_cols <- c("knowledge", "lean correct", "midpoint", "lean incorrect", "misinformation")
states |>
  dplyr::left_join(dplyr::distinct(propositions, study, prop_id, short), by = c("study", "prop_id")) |>
  dplyr::mutate(share = formatC(100 * share, format = "f", digits = 0)) |>
  dplyr::select(study, short, truth, rated, strict, share) |>
  tidyr::pivot_wider(names_from = strict, values_from = share) |>
  dplyr::mutate(
    study = factor(study_labels[study], levels = study_labels),
    truth = dplyr::if_else(truth, "T", "F")
  ) |>
  dplyr::arrange(study, truth, short) |>
  dplyr::mutate(study = dplyr::if_else(duplicated(study), "", as.character(study))) |>
  dplyr::select(study, short, truth, rated, dplyr::all_of(state_cols)) |>
  dplyr::mutate(short = latex_escape(short)) |>
  write_table(
    "tabs/belief_states.tex", "llcrrrrrr",
    c("Study", "Proposition", "", "$n$", "Know", "Lean right", "5", "Lean wrong", "Misinf.")
  )

propositions |>
  dplyr::distinct(prop_id, .keep_all = TRUE) |>
  dplyr::mutate(
    key = dplyr::case_when(!keep ~ "dropped", truth ~ "true", .default = "false"),
    basis = latex_escape(dplyr::coalesce(truth_source, note)),
    text = latex_escape(text),
    study = dplyr::case_when(
      study == "alumni" ~ "2010", study == "mturk_march" ~ "Mar 2017",
      .default = "Jul 2017"
    )
  ) |>
  dplyr::select(study, text, key, basis) |>
  write_table(
    "tabs/propositions.tex", "lp{4.8cm}lp{7cm}",
    c("Survey", "Statement", "Key", "Basis"),
    caption = "Statements, keys, and the basis for each key",
    label = "tab:propositions"
  )

balance_tab |>
  dplyr::mutate(
    study = factor(study_labels[study], levels = study_labels),
    arm = dplyr::recode(arm, mc = "Multiple choice with DK", scale = "0--10 scale", mc_media = "Media-poll wording"),
    dplyr::across(c(democrat, republican), \(x) formatC(x, format = "f", digits = 2)),
    interest = formatC(interest, format = "f", digits = 1)
  ) |>
  dplyr::arrange(study) |>
  dplyr::mutate(study = dplyr::if_else(duplicated(study), "", as.character(study))) |>
  write_table(
    "tabs/balance.tex", "llrrrr",
    c("Study", "Arm", "$n$", "Democrat", "Republican", "Interest (0--10)")
  )
