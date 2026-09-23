root <- "../.."
read_output <- \(name) readr::read_csv(file.path(root, "tabs", name), show_col_types = FALSE)
expect_near <- \(actual, expected, tolerance) expect_lte(abs(actual - expected), tolerance)

test_that("March 2017 false-statement shares match the 2018 draft's Table 2", {
  # The draft computed these with different code; the rule for false
  # statements (share rating 10, or 9-10) was the same.
  prev <- read_output("prevalence.csv") |> dplyr::filter(study == "mturk_march", !truth)
  get <- \(id, scoring) prev$estimate[prev$prop_id == id & prev$scoring == scoring]
  draft <- tibble::tribble(
    ~prop_id, ~strict, ~lenient,
    "m_aca_illegal", .082, .106,
    "m_aca_single_payer", .060, .090,
    "m_aca_mammograms", .050, .100,
    "m_aca_death_panel", .042, .064,
    "m_aca_public_option", .046, .064,
    "m_aca_cuts_benefits", .042, .068,
    "m_gg_lung_cancer", .106, .151,
    "m_eo_terror_green_cards", .129, .191,
    "m_eo_muslim_green_cards", .153, .221
  )
  purrr::pwalk(draft, \(prop_id, strict, lenient) {
    expect_near(get(prop_id, "strict"), strict, 0.0005)
    expect_near(get(prop_id, "lenient"), lenient, 0.0005)
  })
})

test_that("July 2017 multiple-choice shares match the 2018 draft's Table 2", {
  mc <- read_output("mc_outcomes.csv") |> dplyr::filter(study == "mturk_july", arm == "mc")
  share_incorrect <- \(q) {
    d <- dplyr::filter(mc, question == q)
    sum(d$n[d$outcome == "incorrect"]) / sum(d$n)
  }
  expect_near(share_incorrect("birth"), .073, 0.0005)
  expect_near(share_incorrect("religion"), .175, 0.0005)
  expect_near(share_incorrect("mmr"), .081, 0.0005)
})

test_that("alumni answers to the Medicare question match the raw file", {
  mc <- read_output("mc_outcomes.csv") |> dplyr::filter(study == "alumni", question == "aca2")
  count_of <- \(o) sum(mc$n[mc$outcome == o])
  # 7 death panel + 13 public option + 15 cuts benefits; 69 chose the correct answer.
  expect_equal(count_of("incorrect"), 35)
  expect_equal(count_of("correct"), 69)
})

test_that("arms are the sizes the designs imply", {
  balance <- read_output("balance.csv")
  n <- \(s, a) balance$respondents[balance$study == s & balance$arm == a]
  expect_equal(n("mturk_march", "mc") + n("mturk_march", "scale"), 1059)
  expect_equal(n("mturk_july", "mc"), 246)
  expect_equal(n("mturk_july", "scale"), 267)
  expect_equal(n("mturk_july", "mc_media"), 237)
})

test_that("partisan gaps run in the direction the congeniality coding predicts", {
  gaps <- read_output("partisan_gaps.csv") |> dplyr::filter(measure == "scale_strict")
  expect_true(mean(gaps$estimate[gaps$congenial == "R"] > 0) > 0.75)
  expect_true(all(gaps$estimate[gaps$congenial == "D"] < 0))
})
