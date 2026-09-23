raw_path <- \(name) file.path("data", "raw", name)

raw_files <- c(
  alumni_2010.csv = "7baf68c339ab36d99eac7ff018c6c4e1179b13443dd8450f1d0d1c6b13ab6c82",
  staff_2010.csv = "891f04e9cfe768d0b666a3133a23da05478b90c4a6653285fe197d263e7fb5c2",
  mturk_march_2017.csv = "845864d3861868f27a781aa3ed5875e77b5d8587f216eff22930416521808f02",
  mturk_july_2017.csv = "62aa2380baf0fcb4ca7aeb64d552de597c2927d61289d27146072e70f329fa67"
)

verify_sources <- function() {
  found <- purrr::map_chr(names(raw_files), \(f) digest::digest(file = raw_path(f), algo = "sha256"))
  bad <- names(raw_files)[found != raw_files]
  if (length(bad) > 0) stop("Hash mismatch: ", paste(bad, collapse = ", "))
  invisible(TRUE)
}

normalize_text <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", " ") |>
    stringr::str_squish()
}

# Ratings were typed into boxes in 2010. Two answers (30, 50) read as the
# 0-100 scale used elsewhere in the questionnaire and are divided by 10; the
# one other out-of-range answer (11) is set to missing.
parse_rating <- function(x) {
  value <- suppressWarnings(as.numeric(x))
  value <- dplyr::if_else(value %in% seq(20, 100, 10), value / 10, value)
  dplyr::if_else(value %in% 0:10, value, NA_real_)
}

partisanship <- function(party, lean) {
  dplyr::case_when(
    party == "Democrat" ~ "Democrat",
    party == "Republican" ~ "Republican",
    lean == "Democrat" ~ "Democrat",
    lean == "Republican" ~ "Republican",
    !is.na(party) ~ "Independent",
    .default = NA_character_
  )
}

rating_columns_2010 <- c(
  "illegal", "single.payer", "upper.class", "mammograms",
  "death.panel", "medicare", "future.increase", "cuts.benefits"
)

# Alumni and staff answered the same instrument. Respondents who opened the
# survey more than once keep their first complete attempt.
read_panel_2010 <- function(file, id_marker, study) {
  readr::read_csv(raw_path(file), col_types = readr::cols(.default = "c")) |>
    dplyr::filter(stringr::str_detect(userid, id_marker)) |>
    dplyr::mutate(started = lubridate::mdy_hm(date)) |>
    dplyr::arrange(userid, status != "Complete", started) |>
    dplyr::distinct(userid, .keep_all = TRUE) |>
    dplyr::filter(correct.or.conf %in% c("percent", "regular")) |>
    dplyr::transmute(
      study = study,
      respondent = userid,
      arm = dplyr::if_else(correct.or.conf == "percent", "scale", "mc"),
      party = partisanship(
        pid1,
        dplyr::case_when(
          stringr::str_detect(pid2.other, "Democratic") ~ "Democrat",
          stringr::str_detect(pid2.other, "Republican") ~ "Republican"
        )
      ),
      interest = parse_rating(pol.interest),
      mc_aca1 = healthcare.pk1,
      mc_aca2 = healthbill,
      dplyr::across(dplyr::all_of(rating_columns_2010), parse_rating)
    ) |>
    # Partial completes count only if they reached the health care items.
    dplyr::filter(
      (arm == "mc" & (!is.na(mc_aca1) | !is.na(mc_aca2))) |
        (arm == "scale" & dplyr::if_any(dplyr::all_of(rating_columns_2010), \(x) !is.na(x)))
    ) |>
    assertr::assert(assertr::is_uniq, respondent) |>
    assertr::assert(assertr::in_set("scale", "mc"), arm)
}

read_alumni <- \() read_panel_2010("alumni_2010.csv", "FY", "alumni")
read_staff <- \() read_panel_2010("staff_2010.csv", "srep", "staff")

# Qualtrics exports carry two extra header rows under the column names. Both
# studies sampled U.S. workers; the few who consented from abroad (by IP
# country) were not assigned to an arm in March and are dropped in July too.
read_qualtrics <- function(file) {
  path <- raw_path(file)
  names <- names(readr::read_csv(path, n_max = 0, show_col_types = FALSE))
  readr::read_csv(path, skip = 3, col_names = names, col_types = readr::cols(.default = "c")) |>
    dplyr::filter(Finished == "True", consent == "I agree", ccode == "US")
}

mturk_party <- function(pid, lean) {
  lean <- suppressWarnings(as.numeric(lean))
  partisanship(
    pid,
    dplyr::case_when(lean < 5 ~ "Democrat", lean > 5 ~ "Republican")
  )
}

read_mturk_march <- function() {
  raw <- read_qualtrics("mturk_march_2017.csv")
  pick_mc <- \(open, closed) dplyr::coalesce(raw[[open]], raw[[closed]])
  scale_cols <- grep("^rg_s_(aca|aca2|gg|gg2|dt)_[1-4]$", names(raw), value = TRUE)
  raw |>
    dplyr::mutate(
      study = "mturk_march",
      respondent = ResponseId,
      arm = dplyr::recode(reticence_guessing, closed = "mc", scale = "scale"),
      party = mturk_party(pid, pid_strength_1),
      interest = parse_rating(interest_1),
      mc_aca1 = pick_mc("rgc_o_aca", "rgc_c_aca"),
      mc_aca2 = pick_mc("rgc_o_aca2", "rgc_c_aca2"),
      dplyr::across(dplyr::all_of(scale_cols), parse_rating)
    ) |>
    dplyr::filter(!is.na(arm)) |>
    dplyr::select(study, respondent, arm, party, interest, mc_aca1, mc_aca2, dplyr::all_of(scale_cols)) |>
    assertr::assert(assertr::is_uniq, respondent)
}

july_items <- c("birth", "religion", "illegal", "death", "increase", "science", "fraud", "mmr", "deficit")

# The 14k arm is multiple choice with an explicit DK option, the 24k arm the
# 0-10 scale, and the RW arm the wording of widely reported media polls with no
# DK option. All three are kept; the FSR and IPS arms belong to other papers.
read_mturk_july <- function() {
  raw <- read_qualtrics("mturk_july_2017.csv") |>
    dplyr::filter(!is.na(mTurkCode), question_type %in% c("14k", "24k", "RW"))
  scale_cols <- grep("^24k_block[12]_[1-6]$", names(raw), value = TRUE)
  mc <- purrr::map(july_items, \(item) {
    dplyr::coalesce(raw[[paste0("14k_", item)]], raw[[paste0("rw_", item)]])
  }) |>
    rlang::set_names(paste0("mc_", july_items)) |>
    tibble::as_tibble()
  raw |>
    dplyr::transmute(
      study = "mturk_july",
      respondent = ResponseId,
      arm = dplyr::recode(question_type, `14k` = "mc", `24k` = "scale", RW = "mc_media"),
      party = mturk_party(pid, pid_strength_1),
      interest = parse_rating(interest_1),
      dplyr::across(dplyr::all_of(scale_cols), parse_rating)
    ) |>
    dplyr::bind_cols(mc) |>
    assertr::assert(assertr::is_uniq, respondent)
}

read_strict_csv <- function(path) {
  data <- readr::read_csv(path, show_col_types = FALSE)
  if (nrow(readr::problems(data)) > 0) stop("Parsing problems in ", path)
  data
}

read_propositions <- function() {
  read_strict_csv("docs/propositions.csv") |>
    tidyr::separate_longer_delim(study, ";") |>
    assertr::assert(assertr::in_set("R", "D", "none"), congenial) |>
    assertr::verify(!keep | !is.na(truth))
}

read_mc_options <- function() {
  read_strict_csv("docs/mc_options.csv") |>
    tidyr::separate_longer_delim(study, ";")
}
