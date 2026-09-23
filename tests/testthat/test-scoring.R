root <- "../.."
purrr::walk(list.files(file.path(root, "R"), full.names = TRUE), source)

test_that("belief_state() scores true and false statements symmetrically", {
  expect_equal(as.character(belief_state(c(10, 0, 5, 7, 3), rep(TRUE, 5))), c(
    "knowledge", "misinformation", "midpoint", "lean correct", "lean incorrect"
  ))
  expect_equal(as.character(belief_state(c(10, 0), c(FALSE, FALSE))), c("misinformation", "knowledge"))
  expect_equal(as.character(belief_state(c(9, 1), c(TRUE, TRUE), lenient = TRUE)), c("knowledge", "misinformation"))
  expect_true(is.na(belief_state(NA_real_, TRUE)))
})

test_that("parse_rating() keeps 0-10, rescales 0-100 answers, and drops the rest", {
  expect_equal(parse_rating(c("0", "10", "50", "30", "11", "", "abc")), c(0, 10, 5, 3, NA, NA, NA))
})

test_that("classify_mc() refuses answers it cannot place", {
  options <- tibble::tibble(pattern = c("don t know", "^yes$", "^no$"), correct = c(NA, TRUE, FALSE))
  expect_equal(
    classify_mc(c("Yes", "No", "Don't know", NA), options),
    c("correct", "incorrect", "don't know", "no answer")
  )
  expect_error(classify_mc("Maybe", options), "Unmatched")
})

test_that("the released survey files carry no direct identifiers", {
  files <- list.files(file.path(root, "data", "raw"), pattern = "\\.csv$", full.names = TRUE)
  for (f in files) {
    cells <- readr::read_csv(f, col_types = readr::cols(.default = "c")) |>
      # Browser version strings (e.g. 37.0.0.0) look like IP addresses.
      dplyr::select(-dplyr::matches("Version$")) |>
      unlist(use.names = FALSE) |>
      stats::na.omit()
    has <- \(pattern) any(stringr::str_detect(cells, pattern))
    expect_false(has("(?<![\\d.])(?:\\d{1,3}\\.){3}\\d{1,3}(?![\\d.])"), label = paste(basename(f), "IP address"))
    expect_false(has("[\\w.+-]+@[\\w-]+\\.[A-Za-z]{2,}"), label = paste(basename(f), "email"))
    expect_false(has("FY1[0-8]\\d{4}|srep[0-4]\\d{3}"), label = paste(basename(f), "panel ID"))
  }
  blanked <- c(
    "ip.address", "lat", "long", "city", "postal", "postal_code", "IPAddress",
    "LocationLatitude", "LocationLongitude", "other.race2"
  )
  for (f in files) {
    data <- readr::read_csv(f, col_types = readr::cols(.default = "c"))
    if (grepl("mturk", f)) data <- data[-(1:2), ]
    for (column in intersect(blanked, names(data))) {
      expect_true(all(is.na(data[[column]])), label = paste(basename(f), column, "is blank"))
    }
  }
})
