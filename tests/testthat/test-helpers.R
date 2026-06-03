# Tests for the non-operator helper functions

# Paste & cat -------------------------------------------------------------

test_that("paste helpers", {
  expect_identical(paste_("a", "b", "c"), "a_b_c")
  expect_identical(paste_series("a", "b"), "a and b")
  expect_identical(paste_series("a", "b", "c"), "a, b, and c")
  expect_identical(paste_series("a", "b", "c", use_oxford_comma = FALSE),
                   "a, b and c")
  expect_identical(paste_oxford("a", "b", "c"), "a, b, and c")
})

test_that("cat helpers print without separators", {
  expect_identical(capture.output(cat0("a", "b", "c")), "abc")
  expect_identical(capture.output(catN("a", "b")), "ab")
})

# Complete-cases statistics ----------------------------------------------

test_that("complete-cases wrappers drop NAs by default", {
  expect_identical(mean_cc(c(1, 2, NA)), 1.5)
  expect_identical(sum_cc(c(1, 2, NA)), 3)
  expect_identical(min_cc(c(3, 1, NA)), 1)
  expect_identical(max_cc(c(3, 1, NA)), 3)
  expect_identical(length_cc(c(1, NA, 3)), 2L)
})

# Operating-system checks -------------------------------------------------

test_that("os checks return sensible types", {
  expect_true(get_os() %in% c("win", "mac", "linux", "unix"))
  expect_true(is.logical(is.os_mac()) && length(is.os_mac()) == 1)
  expect_match(get_R_version(), "^[0-9]+\\.[0-9]+")
})

# File-extension checks ---------------------------------------------------

test_that("file extension checks", {
  expect_true(is_csv_file("data.csv"))
  expect_false(is_csv_file("data.txt"))
  expect_true(is_txt_file("notes.txt"))
  expect_true(is_rda_file("x.rda"))
  expect_true(is_rds_file("x.RDS"))               # case-insensitive
  expect_true(check_ext_against("REPORT.DOC", c("doc")))
})

# Content checks ----------------------------------------------------------

test_that("content checks", {
  expect_true(is.constant(c(1, 1, 1)))
  expect_false(is.constant(c(1, 2)))
  expect_true(is.binary(c(1, 2, 1)))
  expect_false(is.binary(c(1, 2, 3)))
})

# First / last / nth / most-frequent --------------------------------------

test_that("element and word extraction", {
  expect_identical(get_1st(c(5, 6, 7)), 5)
  expect_identical(get_last(c(5, 6, 7)), 7)
  expect_identical(get_nth(c(5, 6, 7), 2), 6)

  expect_identical(get_1st_word("a b c"), "a")
  expect_identical(get_last_word("a b c"), "c")
  expect_identical(get_nth_word("a b c", 2), "b")

  expect_identical(get_most_frequent(c(1, 2, 2, 3)), 2)
  expect_identical(get_most_frequent_word("a a b"), "a")
})

# seq_around --------------------------------------------------------------

test_that("seq_around builds evenly spaced sequences", {
  expect_identical(seq_around(0, n = 3, spacing = 1), c(-1, 0, 1))
  expect_identical(seq_around(), 1)               # defaults no longer error
})
