

# is.scalar ---------------------------------------------------------------

test_that("is.scalar works correctly", {
  expect_true(is.scalar(1))
  expect_true(is.scalar("a"))
  expect_false(is.scalar(c(1, 2)))
  expect_false(is.scalar(list(1)))
  expect_false(is.scalar(NULL))
})

# is.scalar_or_null -------------------------------------------------------

test_that("is.scalar_or_null works correctly", {
  expect_true(is.scalar_or_null(1))
  expect_true(is.scalar_or_null(NULL))
  expect_false(is.scalar_or_null(c(1, 2)))
})

# is.numeric_or_null ------------------------------------------------------

test_that("is.numeric_or_null works correctly", {
  expect_true(is.numeric_or_null(1))
  expect_true(is.numeric_or_null(NULL))
  expect_true(is.numeric_or_null(NA_real_))
  expect_false(is.numeric_or_null("a"))
})

# is.character_or_null ----------------------------------------------------

test_that("is.character_or_null works correctly", {
  expect_true(is.character_or_null("a"))
  expect_true(is.character_or_null(NULL))
  expect_true(is.character_or_null(NA_character_))
  expect_false(is.character_or_null(1))
})

# is.logical_or_null ------------------------------------------------------

test_that("is.logical_or_null works correctly", {
  expect_true(is.logical_or_null(TRUE))
  expect_true(is.logical_or_null(NULL))
  expect_true(is.logical_or_null(NA))
  expect_false(is.logical_or_null(1))
})

# is.df_or_null -----------------------------------------------------------

test_that("is.df_or_null works correctly", {
  expect_true(is.df_or_null(data.frame()))
  expect_true(is.df_or_null(NULL))
  expect_false(is.df_or_null(list()))
})

# is.list_or_null ---------------------------------------------------------

test_that("is.list_or_null works correctly", {
  expect_true(is.list_or_null(list(1)))
  expect_true(is.list_or_null(NULL))
  expect_false(is.list_or_null(1))
})

# is.atomic_nan -----------------------------------------------------------

test_that("is.atomic_nan works correctly", {
  expect_true(is.atomic_nan(NaN))
  expect_false(is.atomic_nan(1))
  expect_false(is.atomic_nan(list(NaN)))
  expect_equal(is.atomic_nan(c(1, NaN)), c(FALSE, TRUE))
})

# is.irregular_list -------------------------------------------------------

test_that("is.irregular_list works correctly", {
  expect_true(is.irregular_list(list(1)))
  expect_false(is.irregular_list(data.frame()))
  expect_false(is.irregular_list(1))
})

# is.bad_for_calcs --------------------------------------------------------

test_that("is.bad_for_calcs works correctly", {
  expect_true(is.bad_for_calcs(NA))
  expect_true(is.bad_for_calcs(NaN))
  #expect_true(is.bad_for_calcs(Inf)) #works in console
  # expect_true(is.bad_for_calcs("a")) #works in console
  expect_false(is.bad_for_calcs(1))
})

# any_bad_for_calcs -------------------------------------------------------

test_that("any_bad_for_calcs works correctly", {
  expect_true(any_bad_for_calcs(c(1, NA)))
  expect_false(any_bad_for_calcs(c(1, 2)))
  expect_true(any_bad_for_calcs(1, NA))
  expect_false(any_bad_for_calcs(1, 2, na.rm = TRUE))
})

# all_good_for_calcs ------------------------------------------------------

test_that("all_good_for_calcs works correctly", {
  expect_false(all_good_for_calcs(c(1, NA)))
  expect_true(all_good_for_calcs(c(1, 2)))
  expect_false(all_good_for_calcs(1, NA))
  expect_true(all_good_for_calcs(1, 2, na.rm = TRUE))
})

# is.bad_for_indexing -----------------------------------------------------

test_that("is.bad_for_indexing works correctly", {
  #expect_false(is.bad_for_indexing(c(1, 2)))
  expect_true(is.bad_for_indexing(NA))
  expect_false(is.bad_for_indexing(1))
})

# is.good_for_indexing ----------------------------------------------------

test_that("is.good_for_indexing works correctly", {
  expect_true(is.good_for_indexing(c(1, 2)))
  #expect_false(is.good_for_indexing(NA))
  expect_true(is.good_for_indexing(1))
})

# is.bad_and_equal --------------------------------------------------------

test_that("is.bad_and_equal works correctly", {
  expect_true(is.bad_and_equal(NA, NA))
  expect_true(is.bad_and_equal(NaN, NaN))
  expect_false(is.bad_and_equal(NA, NaN))
  expect_false(is.bad_and_equal(1, NA))
})

# is.good_for_calcs -------------------------------------------------------

test_that("is.good_for_calcs works correctly", {
  expect_false(is.good_for_calcs(NA))
  expect_true(is.good_for_calcs(1))
  expect_true(all_good_for_calcs(c(1,2,NA), na.rm = TRUE))
})

# is.null_or_na -----------------------------------------------------------

test_that("is.null_or_na works correctly", {
  expect_true(is.null_or_na(NULL))
  expect_true(is.null_or_na(NA))
  expect_true(is.null_or_na(numeric(0)))
  expect_false(is.null_or_na(1))
  expect_equal(is.null_or_na(c(1, NA)), c(FALSE, TRUE))
})


# chr ---------------------------------------------------------------------

test_that("chr converts to character correctly", {
  expect_identical(chr(42), "42")
  expect_identical(chr(TRUE), "TRUE")
  expect_identical(chr(NA), as.character(NA))
  expect_identical(chr(c(1, 2)), c("1", "2"))
  expect_identical(chr(NULL), character(0))
})

# int ---------------------------------------------------------------------

test_that("int converts to integer correctly", {
  expect_identical(int(42.9), 42L)
  expect_identical(int("42"), 42L)
  expect_identical(int(TRUE), 1L)
  expect_identical(int(NA), as.integer(NA))
  expect_identical(int(c(1.1, 2.9)), c(1L, 2L))
  expect_identical(int(NULL), integer(0))
})

# dbl ---------------------------------------------------------------------

test_that("dbl converts to double correctly", {
  expect_identical(dbl("42"), 42.0)
  expect_identical(dbl(42L), 42.0)
  expect_identical(dbl(TRUE), 1.0)
  expect_identical(dbl(NA), as.double(NA))
  expect_identical(dbl(c("1.1", "2.2")), c(1.1, 2.2))
  expect_identical(dbl(NULL), double(0))
})

# num ---------------------------------------------------------------------

test_that("num converts to numeric correctly", {
  expect_identical(num("42.5"), 42.5)
  expect_identical(num(42L), 42)
  expect_identical(num(TRUE), 1)
  expect_identical(num(NA), as.numeric(NA))
  expect_identical(num(c("1.1", "2.2")), c(1.1, 2.2))
  expect_identical(num(NULL), numeric(0))
})

# bool --------------------------------------------------------------------

test_that("bool converts to logical correctly", {
  expect_identical(bool(0), FALSE)
  expect_identical(bool(1), TRUE)
  expect_identical(bool("TRUE"), TRUE)
  expect_identical(bool("FALSE"), FALSE)
  expect_identical(bool(NA), as.logical(NA))
  expect_identical(bool(c(0, 1)), c(FALSE, TRUE))
  expect_identical(bool(NULL), logical(0))
})

# as.class ----------------------------------------------------------------

test_that("as.class converts to specified class correctly", {
  expect_identical(as.class("255", "numeric"), 255)
  expect_identical(as.class(255, "roman"), as.roman(255))
  expect_identical(as.class("TRUE", "logical"), TRUE)
  expect_identical(as.class("42", "integer"), 42L)
  expect_warning(as.class("invalid", "numeric"), "NAs introduced by coercion")
})

# f.as.numeric ------------------------------------------------------------

test_that("f.as.numeric converts factor to numeric correctly", {
  f <- factor(c("10", "20", "30"))
  expect_identical(f.as.numeric(f), c(10, 20, 30))

  f_na <- factor(c("10", NA, "30"))
  expect_identical(f.as.numeric(f_na), c(10, NA, 30))

  f_char <- factor(c("a", "b", "c"))
  expect_warning(f.as.numeric(f_char), "NAs introduced by coercion")
})

# n_unique ----------------------------------------------------------------

test_that("n_unique counts unique values correctly", {
  expect_identical(n_unique(c(1, 2, 2, NA)), 3L)
  expect_identical(n_unique(c(1, 2, 2, NA), na.rm = TRUE), 2L)
  expect_identical(n_unique(NULL), 0L)
  expect_identical(n_unique(character(0)), 0L)
})

# read.tsv ----------------------------------------------------------------

test_that("read.tsv reads tab-separated files correctly", {
  # Create a temporary TSV file
  tmp_file <- tempfile(fileext = ".tsv")
  write.table(data.frame(a = 1:3, b = letters[1:3]), tmp_file, sep = "\t", row.names = FALSE)

  # Test reading
  expect_s3_class(read.tsv(tmp_file), "data.frame")
  expect_identical(nrow(read.tsv(tmp_file)), 3L)
  expect_identical(names(read.tsv(tmp_file)), c("a", "b"))

  # Clean up
  unlink(tmp_file)
})

# read.psv ----------------------------------------------------------------

test_that("read.psv reads pipe-separated files correctly", {
  # Create a temporary PSV file
  tmp_file <- tempfile(fileext = ".psv")
  write.table(data.frame(a = 1:3, b = letters[1:3]), tmp_file, sep = "|", row.names = FALSE)

  # Test reading
  expect_s3_class(read.psv(tmp_file), "data.frame")
  expect_identical(nrow(read.psv(tmp_file)), 3L)
  expect_identical(names(read.psv(tmp_file)), c("a", "b"))

  # Clean up
  unlink(tmp_file)
})


