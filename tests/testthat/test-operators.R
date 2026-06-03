# Tests for the core operators (R/operators.R)

# String arithmetic -------------------------------------------------------

test_that("string arithmetic operators work", {
  expect_identical("ab" %+% "c", "abc")
  expect_identical("abc" %-% "b", "ac")
  expect_identical(unname("ac" %s*% 2), "acac")
  expect_identical(unname("acac" %s/% "c"), 2L)
  # string division with a regular expression
  expect_identical(unname("a Steve b apple" %s/% "Steve|apple"), 2L)
})

# In-place assignment operators -------------------------------------------

test_that("numeric in-place modifiers reassign in place", {
  x <- 1;  x %+=% 2;   expect_identical(x, 3)
  x <- 3;  x %-=% 1;   expect_identical(x, 2)
  x <- 3;  x %*=% 2;   expect_identical(x, 6)
  x <- 6;  x %/=% 2;   expect_identical(x, 3)
  x <- 2;  x %^=% 3;   expect_identical(x, 8)
  x <- 100; x %log=% 10; expect_identical(x, 2)
  x <- 27; x %root=% 3; expect_equal(x, 3)
})

test_that("%+=% and %-=% work on strings", {
  x <- "a"; x %+=% "b"; expect_identical(x, "ab")
  x <- "foobar"; x %-=% "[fb]"; expect_identical(x, "ooar")
})

test_that("regex assignment operators", {
  x <- c("a1b", "b1", "c", "d0")
  x %regex=% c("\\d+", "x")
  expect_identical(x, c("axb", "bx", "c", "dx"))

  y <- c("a1b", "b1", "c", "d0")
  y %regex<-% c("\\d+", "x")
  expect_identical(y, c("x", "x", "c", "x"))
})

test_that("%na<-% replaces missing values flexibly", {
  x <- c("a", NA, "c"); x %na<-% "b";       expect_identical(x, c("a", "b", "c"))
  x <- c(1, NA, 3, NA); x %na<-% c(2, 4);   expect_identical(x, c(1, 2, 3, 4))
  x <- c(1, NA, 3, NA); x %na<-% 0;         expect_identical(x, c(1, 0, 3, 0))
})

# Comparison operators ----------------------------------------------------

test_that("%==% treats NA == NA as TRUE", {
  expect_identical(c(1, NA, 3) %==% c(1, NA, 4), c(TRUE, TRUE, FALSE))
  expect_false(1 %==% 2)
})

test_that("%===% requires matching class and value", {
  expect_false(int(2) %===% 2)
  expect_true(int(2) %===% int(2))
  expect_true(2 %===% 2)
})

test_that("%>=% and %<=% carry missing-value equality", {
  expect_identical(c(1, NA) %>=% c(1, NA), c(TRUE, TRUE))
  expect_true(2 %>=% 1)
  expect_false(1 %>=% 2)
  expect_true(1 %<=% 2)
})

test_that("between operators work and tolerate reversed bounds", {
  expect_true(2 %><% c(1, 3))
  expect_false(3 %><% c(1, 3))
  expect_true(3 %>=<% c(1, 3))
  # reversed bounds should behave the same
  expect_true(5 %><% c(10, 1))
  expect_true(1 %>=<% c(10, 1))
})

# Floating-point comparisons ----------------------------------------------

test_that("floating-point comparison operators", {
  expect_true((0.1 + 0.1 + 0.1) %~=% 0.3)
  expect_false(1 %~=% 2)
  expect_true((0.1 + 0.1 + 0.1) %>~% 0.3)
  expect_true((0.1 + 0.1 + 0.1) %<~% 0.3)
})

# Logical operators -------------------------------------------------------

test_that("logical operators", {
  expect_true("z" %ni% c("a", "b", "c"))
  expect_false("a" %ni% c("a", "b", "c"))
  expect_true(TRUE %xor% FALSE)
  expect_false(TRUE %xor% TRUE)
  expect_true(TRUE %aon% TRUE)
  expect_true(FALSE %aon% FALSE)
  expect_false(TRUE %aon% FALSE)
})

# SQL-style pattern matching ----------------------------------------------

test_that("%rlike% is case-insensitive, %perl% is case-sensitive", {
  expect_true("FOO" %rlike% "foo")
  expect_identical(c("foo", "dOe") %perl% "[a-z]O[a-z]", c(FALSE, TRUE))
})

# Maths operators ---------------------------------------------------------

test_that("choose, permute, and inline integration", {
  expect_identical(5 %C% 3, 10)
  expect_identical(5 %P% 3, 60)
  f_sq <- function(x) x^2
  expect_equal(f_sq %integrate% c(0, 1), 1 / 3, tolerance = 1e-6)
})
