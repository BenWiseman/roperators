# Tests for the 1.4.0 "syntactic sugar" helpers in R/sugar.R

# f() ---------------------------------------------------------------------

test_that("f() interpolates expressions from the calling environment", {
  name <- "Ben"
  n    <- 2
  expect_identical(f("Hi {name}"), "Hi Ben")
  expect_identical(f("{n} + {n} = {n + n}"), "2 + 2 = 4")
  expect_identical(f("no placeholders here"), "no placeholders here")
})

test_that("f() collapses vector results and handles literal braces", {
  expect_identical(f("{head(LETTERS, 3)}"), "A, B, C")
  expect_identical(f("literal {{braces}}"), "literal {braces}")
})

test_that("f() is vectorised over its inputs", {
  expect_identical(f("a {1}", "b {2}"), c("a 1", "b 2"))
})

# %else% ------------------------------------------------------------------

test_that("%else% returns the LHS when it succeeds and the RHS on error", {
  expect_identical(sqrt(4) %else% NA_real_, 2)
  expect_identical(sqrt("a") %else% NA_real_, NA_real_)
  expect_identical((1:3)[[99]] %else% "oob", "oob")
  expect_identical(stop("boom") %else% "recovered", "recovered")
})

test_that("%else% does not evaluate the fallback unless needed", {
  evaluated <- FALSE
  side_effect <- function() {
    evaluated <<- TRUE
    "fallback"
  }
  expect_identical(1 + 1 %else% side_effect(), 2)
  expect_false(evaluated)
})

# %+-% --------------------------------------------------------------------

test_that("%+-% builds a tolerance interval that composes with %><%", {
  expect_identical(5 %+-% 0.5, c(4.5, 5.5))
  expect_true(4.9 %><% (5 %+-% 0.5))
  expect_false(6 %><% (5 %+-% 0.5))
})

# %/0% --------------------------------------------------------------------

test_that("%/0% guards against divide-by-zero", {
  expect_identical(10 %/0% 2, 5)
  expect_identical(10 %/0% 0, NA_real_)
  expect_identical(c(1, 2, 3) %/0% c(1, 0, 3), c(1, NA, 1))
  expect_identical(0 %/0% 0, NA_real_)
})

# %~% ---------------------------------------------------------------------

test_that("%~% is case- and whitespace-insensitive", {
  expect_true("Foo " %~% "foo")
  expect_true("a  b" %~% "a b")
  expect_false("foo" %~% "bar")
  expect_identical(c("Yes", "NO") %~% c("yes", "no"), c(TRUE, TRUE))
  expect_true(is.na(NA %~% "x"))
})

# as.percent() ------------------------------------------------------------

test_that("as.percent() formats proportions", {
  expect_identical(as.percent(0.75), "75.0%")
  expect_identical(as.percent(2 / 3, digits = 0), "67%")
  expect_identical(as.percent(c(0.1, 0.005)), c("10.0%", "0.5%"))
})
