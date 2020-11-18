library(mockery)
library(testthat)

test_that("it calls new_spc_i", {
  m <- mock()

  stub(spc_i, "new_spc_i", m)

  spc_i(y = 1:10)

  expect_called(m, 1)
  expect_call(m, 1, new_spc_i(x, y, centre, limits, exclude, freeze))
})

test_that("it returns an spc_i/spc object", {
  # testing with an integer
  a <- spc_i(y = 1:10)
  # testing with a double
  b <- spc_i(y = rnorm(10))

  expect_is(a, "spc")
  expect_is(a, "spc_i")

  expect_is(b, "spc")
  expect_is(b, "spc_i")
})

# Start Paramaters ----
# x ====

test_that("x must be a numeric", {
  expect_error(spc_i(x = letters[1:10], y = 1:10))
})

test_that("x must be the same length as y", {
  expect_error(spc_i(x = 1, y = 1:10))

  a <- spc_i(x = 1:10, y = 1:10)
  expect_is(a, "spc")
})

test_that("x defaults to be a sequence of numbers", {
  m <- mock()

  set.seed(123)
  y <- rnorm(10)

  stub(spc_i, "new_spc_i", m)

  spc_i(y = y)

  expect_called(m, 1)
  expect_equal(mock_args(m)[[1]][[1]], seq_along(y))
})

test_that("x cannot contain NA values", {
  expect_error(spc_i(x = c(1:4, NA, 6:10), y = 1:10))
})

# y ====

test_that("y must be a numeric", {
  expect_error(spc_i(y = "a"))
  expect_error(spc_i(y = Sys.Date()))
  expect_error(spc_i(y = list()))
})

test_that("y cannot contain NA values", {
  expect_error(spc_i(y = c(1:4, NA, 6:10)))
})

test_that("y must have length > 0", {
  expect_error(spc_i(), "`y` must have at least 1 value")
})

# centre ====

test_that("centre must be a single numeric", {
  expect_error(spc_i(y = 1:10, centre = 1:10))

  a <- spc_i(y = 1:10, centre = 5)
  expect_is(a, "spc")
})

test_that("centre defaults to the mean of y", {
  set.seed(123)
  y <- rnorm(10)

  expected <- mean(y, na.rm = TRUE)

  actual <- vctrs::field(spc_i(y = y), "centre")[[1]]

  expect_equal(actual, expected)
})

test_that("centre defaults to the mean of y with exclusions", {
  set.seed(123)
  y <- rnorm(10)
  e <- sample(c(TRUE, FALSE), size = length(y), replace = TRUE, prob = c(1, 9))

  expected <- mean(y[!e], na.rm = TRUE)

  actual <- vctrs::field(spc_i(y = y, exclude = e), "centre")[[1]]

  expect_equal(actual, expected)
})

test_that("centre cannot be NA", {
  expect_error(spc_i(y = 1:10, centre = NA))
})

# limits ====

test_that("limits must be a single numeric", {
  expect_error(spc_i(y = 1:10, limits = "a"))
  expect_error(spc_i(y = 1:10, limits = 1:10))

  a <- spc_i(y = 1:10, limits = 2)
  expect_equal(attr(a, "limits"), 2)
})

test_that("limits defaults to 3", {
  m <- mock()

  set.seed(123)
  y <- rnorm(10)

  stub(spc_i, "new_spc_i", m)

  spc_i(y = y)

  expect_called(m, 1)
  expect_equal(mock_args(m)[[1]][[4]], 3)
})

test_that("limits must be > 0", {
  expect_error(spc_i(y = 1:10, limits = 0))
  expect_error(spc_i(y = 1:10, limits = -1))
})

test_that("limits cannot be NA", {
  expect_error(spc_i(y = 1:10, limits = NA))
})

# exclude ====

test_that("exclude must be a logical", {
  expect_error(spc_i(y = 1:10, exclude = 1:10))
})

test_that("exclude must be the same length as y", {
  expect_error(spc_i(y = 1:10, exclude = FALSE))

  a <- spc_i(y = 1:10, exclude = rep(c(TRUE, FALSE), each = 5))
  expect_is(a, "spc")
})

test_that("exclude defaults to a vector of FALSE's", {
  m <- mock()

  set.seed(123)
  y = rnorm(10)
  stub(spc_i, "new_spc_i", m)

  spc_i(y = y)

  expect_called(m, 1)
  expect_equal(mock_args(m)[[1]][[5]], rep(FALSE, length(y)))
})

test_that("exclude cannot contain NA values", {
  expect_error(spc_i(y = 1:10, exclude = c(rep(FALSE, 9), NA)))
})

# End Parameters ----
