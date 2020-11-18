#' SPC I
#'
#' Calculates limits for an SPC-I chart
#'
#' @param x numeric vector to order y by, defaults to an incrementing sequence (assuming the provided order of y is
#' correct)
#' @param y numeric vector that we want to calculate control limits for
#' @param centre the centre of y, defaults to the mean of y
#' @param limits the multiplier used to calculate limits, defaults to +/- 3
#' @param exclude a logical vector the same length of y, indicating points to exclude from control limit calculations
#' @param freeze a number indicating after how many points to freeze control limit calculations after
#'
#' @return an spc_i object
#' @export
#'
#' @examples
#' set.seed(123)
#' y <- rnorm(10)
#' spc_i(y = y)
#'
#' x <- sample(1:10)
#' spc_i(x, y)
#'
#' spc_i(x, y, centre = 3)
#'
#' spc_i(x, y, exclude = sample(c(TRUE, FALSE),
#'                              size = length(y),
#'                              prob = c(0.3, 0.7),
#'                              replace = TRUE))
spc_i <- function(x = seq_along(y),
                  y = numeric(),
                  centre = NULL,
                  limits = 3,
                  exclude = rep(FALSE, length(y)),
                  freeze = NULL) {
  if (missing(y)) {
    y <- x
    x <- seq_along(y)
  }

  new_spc_i(x, y, centre, limits, exclude, freeze)
}

#' @inheritParams spc_i
new_spc_i <- function(x = seq_along(y),
                      y = numeric(),
                      centre = NULL,
                      limits = 3,
                      exclude = rep(FALSE, length(y)),
                      freeze = NULL) {
  spc_assert()

  # thanks to https://github.com/anhoej/qicharts2
  # reorder x and y
  xo <- order(x)
  x <- x[xo]
  y <- y[xo]

  # Average moving range
  mr  <- abs(diff(y[!exclude] - centre))
  amr <- mean(mr, na.rm = TRUE)

  # Upper limit for moving ranges
  ulmr <- 3.267 * amr

  # Remove moving ranges greater than ulmr and recalculate amr, Nelson 1982
  mr  <- mr[mr < ulmr]
  amr <- mean(mr, na.rm = TRUE)

  # Calculate standard deviation, Montgomery, 6.33
  stdev <- amr / 1.128

  # repeat centre and stdev to be same length
  centre <- rep(centre, length(y))
  stdev <- rep(stdev, length(y))

  lcl <- centre - limits * stdev
  ucl <- centre + limits * stdev

  vctrs::new_rcrd(
    list(
      x = x,
      y = y,
      centre = centre,
      stdev = stdev,
      lcl = lcl,
      ucl = ucl,
      icl = y >= lcl & y <= ucl,
      exclude = exclude,
      freeze = rep(ifelse(is.null(freeze), as.numeric(NA), freeze), length(y))
    ),
    limits = limits,
    class = c("spc_i", "spc")
  )
}

#' @export
vec_ptype_full.spc_i <- vec_ptype_abbr.spc_i <- function(x, ...) {
  "spc_i"
}

#' @export
format.spc_i <- function(x, ...) {
  y <- field(x, "y")

  y
}

#' Is spc i
#'
#' Convenience function that checks if an object is an spc i object
#'
#' @param x object to test
#'
#' @return logical inidicating whether `x` is an spc i object or not
#'
#' @export
is_spc_i <- function(x) {
  inherits(x, "spc_i")
}

methods::setOldClass(c("spc_i", "vctrs_rcrd", "vctrs_vctr"))

# testing ----

if (interactive()) {
  library(ggplot2)
  library(dplyr)

  set.seed(123)
  e <- globalenv()
  e$a <- spc_i(rnorm(10))
  e$b <- spc_i(rnorm(5), limits = 3)
  e$t <- spc_i(c(rnorm(10), rnorm(5, 3, 0.4)), freeze = 10)
}

#' @export
#' @import ggplot2
plot.spc_i <- function(x, ...) {
  ggplot(vctrs::vec_data(x), aes(.data$x, .data$y)) +
    geom_line() +
    geom_point(aes(colour = .data$icl)) +
    scale_colour_viridis_d() +
    geom_line(aes(y = .data$centre)) +
    geom_line(aes(y = .data$lcl)) +
    geom_line(aes(y = .data$ucl)) +
    geom_vline(aes(xintercept = .data$freeze), linetype = "dotted", na.rm = TRUE) +
    theme_minimal() +
    theme(axis.line = element_line(),
          axis.ticks = element_line(),
          panel.background = element_blank(),
          panel.grid = element_blank())
}

# how should we handle combining spc vectors? should we?
# should we allow a model to be passed in to correct for trends/seasonality?
# should we allow something more complex than "freeze" after n points?
# should we also implement a dataframe varient on these functions? or just rely on the vector approach?
#   - df %>% spc_table(...)
#  vs
#   - df %>% mutate(spc = spc(...))
