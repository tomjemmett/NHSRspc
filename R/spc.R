#' @importFrom rlang abort is_formula
spc_assert <- function(env = parent.frame()) {
  with(env, {
    x <- vec_cast(x, double())
    y <- vec_cast(y, double())
    if (length(y) < 1) {
      rlang::abort("`y` must have at least 1 value")
    }

    vctrs::vec_assert(exclude, logical(), size = length(y))

    if (!is.null(freeze)) {
      if (rlang::is_formula(freeze)) {
        freeze <- purrr::as_mapper(freeze)
      }

      if (is.function(freeze)) {
        freeze <- min(which(!freeze(x)))
      }

      if (!is.numeric(freeze)) {
        rlang::abort("`freeze` must be a numeric or a function")
      }

      freeze <- vctrs::vec_cast(freeze, double())

      vctrs::vec_assert(freeze, double(), size = 1)

      if (freeze < 1 | freeze > length(y)) {
        rlang::abort("`freeze` must be between 1 and the length of `y`")
      }
      exclude <- exclude | c(rep(FALSE, freeze), rep(TRUE, length(y) - freeze))
    }

    if (is.null(centre)) centre <- mean(y[!exclude], na.rm = TRUE)
    centre <- vec_cast(centre, double())
    limits <- vec_cast(limits, double())

    vctrs::vec_assert(x, double(), size = length(y))
    vctrs::vec_assert(centre, double(), size = 1)
    vctrs::vec_assert(limits, double(), size = 1)

    if (any(is.na(x))) {
      rlang::abort("`x` should not contain any NA values")
    }

    if (any(is.na(y))) {
      rlang::abort("`y` should not contain any NA values")
    }

    if (is.na(centre)) {
      rlang::abort("`centre` cannot be NA")
    }

    if (limits <= 0) {
      rlang::abort("`limits` must be greater than 0")
    }

    if (is.na(limits)) {
      rlang::abort("`limits` cannot be NA")
    }

    if (any(is.na(exclude))) {
      rlang::abort("`exclude` cannot contain any NA values")
    }
  })
}

#' Is spc
#'
#' Convenience function that checks if an object is an spc object
#'
#' @param x object to test
#'
#' @return logical inidicating whether `x` is an spc object or not
#'
#' @export
is_spc <- function(x) {
  inherits(x, "spc")
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.spc <- function(x, ..., sigfig = NULL) {
  # default colouring is red for negative, default for positive
  # should we colour based on spc rule violations?
  pillar::pillar_shaft(field(x, "y"), sigfig = sigfig)
}

#' @export
vec_ptype2.spc.spc <- function(x, y, ...) {
  if (class(x)[[1]] != class(y)[[1]]) {
    stop("Cannot combine ", class(x)[[1]], " and ", class(y)[[1]], " together.")
  }

  xl <- attr(x,  "limits")
  yl <- attr(y, "limits")

  if (xl != yl) {
    stop("Cannot combine spc elements with different limits")
  }
  x
}

# #' @export
# c.spc <- function(...) {
#   dots <- list(...)
#
#   max_group <- 0
#   for (i in seq_along(dots)) {
#     g <- field(dots[[i]], "group") + max_group
#     field(dots[[i]], "group") <- g
#     max_group <- max(g)
#   }
#
#   do.call(vec_c, dots)
# }

#' @export
obj_print_data.spc <- function(x, ..., signif = 3) {
  d <- vctrs::vec_data(x)

  v <- ifelse(abs(d$y) < 1,
              formatC(signif(d$y, signif), format = "f"),
              formatC(d$y, 3, format = "f"))

  fn <- function(i) {
    if (d$y[[i]] < d$lcl[[i]] | d$y[[i]] > d$ucl[[i]]) {
      crayon::red(v[[i]])
    } else {
      as.character(v[[i]])
    }
  }

  cat(vapply(seq_along(d$y), fn, character(1)), sep = ", ")
  cat("\n")
}

#' @export
obj_print_footer.spc <- function(x, ...) {
  cat(">> summary of spc rule violations!!!\n")
}
