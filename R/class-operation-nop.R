#' Identity ("no-op") transformation (Layer operation)
#'
#' A layer [operation] which returns the input [layer-like] object unchanged.
#'
#' @param object One of:
#'  - A [layer-like] object: applies this operation to the layer.
#'  - A missing argument: creates an [operation]
#'
#' @details
#'
#' When `numeric()`s are used with [operation]s, they are converted into
#' sums of `nop()`s.
#'
#' @template operation
#'
#' @examples
#' library(ggplot2)
#'
#' # adding a nop to another operation is equivalent to adding a numeric
#' adjust() + nop()
#'
#' # and vice versa
#' adjust() + 2
#'
#' # here we use adjust() with nop() ( + 1) to create a copy of
#' # the stat_smooth layer, putting a white outline around it.
#' set.seed(1234)
#' k = 1000
#' data.frame(
#'   x = seq(1, 10, length.out = k),
#'   y = rnorm(k, seq(1, 2, length.out = k) + c(0, 0.5)),
#'   g = c("a", "b")
#' ) |>
#'   ggplot(aes(x, y, color = g)) +
#'   geom_point() +
#'   stat_smooth(method = lm, formula = y ~ x, size = 1.5, se = FALSE) *
#'     (adjust(aes(group = g), color = "white", size = 4) + 1) +
#'   scale_color_brewer(palette = "Dark2")
#'
#' # (note this could also be done with copy_under())
#'
#' @name nop
#' @aliases nop-class
#' @export
setClass("nop", contains = "operation")
