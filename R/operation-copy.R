#' Copy layers then adjust params and aesthetics (Layer operation)
#'
#' A layer [operation] for copying and then adjusting the params and aesthetic
#' mappings of a [layer-like] object.
#'
#' @inheritParams adjust
#' @template operation
#'
#' @details
#' These are shortcuts for duplicating a layer and then applying [adjust()].
#' Specifically:
#'
#' - `copy_over(...)` is equivalent to `1 + adjust(...)`
#' - `copy_under(...)` is equivalent to `adjust(...) + 1`
#'
#' @examples
#' library(ggplot2)
#'
#' # here we use copy_under() to create a copy of
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
#'   stat_smooth(method = lm, formula = y ~ x, linewidth = 1.5, se = FALSE) *
#'     copy_under(aes(group = g), color = "white", linewidth = 4) +
#'   scale_color_brewer(palette = "Dark2")
#'
#' @name copy
NULL

new_copy_over = function(mapping = aes(), ...) {
  1 + adjust(mapping = mapping, ...)
}

#' @rdname copy
#' @export
copy_over = make_operation("copy_over", new_copy_over, mapping)

new_copy_under = function(mapping = aes(), ...) {
  adjust(mapping = mapping, ...) + 1
}

#' @rdname copy
#' @export
copy_under = make_operation("copy_over", new_copy_under, mapping)
