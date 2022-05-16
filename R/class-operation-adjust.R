#' Adjust layer params and aesthetics (Layer operation)
#'
#' A layer [operation] for adjusting the params and aesthetic mappings of
#' a [layer-like] object.
#'
#' @param x One of:
#'  - A [layer-like] object: applies this operation to the layer.
#'  - A missing argument: creates an [operation]
#'  - Anything else: creates an [operation], passing `x` along to the
#'    `mapping` argument
#' @param mapping An aesthetic created using `aes()`. Mappings provided here
#'   will overwrite mappings in [ggplot2::layer()]s when this [operation] is applied to
#'   them.
#' @param ... [ggplot2::layer()] parameters, such as would be passed to a `geom_...()`
#'   or `stat_...()` call. Params provided here will overwrite params in layers when
#'   this [operation] is applied to them.
#'
#' @template operation
#'
#' @examples
#'
#' library(ggplot2)
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
#' @name adjust
#' @aliases adjust-class
#' @export
setClass("adjust", representation(mapping = "ANY", params = "list"), contains = "operation")
