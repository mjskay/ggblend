#' Stack multiple blends of the same object on top of each other
#'
#' This is a shortcut for duplicating a layer multiple times, applying a
#' different blend to each layer, then stacking the results on top of each
#' other. This can be useful when a single blend in isolation does not achieve
#' the desired effect (e.g. when dealing with overlapping points).
#'
#' @param x One of:
#'   - A `ggplot2::Layer`, such as a `geom` or `stat`, or a list of layers.
#'   - A string (character vector of length 1) giving the name of a blend,
#'     which is assumed to be the first blend in the sequence.
#'   - A `"ggblend"` object as returned by [blend()], [stack_blends()], etc.
#'
#'  If a layer is provided, this stack of blends is applied to it.
#'  If a string or `"ggblend"` object is provided, a new
#'  `"ggblend"` object is returned which can be applied using the
#'  [`*.ggblend`] and [`^.ggblend`] operators.
#' @param ... A sequence of lists of arguments to pass to [blend()], each
#'   representing a blend applied to a copy of `x`. For example, a single string
#'   (like `"multiply"`) would result in a `blend(x, "multiply")` layer being
#'   created; a list (like `list("multiply", alpha = 0.5)`) would result
#'   in a `blend(x, "multiply", alpha = 0.5)` layer being created.
#'
#' @return
#' One of:
#'
#'  - If the input is a `ggplot2::Layer` or list, the output is a list of `ggplot2::Layer`s
#'    that can be added to a [ggplot()] object.
#'  - If the input is a string or a `"ggblend"`, the output is a `"ggblend"` object,
#'    which can be combined with other `"ggblend"` objects using [`^.ggblend`] or
#'    applied to a ggplot layer using [`*.ggblend`].
#'
#' @family blending functions and operators
#'
#' @examples
#' library(ggplot2)
#'
#' # create two versions of a dataset, where draw order can affect output
#' set.seed(1234)
#' df_a = data.frame(x = rnorm(500, 0), y = rnorm(500, 1), set = "a")
#' df_b = data.frame(x = rnorm(500, 1), y = rnorm(500, 2), set = "b")
#' df_ab = rbind(df_a, df_b) |>
#'   transform(order = "draw a then b")
#' df_ba = rbind(df_b, df_a) |>
#'   transform(order = "draw b then a")
#' df = rbind(df_ab, df_ba)
#'
#' # it can be useful to stack multiple versions of the same layer on top of
#' # each other to create a desired effect: here, `geom_point()` is used
#' # twice to lighten the multiply effect (otherwise points become very dark)
#' df |>
#'   ggplot(aes(x, y, color = set)) +
#'   geom_point(size = 3) |> blend("lighten") +
#'   geom_point(size = 3) |> blend("multiply", alpha = 0.65) +
#'   scale_color_brewer(palette = "Set2") +
#'   facet_grid(~ order)
#'
#' # we can reduce code duplication by using stack_blends()
#' df |>
#'   ggplot(aes(x, y, color = set)) +
#'   geom_point(size = 3) |> stack_blends("lighten", list("multiply", alpha = 0.65)) +
#'   scale_color_brewer(palette = "Set2") +
#'   facet_grid(~ order)
#'
#' @export
stack_blends = function(x, ...) {
  UseMethod("stack_blends")
}

#' @rdname stack_blends
#' @export
stack_blends.Layer = function(x, ...) {
  blend_args = list(...)
  lapply(blend_args, function(args) do.call(blend, c(list(x = x), as.list(args))))
}

#' @rdname stack_blends
#' @export
stack_blends.list = stack_blends.Layer

#' @rdname stack_blends
#' @export
stack_blends.character = function(x, ...) {
  new_ggblend_stack(x, ...)
}

#' @rdname stack_blends
#' @export
stack_blends.ggblend = stack_blends.character
