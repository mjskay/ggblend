#' Stack multiple blends of the same object on top of each other
#'
#' This is a shortcut for duplicating a layer multiple times, applying a
#' different blend to each layer, then stacking the results on top of each
#' other. This can be useful when a single blend in isolation does not achieve
#' the desired effect (e.g. when dealing with overlapping points).
#'
#' @template param-x-layer
#' @param ... A sequence of lists of arguments to pass to [blend()], each
#'   representing a blend applied to a copy of `x`. For example, a single string
#'   (like `"multiply"`) would result in a `blend(x, "muliply")` layer being
#'   created; a list (like `list("multiply", alpha = 0.5)`) would result
#'   in a `blend(x, "multiply", alpha = 0.5)` layer being created.
#'
#' @return
#' A list of `ggplot2::Layers`s which can be added to a `ggplot()` object.
#'
#' @seealso [blend()]
#' @examples
#'
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
  blend_args = list(...)
  lapply(blend_args, function(args) do.call(blend, c(list(x = x), as.list(args))))
}
