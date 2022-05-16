#' Layer operations
#'
#' Layer [operation]s are composable transformations that can be applied to \pkg{ggplot2}
#' [layer-like] objects, such as `stat`s, `geom`s, and lists of `stat`s and
#' `geom`s; see the [layer-like] documentation page for a description of valid
#' [layer-like] objects.
#'
#' @param x,object An [operation].
#' @param ... Further arguments passed to other methods.
#'
#' @details
#'
#' [operation]s can be composed using the `+` and `*` operators (see [operation_sum]
#' and [operation_product]). Addition and multiplication of [operation]s and [layer-like]
#' objects obeys the distributive law.
#'
#' [operation]s can be applied to [layer-like] objects using `*` or `|>`, with slightly
#' different results:
#'
#' - Using `*`, application of [operation]s to a list of [layer-like] objects *is* distributive. For example,
#'   `list(geom_line(), geom_point()) * blend("multiply")` is
#'   equivalent to `list(geom_line() * blend("multiply"), geom_point() * blend("multiply"))`;
#'   i.e. it multiply-blends the contents of the two layers individually.
#'
#' - Using `|>`, application of [operation]s to a list of [layer-like] objects is *not*
#'   distributive (unless the only reasonable interpretation of applying the
#'   transformation is necessarily distributive; e.g. `adjust()`). For example,
#'   `list(geom_line(), geom_point()) |> blend("multiply")` would multiply-blend
#'   both layers together, rather than multiply-blending the contents of the
#'   two layers individually.
#'
#' @name operation-class
#' @aliases operation
#' @export
setClass("operation")
