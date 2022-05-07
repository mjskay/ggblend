
# constructuor ------------------------------------------------------------

#' A blend, represented as arguments to blend()
#' @param ... arguments to blend()
#' @noRd
new_ggblend = function(...) {
  # TODO: could make this a real S4 class, but in the meantime this works
  # as a hack just to get dispatch working correctly on `*` when ggplot objects
  asS4(structure(list(...), class = "ggblend"))
}

as_ggblend = function(x) {
  if (inherits(x, "ggblend")) {
    return(x)
  }
  if (!inherits(x, c("character", "list"))) {
    stop0("Cannot convert object of class ", deparse1(x), ", to ggblend")
  }
  x = as.list(x)
  class(x) = "ggblend"
  x
}

setOldClass("ggblend")


# operators ---------------------------------------------------------------

#' Compose blends into a stack
#'
#' Operator for stacking blends: `blend(..1) ^ blend(..2)` is equivalent to `stack_blends(..1, ..2)`.
#'
#' @template param-e1-blend
#' @template param-e2-blend
#'
#' @details
#' An algebraic alternative syntax to [stack_blends()].
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
#'   geom_point(size = 3) * blend("lighten") +
#'   geom_point(size = 3) * blend("multiply", alpha = 0.65) +
#'   scale_color_brewer(palette = "Set2") +
#'   facet_grid(~ order)
#'
#' # we can reduce code duplication by using `^`
#' df |>
#'   ggplot(aes(x, y, color = set)) +
#'   geom_point(size = 3) * blend("lighten") ^ blend("multiply", alpha = 0.65) +
#'   scale_color_brewer(palette = "Set2") +
#'   facet_grid(~ order)
#'
#' @family blending functions and operators
#' @name pow-ggblend
#' @aliases ^.ggblend
NULL

#' @rdname pow-ggblend
#' @export
setMethod("^", signature(e1 = "ggblend", e2 = "ggblend"), function(e1, e2) {
  new_ggblend_stack(e1, e2)
})

#' Apply a blend to a layer
#'
#' Operator for applying blends: `geom_X(...) * blend(...)` is equivalent to `geom_X(...) |> blend(...)`.
#'
#' @template param-e1-layer-or-blend
#' @template param-e2-layer-or-blend
#'
#' @details
#' An algebraic alternative syntax to [stack_blends()]. See examples.
#'
#' @return
#' An object that can be added to a [ggplot()] object:  a `ggplot2::Layer` or
#' a list of `ggplot2::Layer`s.
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
#' # Using the "darken" blend mode, draw order does not matter:
#' df |>
#'   ggplot(aes(x, y, color = set)) +
#'   geom_point(size = 3) * blend("darken") +
#'   scale_color_brewer(palette = "Set2") +
#'   facet_grid(~ order)
#'
#' # Using the "multiply" blend mode, we can see density within groups:
#' df |>
#'   ggplot(aes(x, y, color = set)) +
#'   geom_point(size = 3) * blend("multiply") +
#'   scale_color_brewer(palette = "Set2") +
#'   facet_grid(~ order)
#'
#' # blend() on a single geom by default blends all grobs in that geom together
#' # using the requested blend mode. If we wish to blend within specific data
#' # subsets using normal blending ("over") but between subsets using the
#' # requested blend mode, we can set the blend_group aesthetic. This will
#' # make "multiply" behave more like "darken":
#' df |>
#'   ggplot(aes(x, y, color = set, blend_group = set)) +
#'   geom_point(size = 3) * blend("multiply") +
#'   scale_color_brewer(palette = "Set2") +
#'   facet_grid(~ order)
#'
#' # We can also blend lists of geoms together; these geoms are rendered using
#' # normal ("over") blending (unless a blend() call is applied to a specific
#' # sub-layer, as in the first layer below) and then blended together using
#' # the requested blend mode.
#' df |>
#'   ggplot(aes(x, y, color = set)) +
#'   list(
#'     geom_point(size = 3) * blend("darken"),
#'     geom_vline(xintercept = 0, color = "gray75", size = 1.5),
#'     geom_hline(yintercept = 0, color = "gray75", size = 1.5)
#'   ) * blend("hard.light") +
#'   scale_color_brewer(palette = "Set2") +
#'   facet_grid(~ order)
#'
#' @family blending functions and operators
#' @name times-ggblend
#' @aliases *.ggblend
NULL

#' @rdname times-ggblend
#' @export
setMethod("*", signature(e2 = "ggblend"), function(e1, e2) {
  do.call(blend, c(list(x = e1), e2))
})

#' @rdname times-ggblend
#' @export
setMethod("*", signature(e1 = "ggblend"), function(e1, e2) {
  do.call(blend, c(list(x = e2), e1))
})


# printing ----------------------------------------------------------------

#' Print a ggblend object
#'
#' Displays a `"ggblend"` object.
#'
#' @param x,object A `"ggblend"` object.
#' @param ... Additional arguments passed to other methods
#'
#' @return
#' `x` / `object`, invisibly.
#'
#' @examples
#'
#' print(blend("multiply", alpha = 0.5))
#'
#' @export
print.ggblend = function(x, ...) {
  args = as.list(match.call(
    function(blend, alpha, ...) {},
    as.call(c("blend", x))
  ))[-1]

  cat0(
    "<blend>( ",
    if ("blend" %in% names(args)) paste0(", blend = ", deparse1(args$blend)),
    if ("alpha" %in% names(args)) paste0(", alpha = ", deparse1(args$alpha)),
    ")\n"
  )
  invisible(x)
}

#' @rdname print.ggblend
#' @export
setMethod("show", signature(object = "ggblend"), function(object) print.ggblend(object))

