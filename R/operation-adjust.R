#' Adjust layer params and aesthetics (Layer Operation)
#'
#' A layer [Operation] for adjusting the params and aesthetic mappings of
#' a [Layer].
#'
#' @param x One of:
#'  - A [Layer]-like object: applies this operation to the layer.
#'  - A missing argument: creates an [Operation]
#'  - Anything else: creates an [Operation], passing `x` along to the
#'    `mapping` argument
#' @param mapping An aesthetic created using `aes()`. Mappings provided here
#'   will overwrite mappings in [Layer]s when this [Operation] is applied to
#'   them.
#' @param ... [Layer] parameters. Params provided here will overwrite
#'   params in [Layer]s when this [Operation] is applied to them.
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
NULL

new_adjust = function(mapping = aes(), ...) {
  names(mapping) = standardise_aes_names(names(mapping))

  params = list(...)
  names(params) = standardise_aes_names(names(params))

  new("Adjust", mapping = mapping, params = params)
}

#' @rdname adjust
#' @export
adjust = make_operation("adjust", new_adjust, mapping)


# operation application ---------------------------------------------------

setMethod("apply_operation", signature(operation = "Adjust"), function(operation, layers) {
  params = operation@params
  mapping = operation@mapping

  layer_apply(layers, function(layer) {
    l = ggproto(NULL, layer)

    l$mapping[names(mapping)] = mapping
    if (!is.null(l$mapping)) {
      class(l$mapping) = "uneval"
    }

    aes_param_names = intersect(names(params), l$geom$aesthetics())
    l$aes_params[aes_param_names] = params[aes_param_names]
    geom_param_names = intersect(names(params), l$geom$parameters(TRUE))
    l$geom_params[geom_param_names] = params[geom_param_names]
    stat_param_names = intersect(names(params), l$stat$parameters(TRUE))
    l$stat_params[stat_param_names] = params[stat_param_names]

    l
  })
})


# operation multiplication -------------------------------------------------

#' @rdname OperationProduct-class
#' @export
setMethod("*", signature(e1 = "Adjust", e2 = "Adjust"), function(e1, e2) {
  e1@mapping[names(e2@mapping)] = e2@mapping
  e1@params[names(e2@params)] = e2@params
  e1
})


# printing ----------------------------------------------------------------

#' @rdname Operation-class
#' @export
setMethod("format", signature(x = "Adjust"), function(x, ...) {
  mapping_string = paste0("aes(", format_name_value_pairs(x@mapping), ")", recycle0 = TRUE)
  params_string = format_name_value_pairs(x@params)
  args_string = paste0(c(mapping_string, params_string), collapse = ", ", recycle0 = TRUE)
  paste0(tolower(class(x)), "(", args_string, ")")
})
