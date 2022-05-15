new_adjust = function(mapping = aes(), ...) {
  names(mapping) = standardise_aes_names(names(mapping))

  params = list(...)
  names(params) = standardise_aes_names(names(params))

  new("adjust", mapping = mapping, params = params)
}

#' @rdname adjust
#' @export
adjust = make_operation("adjust", new_adjust, mapping)


# operation application ---------------------------------------------------

setMethod("apply_operation", signature(operation = "adjust"), function(operation, layers) {
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

#' @rdname operation_product
#' @export
setMethod("*", signature(e1 = "adjust", e2 = "adjust"), function(e1, e2) {
  e1@mapping[names(e2@mapping)] = e2@mapping
  e1@params[names(e2@params)] = e2@params
  e1
})


# printing ----------------------------------------------------------------

#' @rdname operation-class
#' @export
setMethod("format", signature(x = "adjust"), function(x, ...) {
  mapping_string = paste0("aes(", format_name_value_pairs(x@mapping), ")", recycle0 = TRUE)
  params_string = format_name_value_pairs(x@params)
  args_string = paste0(c(mapping_string, params_string), collapse = ", ", recycle0 = TRUE)
  paste0(tolower(class(x)), "(", args_string, ")")
})
