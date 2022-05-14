#' @export
adjust = function(mapping = aes(), ...) {
  names(mapping) = standardise_aes_names(names(mapping))

  params = list(...)
  names(params) = standardise_aes_names(names(params))

  new("Adjust", mapping = mapping, params = params)
}


# operation application ---------------------------------------------------

#' @export
setMethod("apply_operation", signature(operation = "Adjust"), function(operation, layers) {
  params = operation@params
  mapping = operation@mapping

  layer_apply(layers, function(layer) {
    l = ggproto(NULL, layer)

    l$mapping[names(mapping)] = mapping

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

#' @export
setMethod("*", signature(e1 = "Adjust", e2 = "Adjust"), function(e1, e2) {
  e1@mapping[names(e2@mapping)] = e2@mapping
  e1@params[names(e2@params)] = e2@params
  e1
})


# printing ----------------------------------------------------------------

#' @export
setMethod("format", signature(x = "Adjust"), function(x, ...) {
  mapping_string = paste0("aes(", format_name_value_pairs(x@mapping), ")", recycle0 = TRUE)
  params_string = format_name_value_pairs(x@params)
  args_string = paste0(c(mapping_string, params_string), collapse = ", ", recycle0 = TRUE)
  paste0(tolower(class(x)), "(", args_string, ")")
})
