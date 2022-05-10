#' @export
setClass("Adjust", representation(params = "list"), contains = "Operation")

#' @export
adjust = function(...) {
  params = list(...)
  names(params) = standardise_aes_names(names(params))
  new("Adjust", params = params)
}


# operation application ---------------------------------------------------

#' @export
setMethod("apply_operation", signature(operation = "Adjust"), function(operation, layers) {
  params = operation@params
  simplify_layer_list(lapply(layers, function(layer) {
    l = ggproto(NULL, layer)

    aes_param_names = intersect(names(params), l$geom$aesthetics())
    l$aes_params[aes_param_names] = params[aes_param_names]
    geom_param_names = intersect(names(params), l$geom$parameters(TRUE))
    l$geom_params[geom_param_names] = params[geom_param_names]
    stat_param_names = intersect(names(params), l$stat$parameters(TRUE))
    l$stat_params[stat_param_names] = params[stat_param_names]

    l
  }))
})


# printing ----------------------------------------------------------------

#' @export
setMethod("show", signature(object = "Adjust"), function(object) {
  params_string = paste0(names(object@params), " = ", vapply(object@params, deparse1, character(1)), collapse = ", ", recycle0 = TRUE)
  cat0(tolower(class(object)), "(", params_string, ")\n")
  invisible(object)
})
