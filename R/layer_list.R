new_layer_list = function(list) {
  new("layer_list", .Data = list)
}

#' @export
layer_list = function(...) {
  new_layer_list(list(...))
}


# type conversion ---------------------------------------------------------

#' @export
as_layer_list = function(x) {
  UseMethod("as_layer_list")
}

#' @export
as_layer_list.layer_list = function(x) {
  x
}

#' @export
as_layer_list.list = function(x) {
  x = as.list(unlist(x))
  if (!all(vapply(x, is_layer_instance, logical(1)))) {
    stop0("All objects in a layer_list must be ggplot2 Layers")
  }
  new_layer_list(x)
}

#' @export
as_layer_list.LayerInstance = function(x) {
  new_layer_list(list(x))
}


# layer concatenation -------------------------------------------------

#' @export
setMethod("+", signature(e1 = "layer_list", e2 = "layer_list"), function(e1, e2) {
  new_layer_list(c(e1, e2))
})


# printing ----------------------------------------------------------------

#' @export
setMethod("show", signature(object = "layer_list"), function(object) {
  cat("<layer_list>:\n")
  print(object@.Data)
})
