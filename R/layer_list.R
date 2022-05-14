new_layer_list = function(list) {
  new("LayerList", .Data = list)
}

#' @export
layer_list = function(...) {
  new_layer_list(list(...))
}


# predicates --------------------------------------------------------------

#' @export
is_layer_list = function(x) {
  inherits(x, "LayerList")
}


# type conversion ---------------------------------------------------------

#' @export
as_layer_list = function(x) {
  UseMethod("as_layer_list")
}

#' @export
as_layer_list.list = function(x) {
  if (is_layer_list(x)) {
    return(x)
  }
  x = as.list(unlist(x))
  if (!all(vapply(x, is_layer_instance, logical(1)))) {
    stop0("All objects in a LayerList must be ggplot2 Layers")
  }
  new_layer_list(x)
}

#' @export
as_layer_list.Layer = function(x) {
  new_layer_list(list(x))
}


# layer concatenation -------------------------------------------------

#' @export
setMethod("+", signature(e1 = "LayerList", e2 = "LayerList"), function(e1, e2) {
  new_layer_list(c(e1, e2))
})


# printing ----------------------------------------------------------------

#' @export
setMethod("show", signature(object = "LayerList"), function(object) {
  cat("<LayerList>:\n")
  print(object@.Data)
})
