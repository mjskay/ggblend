new_layer_list = function(list) {
  new("layer_list", .Data = list)
}

#' @rdname layer_list
#' @export
layer_list = function(...) {
  new_layer_list(list(...))
}


# type conversion ---------------------------------------------------------

#' @rdname layer_list
#' @export
as_layer_list = function(x) {
  UseMethod("as_layer_list")
}

#' @rdname layer_list
#' @export
as_layer_list.layer_list = function(x) {
  x
}

#' @rdname layer_list
#' @export
as_layer_list.list = function(x) {
  if (!is_layer_like(x)) {
    stop0("All objects in a layer_list must be layer-like objects")
  }
  new_layer_list(as.list(x))
}

#' @rdname layer_list
#' @export
as_layer_list.LayerInstance = function(x) {
  new_layer_list(list(x))
}


# layer concatenation -------------------------------------------------

#' @rdname layer_list
#' @export
setMethod("+", signature(e1 = "layer_list", e2 = "layer_list"), function(e1, e2) {
  new_layer_list(c(e1, e2))
})


# printing ----------------------------------------------------------------

#' @rdname layer_list
#' @export
setMethod("show", signature(object = "layer_list"), function(object) {
  cat("<layer_list>:\n")
  print(object@.Data)
})
