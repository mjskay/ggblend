new_layer_list = function(list) {
  new("LayerList", .Data = list)
}

#' @export
setClass("LayerList", contains = "list")

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
  x = as.list(x)
  if (!vapply(x, inherits, what = "Layer", logical(1))) {
    stop0("All objects in a LayerList must be ggplot2 Layers")
  }
  new_layer_list(x)
}

#' @export
as_layer_list.Layer = function(x) {
  new_layer_list(list(x))
}

#' When returning layers / lists from an adjustment, use this function so that
#' layer lists of length 1 are returned as a single layer
#' @noRd
simplify_layer_list = function(x) {
  if (is.list(x)) {
    if (length(x) == 1) {
      x[[1]]
    } else {
      new_layer_list(x)
    }
  } else {
    x
  }
}

# printing ----------------------------------------------------------------

#' @export
setMethod("show", signature(object = "LayerList"), function(object) {
  cat("<LayerList>:\n")
  print(asS3(object))
})
