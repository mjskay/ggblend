new_operation_sum = function(list) {
  new("OperationSum", operations = list)
}

#' @export
setClass("OperationSum", representation(operations = "list"), contains = "Operation")

#' @export
operation_sum = function(...) {
  new_operation_sum(list(...))
}


# predicates --------------------------------------------------------------

#' @export
is_operation_sum = function(x) {
  inherits(x, "OperationSum")
}


# type conversion ---------------------------------------------------------

#' @export
as_operation_sum = function(x) {
  UseMethod("as_operation_sum")
}

#' @export
as_operation_sum.list = function(x) {
  if (is_operation_sum(x)) {
    return(x)
  }
  x = as.list(x)
  if (!vapply(x, inherits, what = "Operation", logical(1))) {
    stop0("All objects in a OperationSum must be ggblend Operations")
  }
  new_operation_sum(x)
}

#' @export
as_operation_sum.Operation = function(x) {
  new_operation_sum(list(x))
}


# operation application ---------------------------------------------------

#' @export
setMethod("apply_operation", signature(operation = "OperationSum"), function(operation, layers) {
  simplify_layer_list(lapply(operation@operations, apply_operation, layers = layers))
})


# operation concatenation -------------------------------------------------

#' @export
setMethod("+", signature(e1 = "OperationSum", e2 = "OperationSum"), function(e1, e2) {
  new_operation_sum(c(e1@operations, e2@operations))
})

#' @export
setMethod("+", signature(e1 = "Operation", e2 = "OperationSum"), function(e1, e2) {
  new_operation_sum(c(list(e1), e2@operations))
})

#' @export
setMethod("+", signature(e1 = "OperationSum", e2 = "Operation"), function(e1, e2) {
  new_operation_sum(c(e1@operations, list(e2)))
})

#' @export
setMethod("+", signature(e1 = "Operation", e2 = "Operation"), function(e1, e2) {
  new_operation_sum(list(e1, e2))
})


# printing ----------------------------------------------------------------

#' @export
setMethod("show", signature(object = "OperationSum"), function(object) {
  cat("<OperationSum>:\n")
  print(object@operations)
})
