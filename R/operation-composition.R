new_operation_composition = function(operation1, operation2) {
  new("operation_composition", operation1 = operation1, operation2 = operation2)
}


# operation application ---------------------------------------------------

setMethod("apply_operation", signature(operation = "operation_composition"), function(operation, layers) {
  apply_composed_operation(operation@operation2, layers * operation@operation2)
})


# printing ----------------------------------------------------------------

#' @rdname operation-class
#' @export
setMethod("format", signature(x = "operation_composition"), function(x, ...) {
  if (length(x) == 0) {
    "0"
  } else {
    format1 = format(x@operation1, ...)
    format2 = format(x@operation2, ...)
    paste0(format1, " |> ", format2)
  }
})
