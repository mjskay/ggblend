
# construct an operation --------------------------------------------------

#' Make a function that can make an operation
#' @param name string: name of the operation
#' @param constructor: bare (unevaluated) name of constructor function
#' @param y bare (unevaluated) name of an argument to `constructor` that
#'  will be filled in with the first argument to the operation when the
#'  operation is not immediately applied to a layer. Optional.
#' @noRd
make_operation = function(name, constructor, y) {
  # construct (x = x, y = y, ... = ..., etc) arg
  # list for calling the constructor
  args = formals(constructor)
  constructor_args = lapply(names(args), as.symbol)
  names(constructor_args) = names(args)
  .constructor = substitute(constructor)

  # construct args for the output function
  f_args = c(alist(object = ), args)

  # if y is provided, it is an argument that object will be copied into if
  # we are constructing the operator (and not applying it directly)
  copy_object_to_y = if (!missing(y)) {
    y = substitute(y)
    y_string = as.character(y)

    bquote(
      if (not_missing_object) {
        if (!missing(.(y))) {
          stop0(
            "Cannot provide both the `object` and `", .(y_string),
            "` arguments to `", .(name), "()` simultaneously."
          )
        }
        .(y) = object
      }
    )
  }

  f_body = bquote(splice = TRUE, {
    not_missing_object = !missing(object)
    if (not_missing_object && is_layer_like(object)) {
      operation = .(.constructor)(..(constructor_args))
      layer = as_layer_like(object)
      apply_composed_operation(operation, layer)
    } else if (not_missing_object && is(object, "operation")) {
      operation = .(.constructor)(..(constructor_args))
      new_operation_composition(object, operation)
    } else {
      .(copy_object_to_y)
      .(.constructor)(..(constructor_args))
    }
  })

  as.function(c(f_args, f_body), envir = parent.frame())
}


# type conversion ---------------------------------------------------------

setAs("numeric", "operation", function(from) {
  from * nop()
})

setAs("list", "operation", function(from) {
  as(from, "operation_sum")
})


# operation application ---------------------------------------------------

setGeneric("apply_operation", function(operation, layers) {
  stop0("Unimplemented layer operation")
})

setGeneric("apply_composed_operation", function(operation, layers) {
  apply_operation(operation, layers)
})

#' @rdname operation_product
#' @export
setMethod("*", signature(e1 = "operation"), function(e1, e2) {
  apply_operation(e1, as_layer_like(e2))
})

#' @rdname operation_product
#' @export
setMethod("*", signature(e2 = "operation"), function(e1, e2) {
  apply_operation(e2, as_layer_like(e1))
})


# printing ----------------------------------------------------------------

#' @describeIn operation Print an [operation].
#' @export
setMethod("show", signature(object = "operation"), function(object) {
  cat0("<operation>: ", format(object), "\n")
  invisible(object)
})

#' @describeIn operation Format an [operation] for printing.
#' @export
setMethod("format", signature(x = "operation"), function(x, ...) {
  arg_names = setdiff(slotNames(x), ".Data")
  args = attributes(x)[arg_names]
  args_string = format_name_value_pairs(args)
  paste0(tolower(class(x)), "(", args_string, ")")
})

