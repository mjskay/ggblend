
#' Layer operations
#'
#' [Operation]s are composable transformations that can be applied to \pkg{ggplot2}
#' [Layer]s or [Layer]-like objects (e.g. lists of [Layer]s; see the [Layer]
#' documentation page for a description of valid [Layer]-like objects).
#'
#' @param x,object An [Operation].
#' @param ... Further arguments passed to other methods.
#'
#' @details
#'
#' [Operation]s can be composed using the `+` and `*` operators (see [OperationSum]
#' and [OperationProduct]). Addition and multiplication of [Operation]s and [Layer]s
#' obeys the distributive law.
#'
#' [Operation]s can be applied to [Layer]s using `*` or `|>`, with slightly
#' different results:
#'
#' - Using `*`, application of [Operation]s to a list of [Layer]s *is* distributive. For example,
#'   `list(geom_line(), geom_point()) * blend("multiply")` is
#'   equivalent to `list(geom_line() * blend("multiply"), geom_point() * blend("multiply"))`;
#'   i.e. it multiply-blends the contents of the two layers individually.
#'
#' - Using `|>`, application of [Operation]s to a list of [Layer]s is *not*
#'   distributive (unless the only reasonable interpretation of applying the
#'   transformation is necessarily distributive; e.g. `adjust()`). For example,
#'   `list(geom_line(), geom_point()) |> blend("multiply")` would multiply-blend
#'   both layers together, rather than multiply-blending the contents of the
#'   two layers individually.
#'
#' @name Operation-class
#' @aliases Operation
NULL


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
  f_args = c(alist(x = ), args)

  # if y is provided, it is an argument that x will be copied into if
  # we are constructing the operator (and not applying it directly)
  copy_x_to_y = if (!missing(y)) {
    y = substitute(y)
    y_string = as.character(y)

    bquote(
      if (!missing(x)) {
        if (!missing(.(y))) {
          stop0(
            "Cannot provide both the `x` and `", .(y_string),
            "` arguments to `", .(name), "()` simultaneously."
          )
        }
        .(y) = x
      }
    )
  }

  f_body = bquote(splice = TRUE, {
    if (!missing(x) && is_layer(x)) {
      x * .(.constructor)(..(constructor_args))
    } else {
      .(copy_x_to_y)
      .(.constructor)(..(constructor_args))
    }
  })

  as.function(c(f_args, f_body), envir = parent.frame())
}


# type conversion ---------------------------------------------------------

setAs("numeric", "Operation", function(from) {
  from * nop()
})


# operation application ---------------------------------------------------

setGeneric("apply_operation", function(operation, layers) {
  stop0("Unimplemented layer operation")
})

#' @rdname OperationProduct-class
#' @export
setMethod("*", signature(e1 = "Operation"), function(e1, e2) {
  apply_operation(e1, as_layer(e2))
})

#' @rdname OperationProduct-class
#' @export
setMethod("*", signature(e2 = "Operation"), function(e1, e2) {
  apply_operation(e2, as_layer(e1))
})


# printing ----------------------------------------------------------------

#' @describeIn Operation-class Print an [Operation].
#' @export
setMethod("show", signature(object = "Operation"), function(object) {
  cat0("<Operation>: ", format(object), "\n")
  invisible(object)
})

#' @describeIn Operation-class Format an [Operation] for printing.
#' @export
setMethod("format", signature(x = "Operation"), function(x, ...) {
  arg_names = setdiff(slotNames(x), ".Data")
  args = attributes(x)[arg_names]
  args_string = format_name_value_pairs(args)
  paste0(tolower(class(x)), "(", args_string, ")")
})

