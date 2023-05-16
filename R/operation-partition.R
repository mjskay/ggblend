#' Partition a layer into subgroups (Layer operation)
#'
#' A layer [operation] for adding a `partition` aesthetic to a [layer].
#'
#' @param object One of:
#'  - A [layer-like] object: applies this operation to the layer.
#'  - A missing argument: creates an [operation]
#'  - Anything else: creates an [operation], passing `object` along to the
#'    `partition` argument
#' @param partition One of:
#'   - A list of quosures, such as returned by [vars()], giving a (possibly multi-)
#'     column expression for the `partition` aesthetic. These expressions are
#'     combined using [interaction()] to be passed on to `aes(partition = ...)`
#'   - A one-sided formula, giving a single-column expression for the `partition`
#'     aesthetic, which is passed on to `aes_(partition = ...)`.
#' @template operation
#'
#' @details
#' This is a shortcut for setting the `partition` aesthetic of a [layer].
#'
#' - `partition(~ XXX)` is roughly equivalent to `adjust(aes(partition = XXX))`
#' - `partition(vars(X, Y, ...))` is roughly equivalent to `adjust(aes(partition = interaction(X, Y, ...)))`
#'
#' When a [layer] with a `partition` aesthetic is used by the following
#' [operation]s, the effects of the operations are applied across groups:
#'
#' - [blend()]: Blends graphical objects within the subgroups defined by the
#'   partition together using normal (`"over"`) blending before applying its
#'   blend between subgroups.
#'
#' @examples
#' library(ggplot2)
#'
#' # TODO
#'
#' @name partition
NULL

new_partition = function(partition) {
  if (inherits(partition, "quosures")) {
    if (length(partition) > 1) {
      mapping = aes(partition = interaction(!!!partition, drop = TRUE, lex.order = TRUE))
    } else {
      mapping = aes(partition = !!partition[[1]])
    }
  } else if (inherits(partition, "formula")) {
    mapping = aes(partition = !!rlang::as_quosure(partition))
  } else {
    mapping = aes(partition = !!partition)
  }

  adjust(mapping = mapping)
}

#' @rdname partition
#' @export
partition = make_operation("partition", new_partition, partition)
