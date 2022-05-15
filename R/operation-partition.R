#' Partition a layer into subgroups (Layer operation)
#'
#' A layer [operation] for adding a `partition` aesthetic to a [layer].
#'
#' @param x One of:
#'  - A [Layer]-like object: applies this operation to the layer.
#'  - A missing argument: creates an [operation]
#'  - Anything else: creates an [operation], passing `x` along to the
#'    `partition` argument
#' @param partition A quoted column expression for the `partition`
#'   aesthetic, to be passed on to `aes_()`; e.g. a [formula],
#'   [quosure], or output from [vars()].
#' @template operation
#'
#' @details
#' This is a shortcut for setting the `partition` aesthetic of a [layer].
#' `partition(~ XXX)` is equivalent to `adjust(aes(partition = XXX))`.
#'
#' When a [layer] with a `partition` aesthetic is used by the following
#' [operation]s, the effects of the operations are applied to each subgroup
#' defined by the partition: [blend()].
#'
#' @examples
#' library(ggplot2)
#'
#' # TODO
#'
#' @name partition
NULL

new_partition = function(partition) {
  if (is.list(partition) && length(partition) == 1) {
    # to support partition(vars(...))
    partition = partition[[1]]
  }
  adjust(aes_(partition = partition))
}

#' @rdname copy
#' @export
partition = make_operation("partition", new_partition, partition)
