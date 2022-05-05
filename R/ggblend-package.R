#' Blending and compositing for ggplot2
#'
#' @docType package
#' @name ggblend-package
#' @aliases ggblend
#'
#' @description
#'
#' \pkg{ggblend} is an R package that adds support for R 4.2 blend modes
#' (e.g. `"multiply"`, `"overlay"`, etc) to \pkg{ggplot2}.
#'
#' @details
#'
#' Support for blending layers (e.g. `geom`s) in \pkg{ggplot2}. See `blend()`.
#'
#' @section Package options:
#'
#' The following global options can be set using `options()` to modify the
#' behavior of \pkg{ggblend}:
#'
#' - `"ggblend.check_blend"`: If `TRUE` (default), `blend()` will warn if
#'   you attempt to use a blend mode not supported by the current graphics
#'   device, as reported by `dev.capabilities()$compositing`. Since this check
#'   can be unreliable on some devices (they will report not support a blend
#'   mode that they do support), you can disable this warning by setting this
#'   option to `FALSE`.
#'
#' @import grid
#' @import ggplot2
NULL
