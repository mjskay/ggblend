#' Translate, scale, and rotate ggplot2 layers (Layer operation)
#'
#' Transform objects within a single layer (geom) or across multiple layers (geoms)
#' using affine transformations, like translation, scale, and rotation. Uses
#' the built-in compositing support in graphical devices added in R 4.2.
#'
#' @param object One of:
#'  - A [layer-like] object: applies this operation to the layer.
#'  - A missing argument: creates an [operation]
#'  - A `numeric()` or `unit()` giving the x-axis translation,
#'    which takes the place of the `x` argument.
#' @param x A `numeric()` or `unit()` giving the x translation to apply.
#' @param y A `numeric()` or `unit()` giving the y translation to apply.
#' @param width A `numeric()` or `unit()` giving the width.
#' @param height A `numeric()` or `unit()` giving the height.
#' @param angle A `numeric()` giving the angle to rotate, in degrees.
#' @template operation
#'
#' @details
#'
#' TODO: current implementation is incomplete; details forthcoming
#'
#' @section Supported devices:
#' Transformation is not currently supported by all graphics devices. As of this writing,
#' at least `png(type = "cairo")`, `svg()`, and `cairo_pdf()` are known to support
#' blending.
#'
#' `affine_transform()` attempts to auto-detect support for affine transformation using `dev.capabilities()`.
#' You may receive a warning when using `affine_transform()` if it appears transformation is not
#' supported by the current graphics device. This warning **either** means (1)
#' your graphics device does not support transformation (in which case you should
#' switch to one that does) or (2) your graphics device
#' supports transformation but incorrectly reports that it does not. Unfortunately,
#' not all graphics devices that support transformation appear to correctly *report*
#' that they support transformation, so even if auto-detection fails, `blend()` will
#' still attempt to apply the transformation, just in case.
#'
#' If the warning is issued and the output is still correctly transformed, this is
#' likely a bug in the graphics device. You can report the bug to the authors of
#' the graphics device if you wish; in the mean time, you can use
#' `options(ggblend.check_affine_transform = FALSE)` to disable the check.
#'
#' @references
#' Murrell, Paul (2021):
#' [Groups, Compositing Operators, and Affine Transformations in R Graphics](https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/groups/groups.html).
#' The University of Auckland. Report.
#' \doi{10.17608/k6.auckland.17009120.v1}.
#'
#' @examples
#'
#' # TODO: current implementation is incomplete; examples forthcoming
#'
#' @name affine_transform
#' @aliases affine_transform-class
#' @export
setClass("affine_transform", slots = list(x = "ANY", y = "ANY", width = "ANY", height = "ANY", angle = "numeric"), contains = "operation")
