% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-operation-nop.R, R/operation-nop.R
\docType{class}
\name{nop}
\alias{nop}
\alias{nop-class}
\title{Identity ("no-op") transformation (Layer operation)}
\usage{
nop(object)
}
\arguments{
\item{object}{One of:
\itemize{
\item A \link{layer-like} object: applies this operation to the layer.
\item A missing argument: creates an \link{operation}
}}
}
\value{
A \link{layer-like} object (if \code{object} is \link{layer-like}) or an \link{operation} (if not).
}
\description{
A layer \link{operation} which returns the input \link{layer-like} object unchanged.
}
\details{
When \code{numeric()}s are used with \link{operation}s, they are converted into
sums of \code{nop()}s.
}
\examples{
library(ggplot2)

# adding a nop to another operation is equivalent to adding a numeric
adjust() + nop()

# and vice versa
adjust() + 2

# here we use adjust() with nop() ( + 1) to create a copy of
# the stat_smooth layer, putting a white outline around it.
set.seed(1234)
k = 1000
data.frame(
  x = seq(1, 10, length.out = k),
  y = rnorm(k, seq(1, 2, length.out = k) + c(0, 0.5)),
  g = c("a", "b")
) |>
  ggplot(aes(x, y, color = g)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x, linewidth = 1.5, se = FALSE) *
    (adjust(aes(group = g), color = "white", linewidth = 4) + 1) +
  scale_color_brewer(palette = "Dark2")

# (note this could also be done with copy_under())

}
\seealso{
\link{operation} for a description of layer operations.

Other layer operations: 
\code{\link{adjust}},
\code{\link{affine_transform}},
\code{\link{blend}},
\code{\link{copy}},
\code{\link{partition}()}
}
\concept{layer operations}
