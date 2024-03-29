% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-operation-sum.R, R/operation-sum.R
\docType{class}
\name{operation_sum}
\alias{operation_sum}
\alias{operation_sum-class}
\alias{sum,operation-method}
\alias{+,operation,operation-method}
\alias{+,operation,numeric-method}
\alias{+,numeric,operation-method}
\alias{format,operation_sum-method}
\title{Layer operation sums}
\usage{
\S4method{sum}{operation}(x, ..., na.rm = FALSE)

\S4method{+}{operation,operation}(e1, e2)

\S4method{+}{operation,numeric}(e1, e2)

\S4method{+}{numeric,operation}(e1, e2)

\S4method{format}{operation_sum}(x, ...)
}
\arguments{
\item{x, ...}{\link{operation}s}

\item{na.rm}{ignored}

\item{e1}{an \link{operation} or \code{\link[=numeric]{numeric()}}}

\item{e2}{an \link{operation} or \code{\link[=numeric]{numeric()}}}
}
\value{
An \link{operation}.
}
\description{
\link{operation}s can be added together to form stacks of operations, which
when multiplied by (applied to) \link{layer-like} objects, those \link{layer-like} objects are distributed
over the \link{operation}s (i.e. copied).
}
\details{
Addition of \pkg{ggblend} \link{operation}s depends on the types of
objects being summed:
\itemize{
\item If you add an \link{operation} to an \link{operation}, they are merged into
a single \link{operation} that copies input \link{layer-like} objects, one for each \link{operation}.
\item If you add an \link{operation} to a \code{\link[=numeric]{numeric()}} \emph{n}, it is equivalent to
adding \code{*} \code{\link[=nop]{nop()}}s to that \link{operation}.
}
}
\examples{
library(ggplot2)

# adding operations together creates a sum of operations
adjust(color = "red") + adjust(linewidth = 2)

# addition and multiplication obey the distributive law
op = (adjust(aes(y = 11 -x), color = "skyblue") + 1) * (adjust(color = "white", linewidth = 4) + 1)
op

# multiplication by a geom returns a modified version of that geom,
# distributed over the sum of the operations
data.frame(x = 1:10) |>
  ggplot(aes(x = x, y = x)) +
  geom_line(linewidth = 2) * op
}
