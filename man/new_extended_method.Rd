% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extended-method-class.R
\name{new_extended_method}
\alias{new_extended_method}
\title{Create an ExtendedMethod object}
\usage{
new_extended_method(name, label, base_method, extended_method)
}
\arguments{
\item{name}{a short name identifier.  Must be alphanumeric.}

\item{label}{a longer, human readable label that can have other characters
such as spaces, hyphens, etc.}

\item{base_method}{the object of class \code{\linkS4class{Method}} or
of class \code{\linkS4class{Method}} that is being extended}

\item{extended_method}{a function with arguments "model", "draw", "out", and
"base_method".}
}
\description{
Creates a new \code{\linkS4class{ExtendedMethod}} object.
}
