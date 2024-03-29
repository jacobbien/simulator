% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parallel-methods.R
\name{run_extmethod_parallel}
\alias{run_extmethod_parallel}
\title{Run one or more extended methods on simulated data.}
\usage{
run_extmethod_parallel(
  my_extmethods,
  dir,
  model_name,
  index,
  out_dir,
  out_loc,
  socket_names,
  libraries,
  save_locally = TRUE
)
}
\arguments{
\item{my_extmethods}{list of extended methods to be run in parallel}

\item{dir}{the directory where Model object was saved (by
\code{\link{generate_model}})}

\item{model_name}{the Model object's \code{name} attribute}

\item{index}{a vector of positive integer indices.  Allows simulations to be
carried out in chunks.  Each chunk gets a separate RNG stream,
meaning that the results will be identical whether we run these in
parallel or sequentially.}

\item{out_dir}{full directory to where method outputs are stored.}

\item{out_loc}{a length-1 character vector that gives location
(relative to model's path) that method outputs are stored.This can be
useful for staying organized when multiple simulations are based on
the same Model and Draws objects.}

\item{socket_names}{(quoting from \code{\link[parallel]{makePSOCKcluster}})
"either a character vector of host names on which to run the worker
copies of R, or a positive integer (in which case that number of
copies is run on localhost)."}

\item{libraries}{character vector of R packages that will be needed on the
slaves.}

\item{save_locally}{if TRUE, then files will be saved on slaves.  If FALSE,
they will be saved on master.}
}
\description{
This is an internal function. Jobs are run in parallel both by method
and by index.  Users should call the wrapper function
\code{\link{run_method}}.
}
\keyword{internal}
