% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_eval.R
\name{plot_eval}
\alias{plot_eval}
\title{Plot a metric's value for each method}
\usage{
plot_eval(
  object,
  metric_name,
  use_ggplot2 = TRUE,
  main,
  facet_mains,
  ylab,
  ylim,
  include_zero = FALSE,
  angle = 0,
  ...
)
}
\arguments{
\item{object}{an object of class \code{\linkS4class{Simulation}},
\code{\linkS4class{Evals}}, or \code{listofEvals}}

\item{metric_name}{the name of a metric to plot}

\item{use_ggplot2}{whether to use \code{ggplot2} (requires installation
of \code{ggplot2})}

\item{main}{title of plot. Default is \code{model_label} when evals is
a single Evals.}

\item{facet_mains}{only to be used when evals is a \code{listofEvals}
and should be of the same length. Default will be the model_label
for each model.}

\item{ylab}{the y-axis label (default is \code{metric_label})}

\item{ylim}{the y-axis limits to use (across all plots)}

\item{include_zero}{whether ylim should include 0.  Ignored if ylim
is passed explicitly}

\item{angle}{angle of labels (only when \code{use_ggplot2 = FALSE})}

\item{...}{additional arguments to pass to \code{boxplot} (only when
\code{use_ggplot2 = FALSE}).}
}
\description{
When the evaluted metric is scalar-valued, this functions makes a boxplot of
this metric for each method.  When the metric is vector-valued, this function
makes a curve with this metric on the y-axis, with one curve for each method
(the x-axis is the corresponding entry of that metric's vector). If evals is
a \code{listofEvals}, then each model will be its own plot.
}
\examples{
\dontrun{
 # suppose previously we had run the following:
 sim <- new_simulation(name = "normal-example",
                       label = "Normal Mean Estimation",
                       dir = tempdir()) \%>\%
   generate_model(make_my_example_model, n = 20) \%>\%
   simulate_from_model(nsim = 50, index = 1:3) \%>\%
   run_method(my_example_method) \%>\%
   evaluate(my_example_loss)
   # then we could plot this
   plot_eval(sim, "myloss") # "myloss" is my_example_loss@name
 }
}
\seealso{
\code{\link{plot_evals}} \code{\link{plot_eval_by}}
\code{\link{tabulate_eval}}
}
