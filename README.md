
<!-- README.md is generated from README.Rmd. Please edit that file -->
The Simulator
=============

The simulator is an R package that streamlines the process of performing simulations by creating a common infrastructure that can be easily used and reused across projects. The paper [The Simulator: An Engine to Streamline Simulations](http://arxiv.org/abs/1607.00021) discusses the simulator in greater depth.

Installation
------------

The easiest way to install the simulator is by using the [devtools](https://cran.r-project.org/package=devtools) R package (if not already installed, open R and type `install.packages("devtools")`). To install simulator, type

``` r
devtools::install_github("jacobbien/simulator")
```

in R. This installs `simulator` from github.

Vignettes
---------

The [Getting Started](http://faculty.bscb.cornell.edu/~bien/simulator_vignettes/getting-started.html) vignette walks you through setting up your first simulation with the simulator.

The best way to get a sense of how to use the simulator is to look at examples. There are several vignettes that demonstrate how the simulator can be used to conduct simulations for some of the most famous statistical methods.

1.  [Lasso](http://faculty.bscb.cornell.edu/~bien/simulator_vignettes/lasso.html) vignette: Explains basics, including the magrittr pipe and making plots and tables. Also demonstrates some more advanced features such as writing method extensions (such as refitting the result of the lasso or performing cross-validation).
2.  [James-Stein](http://faculty.bscb.cornell.edu/~bien/simulator_vignettes/js.html) vignette: Shows how to step into specific parts of the simulation for troubleshooting your code.
3.  [Elastic net](http://faculty.bscb.cornell.edu/~bien/simulator_vignettes/en.html) vignette: Shows how we can work with a sequence of methods that are identical except for a parameter that varies
4.  [Benjamini-Hochberg](http://faculty.bscb.cornell.edu/~bien/simulator_vignettes/fdr.html) vignette: Shows how we can load a preexisting simulation and add more random draws without having to rerun anything. It also shows how one can have multiple simulation objects that point to overlapping sets of results.
