% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TabPFN-fit.R
\name{tab_pfn}
\alias{tab_pfn}
\alias{tab_pfn.default}
\alias{tab_pfn.data.frame}
\alias{tab_pfn.matrix}
\alias{tab_pfn.formula}
\alias{tab_pfn.recipe}
\title{Fit a \code{TabPFN} model.}
\usage{
tab_pfn(x, ...)

\method{tab_pfn}{default}(x, ...)

\method{tab_pfn}{data.frame}(x, y, ignore_pretraining_limits = FALSE, n_jobs = 1L, ...)

\method{tab_pfn}{matrix}(x, y, ignore_pretraining_limits = FALSE, n_jobs = 1L, ...)

\method{tab_pfn}{formula}(formula, data, ignore_pretraining_limits = FALSE, n_jobs = 1L, ...)

\method{tab_pfn}{recipe}(x, data, ignore_pretraining_limits = FALSE, n_jobs = 1L, ...)
}
\arguments{
\item{x}{Depending on the context:
\itemize{
\item A \strong{data frame} of predictors.
\item A \strong{matrix} of predictors.
\item A \strong{recipe} specifying a set of preprocessing steps
created from \code{\link[recipes:recipe]{recipes::recipe()}}.
}}

\item{...}{Not currently used, but required for extensibility.}

\item{y}{When \code{x} is a \strong{data frame} or \strong{matrix}, \code{y} is the outcome
specified as:
\itemize{
\item A \strong{data frame} with 1 numeric column.
\item A \strong{matrix} with 1 numeric column.
\item A numeric \strong{vector} for regression or a \strong{factor} for classification.
}}

\item{ignore_pretraining_limits}{A logical. The maximum number of features
500 officially supported by the TabPFN python api. Set
\code{ignore_pretraining_limits} to \code{TRUE} to override.}

\item{n_jobs}{The number of parallel process workers.}

\item{formula}{A formula specifying the outcome terms on the left-hand side,
and the predictor terms on the right-hand side.}

\item{data}{When a \strong{recipe} or \strong{formula} is used, \code{data} is specified as:
\itemize{
\item A \strong{data frame} containing both the predictors and the outcome.
}}
}
\value{
A \code{tab_pfn} object with elements:
\itemize{
\item \code{fit}: the python object containing the model.
\item \code{levels}: a character string of class levels (or NULL for regression)
\item \code{training}: a vector with the training set dimensions.
\item \code{versions}: a list of python and python package versions and information.
\item \code{logging}: any R or python messages produced by the computations.
\item \code{blueprint}: am object produced by \code{\link[hardhat:mold]{hardhat::mold()}} used to process
new data during prediction.
}
}
\description{
\code{tab_pfn()} fits a model.
}
\details{
Predictors do not require preprocessing; missing values and factor vectors
are allowed.
}
\examples{
predictors <- mtcars[, -1]
outcome <- mtcars[, 1]

# XY interface
mod <- tab_pfn(predictors, outcome)

# Formula interface
mod2 <- tab_pfn(mpg ~ ., mtcars)

# Recipes interface
if (!rlang::is_installed("recipes")) {
 library(recipes)
 rec <-
  recipe(mpg ~ ., mtcars) \%>\%
  step_log(disp)

 mod3 <- tab_pfn(rec, mtcars)
 mod3
}

}
\references{
Hollmann, Noah, Samuel Müller, Lennart Purucker, Arjun Krishnakumar, Max
Körfer, Shi Bin Hoo, Robin Tibor Schirrmeister, and Frank Hutter.
"Accurate predictions on small data with a tabular foundation model."
\emph{Nature} 637, no. 8045 (2025): 319-326.

Hollmann, Noah, Samuel Müller, Katharina Eggensperger, and Frank Hutter.
"Tabpfn: A transformer that solves small tabular classification problems in
a second." \emph{arXiv preprint} arXiv:2207.01848 (2022).

Müller, Samuel, Noah Hollmann, Sebastian Pineda Arango, Josif Grabocka, and
Frank Hutter. "Transformers can do bayesian inference." \emph{arXiv preprint}
arXiv:2112.10510 (2021).
}
