% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/commandArgs.R
\name{commandArgs}
\alias{commandArgs}
\title{\code{\link[base:commandArgs]{base::commandArgs()}} with Defaults}
\usage{
commandArgs(trailingOnly = TRUE, args = c(), debugging = interactive())
}
\arguments{
\item{trailingOnly}{see \code{\link[base:commandArgs]{base::commandArgs()}};
no effect on \code{args}. NB: different default}

\item{args}{character vector, result to return
when \code{debugging == TRUE}}

\item{debugging}{logical, use \code{args} or not}
}
\description{
\code{\link[base:commandArgs]{base::commandArgs()}} with Defaults
}
