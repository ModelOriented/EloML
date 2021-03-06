% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_elo.R
\name{calculate_actual_wins}
\alias{calculate_actual_wins}
\title{Calulate actual results for every pair of models}
\usage{
calculate_actual_wins(
  results,
  decreasing_metric = TRUE,
  compare_in_split,
  aggregate = TRUE
)
}
\arguments{
\item{results}{raw results}

\item{decreasing_metric}{if used metric is decreasing}

\item{compare_in_split}{if compare models and parameters only in fhe same fold}

\item{aggregate}{if results should be aggregated for every pair of models and hyperparameters. Otherwise output will have many rows with binary response (acccording to amount of splitsin crossvalidation) for every pair of models}
}
\description{
Calulate actual results for every pair of models
}
