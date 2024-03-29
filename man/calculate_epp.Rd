% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_elo.R
\name{calculate_epp}
\alias{calculate_epp}
\title{Calculate EPP Meta-Scores for All Players}
\usage{
calculate_epp(
  results,
  decreasing_metric = TRUE,
  compare_in_round = TRUE,
  keep_columns = FALSE,
  keep_model = FALSE,
  reference = NULL,
  keep_data = TRUE,
  estimation = "glmnet"
)
}
\arguments{
\item{results}{Data frame. Results for one Round Data should be in the following format.
First 3 columns should correspond to: Player, Round, Score. See more in 'details' section.}

\item{decreasing_metric}{Logical. If TRUE used Score is considered as decreasing, that means a Player with higher Score value is considered as winner.
If FALSE used Score will be considered as increasing.}

\item{compare_in_round}{Logical. If TRUE compares Players only in the same fold. If FALSE compares Players across folds.}

\item{keep_columns}{Logical. If TRUE original data frame with new 'epp' column will be returned.}

\item{keep_model}{Logical. If TRUE logistic regression model to compute EPP will be returned.}

\item{reference}{Character. Name of the Player that should be a reference level for EPP Meta-scores. It should be a name of one of the Players from
'results' data frame. If NULL, none of the Players will be chosen.}

\item{keep_data}{If all the meta-data should be kept in result.}

\item{estimation}{Method of estimating EPP coefficients, 'glm' or 'glmnet'.}
}
\value{
epp_results
}
\description{
Calculate EPP Meta-Scores for All Players
}
\details{
Format of the data frame passed via results parameter.
First column should correspond to a Player.
Second column corresponds to indexes of Rounds As EPP is based on Elo rating system, power of Player is assessed by comparing its results
with other Players on multiple Rounds. Therefore, each Player should be evaluated on multiple Rounds.
Indexes of Rounds should be in this column. And should match across Players.
Third column contains Score used to evaluate players. It can be both decreasing or increasing metric.
just remember to set the \code{decreasing_metric} parameter accordingly.
The following columns can be of any kind.

Naming convention, such as Player, Rounds, etc. comes from \href{https://arxiv.org/abs/2006.02293}{Gosiewska et al. (2020)}.
}
\examples{
library(EloML)
data(auc_scores)
calculate_epp(auc_scores[1:400,])

}
