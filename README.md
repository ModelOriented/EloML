# Elo rating system for Machine Learning models <img src="man/figures/logo.png" align="right" width="150"/>

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/EloML)](https://cran.r-project.org/package=EloML)
<img src="http://cranlogs.r-pkg.org/badges/grand-total/EloML" />
[![Build
Status](https://travis-ci.org/ModelOriented/EloML.svg?branch=master)](https://travis-ci.org/ModelOriented/EloML)
[![Coverage
Status](https://img.shields.io/codecov/c/github/modeloriented/EloML/master.svg)](https://codecov.io/github/modeloriented/EloML?branch=master)

## Overview

The `EloML` package provides Elo rating system for machine learning models. Elo Predictive Power (EPP) score helps to assess model performance based Elo ranking system. 

Find more in the [EPP: interpretable score of model predictive power](https://arxiv.org/abs/1908.09213) arxiv paper.


## Installation

```{r}
# Install the the development version from GitHub:
# install.packages("devtools")
devtools::install_github("ModelOriented/EloML")
```

## Usage

```
library(EloML)
data(auc_scores)
calculate_elo(auc_scores)
```
