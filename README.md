# Elo rating system for Machine Learning models <img src="man/figures/logo.png" align="right" width="150"/>

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/epp)](https://cran.r-project.org/package=epp)
<img src="http://cranlogs.r-pkg.org/badges/grand-total/epp" />
[![Build
Status](https://travis-ci.org/ModelOriented/epp.svg?branch=master)](https://travis-ci.org/ModelOriented/epp)
[![Coverage
Status](https://img.shields.io/codecov/c/github/modeloriented/epp/master.svg)](https://codecov.io/github/modeloriented/epp?branch=master)

## Overview

The `epp` package (Elo Predictive Power) helps to assess model performance based Elo ranking system. 

Find more in the [EPP: interpretable score of model predictive power](https://arxiv.org/abs/1908.09213) arxiv paper.


## Installation

```{r}
# Install the the development version from GitHub:
# install.packages("devtools")
devtools::install_github("ModelOriented/epp")
```

## Usage

```
library(epp)
data(auc_scores)
calculate_epp(auc_scores)
```
