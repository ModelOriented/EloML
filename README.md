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

Installation time should not exceed 1 minute.

```r
# Install the the development version from GitHub:
# install.packages("devtools")
devtools::install_github("ModelOriented/EloML")
```

## Usage

The following example takes less than 20 seconds to complete.

Load `EloML` library and benchmark data. In the example we use the data frame `auc_data` from the `EloML` package. The data used for EPP calculations should be a data frame, where first 3 columns correspond to: Player (`model`), Round (`split`), Score (`auc`).

```{r}
library(EloML)
data(auc_scores)

head(auc_scores)

#        model split       auc
# 1 catboost_1     1 0.9824724
# 2 catboost_1     2 0.9820267
# 3 catboost_1     3 0.9801000
# 4 catboost_1     4 0.9848932
# 5 catboost_1     5 0.9845456
# 6 catboost_1     6 0.9858062

```

To calculate EPP use `calculate_epp` function. For more options see help of the function `?calculate_epp`.

```{r}
calculate_epp(auc_scores)

# Head of Players EPP: 
#       player        epp
# 1 catboost_1  -0.793627
# 2 catboost_2   2.915507
# 3 catboost_3  -1.990134
# 4      gbm_1 -20.381584
# 5     gbm_10   1.664303
# 6     gbm_11   2.714073
# Type of estimation:  glmnet
```
