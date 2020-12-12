# Introduction 

This package was created for STAT 302. It includes the following functions:

'my_t_test'
'my_lm'
'my_knn_cv'
'my_rf_cv'

In order to install this package from Github, you will need to input the following code into your console:

```{r, eval = FALSE}
devtools::install_github("jackgo1889/mystatspackage")
library(mystatspackage)
```
 
  <!-- badges: start -->
[![Build Status](https://travis-ci.com/jackgo1889/mystatspackage.svg?branch=master)](https://travis-ci.com/jackgo1889/mystatspackage)
  [![Codecov test coverage](https://codecov.io/gh/jackgo1889/mystatspackage/branch/master/graph/badge.svg)](https://codecov.io/gh/jackgo1889/mystatspackage?branch=master)
  <!-- badges: end -->

# Installing Vignette

The vignette included within the package demontrates usage of all functions. To install the vignettes with the package from Github, input the following code:

```{r, eval = FALSE}
install.packages("devtools")
devtools::install_github("jackgo1889/mystatspackage", build_vignette = TRUE, build_opts = c(), force = TRUE)
library(mystatspackage)
# Use this to view the vignette in the mystatspackpage HTML help
help(package = "mystatspackage", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "mystatspackage")
```

