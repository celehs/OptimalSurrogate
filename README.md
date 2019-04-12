**Model Free Approach to Quantifying the Proportion of Treatment Effect Explained by a Surrogate Marker**

by Xuan Wang, Layla Parast, Lu Tian, and Tianxi Cai.

**NOTE**: This paper is in press at _Biometrika_. 

## Overview

Provides functions to identify an optimal transformation of a potential surrogate marker such that the proportion of the treatment effect on a primary outcome can be inferred based on the treatment effect on this identified optimal transformation and functions to estimate the proportion of treatment effect explained by this optimal transformation. The potential surrogate may be continuous or discrete. Two different estimates of the proportion of treatment effect explained are available, one which is based on the ratio between the treatment effect on the optimal transformation of the potential surrogate marker and the treatment effect on the primary outcome and a second estimate which is based on quantifying how well the individual-level treatment effect on the primary outcome can be approximated by the effect on the identified optimal transformation. These estimates are based on model-free definitions of the proportion of treatment effect explained and thus, do not require any correct model specification.

## Installation

Install development version from GitHub:
 
```r
# install.packages("devtools")
devtools::install_github("celehs/OptimalSurrogate")
```

Two data examples are provided:

- `marker_cont` with **continuous** surrogate marker

- `marker_disc` with **discrete** surrogate marker

