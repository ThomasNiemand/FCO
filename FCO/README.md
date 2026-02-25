
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Package FCO - Flexible Cutoffs for Model Fit Evaluation in Covariance-Based Structural Models

## Changes in FCO

### FCO 2.0.2

- Small changes to solve CRAN check errors on some platforms.

------------------------------------------------------------------------

### FCO 2.0.1

- Skewness and kurtosis now estimated by an internal function as
  semTools may be archived.

### FCO 2.0.0

- New release including new functions (e.g., gen_fit2, flex_co2,
  plot_fit2)
- The new functions now incorporate multiple extensions of the tool for
  multple decision rules, Type I and II error control and convenience
- Old functions remain available for compatibility

### FCO 0.8.0

- Changed vignette after release on CRAN
- Fixed issue in index_guess
- Allows to specify sample size n instead of dataset x

### FCO 0.7.2

- New seed argument in gen_fit for reproducible cutoffs

### FCO 0.7.1

- Added a `NEWS.md` file to track changes to the package.
- Minor revisions to tests

### FCO 0.7.0

- Speed improvements in the vignette
- New naming scheme
- Minor revisions to the descriptions and references
- Added contributor

### FCO 0.69

- Bug fixes in gen_fit for OS compatibility
- Improvements in the vignette

### FCO 0.67

- First stable release

## Description

The goal of FCO is to to derive flexible cutoffs for fit indices in
Covariance-based Structural Equation Modeling based on the paper by
Niemand & Mai (2018). Flexible cutoffs are an alternative to fixed
cutoffs - rules-of-thumb - regarding an appropriate cutoff for fit
indices such as CFI or SRMR. It has been demonstrated that these
flexible cutoffs perform better than fixed cutoffs in grey areas where
misspecification is not easy to detect. The package provides an
alternative to the tool at flexiblecutoffs.org as it allows to tailor
flexible cutoffs to a given dataset and model, which is so far not
available in the tool. The package simulates fit indices based on a
given dataset and model and then estimates the flexible cutoffs. Some
useful functions, e.g., to determine the GoF or BoF-nature of a fit
index, are provided. So far, additional options for a relative use (is a
model better than another?) are provided in an exploratory manner.
Starting with version 2, we offer a lot improvements and additional new
decision rules as well as many flexibility options.

## Installation

You can install the FCO from CRAN [CRAN](https://cran.r-project.org)
with:

``` r
install.packages("FCO")
library(FCO)
```

## Example

This is the basic usage for FCO in case of deriving flexible cutoffs for
a single model:

``` r
library(FCO)
library(lavaan)
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- cfa(
  HS.model,
  data = HolzingerSwineford1939
)

#Fit for the model
fitmeasures(fit)

#Simulation
#Note: Demonstration only! Please use higher numbers of replications for your applications (>= 500).
fits <- gen_fit2(fit = fit, rep = 100)

#Obtain and plot cutoffs
flex_co2(fits)
plot_fit2(fits)
```
