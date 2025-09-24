
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Package FCO - Flexible Cutoffs for Model Fit Evaluation in Covariance-Based Structural Models

## Changes in FCO

### FCO 2.0.0

- New release including new functions (e.g., gen_fit2, flex_co2,
  plot_fit2)
- The new functions now incorporate multiple extensions of the tool for
  multple decision rules, Type I and II error control and convenience
- Old functions remain available for compatibility

------------------------------------------------------------------------

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
#> This is lavaan 0.6-19
#> lavaan is FREE software! Please report any bugs.
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- cfa(
  HS.model,
  data = HolzingerSwineford1939
)

#Fit for the model
fitmeasures(fit)
#>                  npar                  fmin                 chisq 
#>                21.000                 0.142                85.306 
#>                    df                pvalue        baseline.chisq 
#>                24.000                 0.000               918.852 
#>           baseline.df       baseline.pvalue                   cfi 
#>                36.000                 0.000                 0.931 
#>                   tli                  nnfi                   rfi 
#>                 0.896                 0.896                 0.861 
#>                   nfi                  pnfi                   ifi 
#>                 0.907                 0.605                 0.931 
#>                   rni                  logl     unrestricted.logl 
#>                 0.931             -3737.745             -3695.092 
#>                   aic                   bic                ntotal 
#>              7517.490              7595.339               301.000 
#>                  bic2                 rmsea        rmsea.ci.lower 
#>              7528.739                 0.092                 0.071 
#>        rmsea.ci.upper        rmsea.ci.level          rmsea.pvalue 
#>                 0.114                 0.900                 0.001 
#>        rmsea.close.h0 rmsea.notclose.pvalue     rmsea.notclose.h0 
#>                 0.050                 0.840                 0.080 
#>                   rmr            rmr_nomean                  srmr 
#>                 0.082                 0.082                 0.065 
#>          srmr_bentler   srmr_bentler_nomean                  crmr 
#>                 0.065                 0.065                 0.073 
#>           crmr_nomean            srmr_mplus     srmr_mplus_nomean 
#>                 0.073                 0.065                 0.065 
#>                 cn_05                 cn_01                   gfi 
#>               129.490               152.654                 0.943 
#>                  agfi                  pgfi                   mfi 
#>                 0.894                 0.503                 0.903 
#>                  ecvi 
#>                 0.423

#Simulation
#Note: Demonstration only! Please use higher numbers of replications for your applications (>= 500).
fits <- gen_fit2(fit = fit, rep = 10)
#> Warning in pop_mod(mod = mod, x = x, type = type, standardized = standardized,
#> : At least one loading is > 1. Consider revision of standardized.
#> Warning in checker(pop.mod = cpop, cfa = cfa, x = x, esti = esti, sm = sm, :
#> Skewness (80.5) or Kurtosis (987.3) are very high. Consider changing estimator
#> and/or arguments sk and ku.

#Obtain and plot cutoffs
flex_co2(fits)
#> Warning: The `x` argument of `as_tibble.matrix()` must have unique column names if
#> `.name_repair` is omitted as of tibble 2.0.0.
#> ℹ Using compatibility `.name_repair`.
#> ℹ The deprecated feature was likely used in the FCO package.
#>   Please report the issue to the authors.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> $quantiles
#> # A tibble: 3 × 5
#>   quant mod     index value type 
#>   <dbl> <chr>   <chr> <dbl> <chr>
#> 1  0.05 correct CFI   0.960 GoF  
#> 2  0.95 miss    CFI   0.981 GoF  
#> 3  0.9  miss    CFI   0.968 GoF  
#> 
#> $cutoffs
#> # A tibble: 10 × 5
#>    alpha  beta dec.rule index cutoff
#>    <dbl> <dbl> <chr>    <chr>  <dbl>
#>  1  0.05  0.05 FCO1     CFI    0.960
#>  2  0.05  0.05 FCO2     CFI    0.960
#>  3  0.05  0.05 DFI      CFI   NA    
#>  4  0.05  0.05 Fix      CFI    0.95 
#>  5  0.05  0.05 CP       CFI    0.960
#>  6  0.05  0.1  FCO1     CFI    0.960
#>  7  0.05  0.1  FCO2     CFI    0.960
#>  8  0.05  0.1  DFI      CFI   NA    
#>  9  0.05  0.1  Fix      CFI    0.95 
#> 10  0.05  0.1  CP       CFI    0.960
#> 
#> $evaluation
#> # A tibble: 10 × 14
#>    alpha  beta dec.rule index cutoff    TN    FP    TP    FN TypeI TypeII
#>    <dbl> <dbl> <chr>    <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
#>  1  0.05  0.05 FCO1     CFI    0.960    10     0     9     1   0      0.1
#>  2  0.05  0.05 FCO2     CFI    0.960    10     0     9     1   0      0.1
#>  3  0.05  0.05 Fix      CFI    0.95     10     0     9     1   0      0.1
#>  4  0.05  0.05 CP       CFI    0.960    10     0     9     1   0      0.1
#>  5  0.05  0.1  FCO1     CFI    0.960    10     0     9     1   0      0.1
#>  6  0.05  0.1  FCO2     CFI    0.960    10     0     9     1   0      0.1
#>  7  0.05  0.1  Fix      CFI    0.95     10     0     9     1   0      0.1
#>  8  0.05  0.1  CP       CFI    0.960    10     0     9     1   0      0.1
#>  9  0.05  0.05 DFI      CFI   NA         5     5    10     0   0.5    0  
#> 10  0.05  0.1  DFI      CFI   NA         5     5    10     0   0.5    0  
#> # ℹ 3 more variables: SumTypes <dbl>, Power <dbl>, Specificity <dbl>
#> 
#> $notation
#> [1] "Notation for power: positive = Models are different, negative = Models are equal, TN = correct & GoF > co or BoF < co, TP = miss & GoF < co or BoF > co, FP = correct & GoF < co or BoF > co, FN = miss & GoF > co or BoF < co"
#> 
#> $overlap
#> # A tibble: 3 × 2
#>   Statistic                 CFI
#>   <chr>                   <dbl>
#> 1 Overlap (percentage)   0.0385
#> 2 AUC (Area under curve) 0.98  
#> 3 U-test (effect size d) 2.84
plot_fit2(fits)
```

<img src="man/figures/README-example-1.png" width="100%" />
