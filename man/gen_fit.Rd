% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_fit.R
\name{gen_fit}
\alias{gen_fit}
\title{Obtain fit statistics from one or two models}
\usage{
gen_fit(
  mod1 = NULL,
  mod2 = NULL,
  x = NULL,
  n = NULL,
  rep = 500,
  type = "NM",
  dv = FALSE,
  dv.factors = NULL,
  merge.mod = FALSE,
  dv.cutoff = 0.9,
  standardized = TRUE,
  assume.mvn = TRUE,
  multi.core = TRUE,
  cores = 2,
  seed = 1111,
  pop.mod1 = NULL,
  pop.mod2 = NULL
)
}
\arguments{
\item{mod1}{A lavaan model to specify the CFA.}

\item{mod2}{Another lavaan model for a model comparison. If missing and merge.mod = TRUE, a merged model from function merge_factors is estimated based on mod1.}

\item{x}{A dataset for the model of nrow observations (minimum: 50) and ncol indicators (minimum: 4)}

\item{n}{A sample size specified instead of a dataset (minimum: 50, maximum: 50000). Requires a population model via pop.mod1.}

\item{rep}{Number of replications to be simulated (default: 500, minimum: 10, maximum: 5000)}

\item{type}{Type of underlying population model. Based on the model(s) provided, a population model is derived to simulate the fit indices by function pop_mod. The type determines
the factor loadings and covariances assumed for this population model. NM (the default when only one model is provided): Uses the factor loadings and
covariances from Niemand & Mai's (2018) simulation study. HB: Uses the factor loadings and covariances from Hu & Bentler's (1999) simulation study.
EM: Empirical (the default when two models are provided or merge.mod is TRUE), uses the given factor loadings and covariances.}

\item{dv}{Should the fit statistics be calculated for discriminant validity testing? If no (the default), this is not assumed. If yes, consider the arguments of merge.mod, dv.factors and cutoff.
So far, two options of discriminant validity testing are supported. Constraining: A factor correlation between two factors can be constrained as selected by the dv.factors argument. In this case, dv.cutoff applies and merge.mod is not required.
Merging: Two factors can be merged into one, again controlled by the dv.factors argument. In this case, merge.mod applies and dv.cutoff is not required (as cutoff = 1 is implied).}

\item{dv.factors}{Names of the factors to be considered. Must be equal to 2. If missing (the default), the first and second factor of the model are selected.}

\item{merge.mod}{This is used for merging. If FALSE (the default), fit measures for mod1 are estimated for a single model as long as no mod2 is provided. If TRUE, a merged model from function merge_factors is estimated based on mod1. In this case, no mod2 is required.}

\item{dv.cutoff}{This is used for constraining. It determines the critical correlation assumed to be a cutoff for discriminant validity testing.
For example, based on Rönkkö & Cho (2020), a cutoff of .9 indicates a severe issue in discriminant validity between the selected factors. Cutoffs between .8 and 1 are recommended.
The function returns a warning, if the cutoff is below .8.}

\item{standardized}{Are factor loadings assumed to be standardized and covariances to be correlations (default: TRUE)?}

\item{assume.mvn}{Should multivariate normality (mvn) be assumed? If TRUE (the default), kurtosis and skewness are set to 1 for simulated data.
If FALSE, kurtosis and skewness are estimated from dataset x via semTools::mardiaKurtosis and semTools::mardiaSkew.}

\item{multi.core}{Should multiple cores be used to simulate fit indices?
If TRUE (the default), mclapply (on Linux or Mac machines) or parLapply (on Windows machines) from parallel package with the number of specified cores is used. If FALSE, a single core is used.}

\item{cores}{How many cores should be used for multiple cores? The default is 2. Consider the available number of cores of your system.}

\item{seed}{The seed to be set to obtain reproducible cutoffs (default: 1111). Defines a vector of length rep with the seed being the first value.}

\item{pop.mod1}{For flexibility reasons, an optional lavaan population model can be provided. This is required together with n if x is missing.}

\item{pop.mod2}{Another optional lavaan population model.}
}
\value{
A list of simulated fit statistics (fco) and all previously defined parameters.
}
\description{
Obtain fit statistics from one or two models
}
\examples{
#Note: Demonstration only! Please use higher numbers of replications for your applications (>= 500).
#A single model to obtain fit indices for
mod <- "
F1 =~ Q5 + Q7 + Q8
F2 =~ Q2 + Q4
F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
F4 =~ Q1 + Q17
F5 =~ Q6 + Q14 + Q15 + Q16
"
fits.single <- gen_fit(mod1 = mod, x = bb1992, rep = 10, standardized = FALSE)

\donttest{
#Two models, an unconstrained and a constrained model to compare fit indices
mod.con <- "
F1 =~ Q5 + Q7 + Q8
F2 =~ Q2 + Q4
F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
F4 =~ Q1 + Q17
F5 =~ Q6 + Q14 + Q15 + Q16
F1 ~~ 0 * F2
"
fits.con <- gen_fit(
 mod1 = mod,
 mod2 = mod.con,
 x = bb1992,
 rep = 10
)
#Two models for discriminant validity testing, this resembles constraining with a cutoff of .9
fits.dv.con <- gen_fit(
 mod1 = mod,
 x = bb1992,
 rep = 10,
 dv = TRUE,
 dv.factors = c("F4", "F5"),
 dv.cutoff = .9
)

#Two models for discriminant validity testing, this resembles merging.
fits.dv.merge <- gen_fit(
 mod1 = mod,
 x = bb1992,
 rep = 10,
 dv = TRUE,
 dv.factors = c("F4", "F5"),
 merge.mod = TRUE
)
}
}
\references{
Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis: Conventional criteria versus new alternatives. Structural Equation Modeling, 6(1), 1–55. https://doi.org/10.1080/10705519909540118

Niemand, T., & Mai, R. (2018). Flexible cutoff values for fit indices in the evaluation of structural equation models. Journal of the Academy of Marketing Science, 46(6), 1148—1172. https://doi.org/10.1007/s11747-018-0602-9

Rönkkö, M., & Cho, E. (2020). An updated guideline for assessing discriminant validity. Organizational Research Methods. https://doi.org/10.1177/1094428120968614
}
