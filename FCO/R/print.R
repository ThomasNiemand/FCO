#Test print

#Note: Demonstration only! Please use higher numbers of replications for your applications (>= 500).
#A single model to obtain fit indices for
mod <- "
F1 =~ Q5 + Q7 + Q8
F2 =~ Q2 + Q4
F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
F4 =~ Q1 + Q17
F5 =~ Q6 + Q14 + Q15 + Q16
"

lavaan::fitmeasures(lavaan::cfa(model = mod, data = bb1992))
fits.single <- gen_fit(mod1 = mod, x = bb1992, rep = 1000, standardized = FALSE, cores = 4)
x <- recommend(fits.single, override = T, index = c("CFI", "SRMR", "RMSEA"))

print.recommend <- function(x, ...) {
  #Print empirical fit
  cat("Empirical fit values for the recommended fit indices: \n")
  print(x$recommended)
  cat("\n")
  #Print recommendation
  cat("Recommended cutoffs: \n")
  print(x$cutoffs)
  cat("\n")
  #Print decisions
  cat("Based on empirical fit and the recommended cutoffs, the model should be: \n")
  print(x$decisions)
  cat("\n")
  #Print comments
  cat(c("Replications:"), x$replications)
  cat("\n")
  cat(c("Comments:", x$comment))
  #Hiding
  invisible()
  #To use to hide the results object, but keep the result object still?
}

print.recommend(x)

fits.dv.merge <- gen_fit(
 mod1 = mod,
 x = bb1992,
 rep = 1000,
 dv = TRUE,
 dv.factors = c("F4", "F5"),
 merge.mod = TRUE
)

y <- recommend_dv(fits.dv.merge)

print.recommend_dv <- function(y, ...) {
  coy <- subset(y$cutoffs, model == "original")
  rownames(coy) <- NULL
  ccy <- subset(y$cutoffs, model != "original")
  rownames(ccy) <- NULL
  eoy <- subset(y$fit.values, model == "original")
  ecy <- subset(y$fit.values, model != "original")
  #Print empirical fit
  cat(c("Empirical fit values of CFI for the original model:", eoy$CFI))
  cat("\n")
  cat(c("Empirical fit values of CFI for the constrained/merged model:", ecy$CFI))
  cat("\n")
  #Print cutoffs
  cat("Recommended cutoffs for the original model: \n")
  print(coy[,-2])
  cat("\n")
  cat("Recommended cutoffs for the constrained/merged model: \n")
  print(ccy[,-2])
  cat("\n")
  #Print decisions
  cat("Based on empirical fit and the recommended cutoffs, discriminant validity is: \n")
  print(y$decisions)
  cat("\n")
  #Print comments
  cat(c("Replications:"), y$replications)
  cat("\n")
  cat(c("Comments:", y$comment))
  #Hiding
  invisible()
  #To use to hide the results object, but keep the result object still?
  #It would be better if it detects the type of dv testing.
}

print.recommend_dv(y)
