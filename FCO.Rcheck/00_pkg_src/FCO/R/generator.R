#' Internal function to generate fits
#'
#' Allows for one or two models. In case of the latter, it only generates data from the population model of model one.
#' @noRd
generator <- function(x,
                      seed,
                      mode,
                      pop.mod1,
                      free1,
                      free2 = NULL,
                      s = 1,
                      k = 1,
                      nf) {
  y <- NA
  rf1 <- rep(NA, nf)
  rf2 <- rf1
  af <- rf1
  y <- try(lavaan::simulateData(
    model = pop.mod1,
    model.type = "cfa",
    sample.nobs = nrow(x),
    skewness = s,
    kurtosis = k,
    seed = seed,
    auto.fix.first = FALSE,
    std.lv = TRUE
  ),
  silent = TRUE)
  if (is.data.frame(y) & mode != "single") {
    sf1 <- try(lavaan::cfa(
      free1,
      y,
      estimator = "MLM",
      auto.fix.first = FALSE,
      std.lv = TRUE,
      warn = FALSE
    ),
    silent = TRUE)
    sf2 <- try(lavaan::cfa(
      free2,
      y,
      estimator = "MLM",
      auto.fix.first = FALSE,
      std.lv = TRUE,
      warn = FALSE
    ),
    silent = TRUE)
    if (inherits(sf1, "try-error") & inherits(sf2, "try-error")) {
      sf1 <- NA
      sf2 <- NA
    }
    if (typeof(sf1) == "S4" & typeof(sf2) == "S4") {
      if (sf1@Fit@converged & sf2@Fit@converged) {
        rf1 <- lavaan::fitmeasures(sf1)
        rf2 <- lavaan::fitmeasures(sf2)
      }
    }
  }
  if (is.data.frame(y) & mode == "single") {
    sf <- try(lavaan::cfa(
      free1,
      y,
      estimator = "MLM",
      auto.fix.first = FALSE,
      std.lv = TRUE,
      warn = FALSE
    ),
    silent = TRUE)
    if (inherits(sf, "try-error")) {
      sf <- NA
    }
    if (typeof(sf) == "S4") {
      if (sf@Fit@converged) {
        af <- lavaan::fitmeasures(sf)
      }
    }
  }
  if (mode != "single") {
    r <- rbind(rf1, rf2)
  }
  if (mode == "single") {
    r <- af
  }
  return(r)
}
