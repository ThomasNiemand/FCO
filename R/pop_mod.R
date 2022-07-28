#' Helper function to obtain population model for simulation based on data and model
#'
#' @param mod A lavaan model (only CFA supported so far)
#' @param x A dataset for the model of nrow observations (minimum: 50) and ncol indicators (minimum: 4)
#' @param type Type of population model. NM (the default): Uses the factor loadings and
#' covariances from Niemand & Mai's (2018) simulation study. HB: Uses the factor loadings and covariances from Hu & Bentler's (1999) simulation study.
#' EM: Empirical, uses the given factor loadings and covariances. EM is not recommended for confirmative use as it leads to the least generalizable cutoffs.
#' @param standardized Are factor loadings assumed to be standardized and covariances to be correlations (default: TRUE)?
#' @param afl Average factor loading of indicators per factor, only relevant for type = "NM" (default: .7).
#' @param aco Average correlation between factors, only relevant for type = "NM" (default: .3).
#' @return List of population model type, standardized, average factor loading and average correlation. All values are round to three decimals.
#' @examples
#'mod <- "
#'F1 =~ Q5 + Q7 + Q8
#'F2 =~ Q2 + Q4
#'F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
#'F4 =~ Q1 + Q17
#'F5 =~ Q6 + Q14 + Q15 + Q16
#'"
#'pop_mod(mod, x = bb1992, type = "NM")$pop.mod
#'pop_mod(mod, x = bb1992, type = "HB")$pop.mod
#'pop_mod(mod, x = bb1992, type = "EM")$pop.mod

#'pop_mod(mod, x = bb1992, type = "NM", afl = .9)$pop.mod
#'pop_mod(mod, x = bb1992, type = "NM", aco = .5)$pop.mod
#'pop_mod(mod, x = bb1992, type = "EM", standardized = FALSE)$pop.mod
#' @export
pop_mod <-
  function(mod,
           x,
           type = "NM",
           standardized = TRUE,
           afl = .7,
           aco = .3) {
    fit <-
      try(lavaan::cfa(mod, x, auto.fix.first = FALSE, std.lv = TRUE),
          silent = TRUE)
    if (inherits(fit, "try-error"))
      stop("Invalid model or data. Please revise.")
    lam <- lavaan::inspect(fit, "est")$lambda
    if (!all(as.vector(lam) < 1) & standardized) {
      warning("At least one loading is > 1. Consider revision of standardized.")
    }
    if (all(as.vector(lam) < 1) & !standardized) {
      warning("All loadings are < 1. Consider revision of standardized.")
    }
    cvm <- lavaan::inspect(fit, "est")$psi
    ks <- ncol(cvm)
    lam.mod <- rep(NA, ks)
    rv.mod <- lam.mod
    if (type == "NM") {
      lam.2 <- as.vector(lam)
      lam.2[which(lam.2 != 0)] <- afl
      lam <- matrix(
        lam.2,
        nrow = nrow(lam),
        ncol = ncol(lam),
        dimnames = list(rownames(lam), colnames(lam))
      )
      diag(cvm) <- 1
      cvm[lower.tri(cvm)] <- aco
    }
    if (type == "HB") {
      for (i in seq_len(ncol(lam))) {
        lam.2 <- lam[, i]
        lam.2 <- lam.2[which(lam.2 != 0)]
        lam[which(lam[, i] != 0), i] <- hb_load(lam.2)
      }
      diag(cvm) <- 1
      cvm[lower.tri(cvm)] <- rep(c(.5, .4, .3), 5)[1:ks]
    }
    eg <- expand.grid(colnames(cvm), colnames(cvm))
    eg$cvm <- round(as.vector(cvm), 3)
    eg <-
      eg[which(as.vector(lower.tri(cvm, diag = TRUE)) == TRUE), ]
    var.mod <- rep(NA, nrow(eg))
    for (i in seq_len(ks)) {
      wh <- which(lam[, i] != 0)
      lam.mod[i] <-
        paste0(colnames(lam)[i],
               "=~",
               paste0(round(lam[wh, i], 3), "*",
                      names(wh), collapse = "+"))
      if (standardized) {
        rv.mod[i] <-
          paste0(names(wh),
                 "~~",
                 paste0(round(1 - (lam[wh, i]) ^ 2, 3)),
                 "*",
                 names(wh),
                 collapse = "\n")
      }
    }
    for (i in seq_len(nrow(eg))) {
      var.mod[i] <- paste0(eg[i, 1], "~~", eg[i, 3], "*", eg[i, 2])
    }
    if (standardized) {
      #Remove identical rv
      rv.mod <- paste(rv.mod, "\n", collapse = "")
      rv.mod <-
        unique(trimws(strsplit(rv.mod, split = "\n")[[1]], "both"))
      rv.mod <- rv.mod[-which(rv.mod == "")]
      rv.mod <- paste(rv.mod, "\n", collapse = "")
      pop.mod <-
        paste(c(
          paste(lam.mod, "\n", collapse = ""),
          paste(var.mod, "\n", collapse = ""),
          rv.mod
        ), collapse = "")
    }
    if (!standardized) {
      pop.mod <-
        paste(c(
          paste(lam.mod, "\n", collapse = ""),
          paste(var.mod, "\n", collapse = "")
        ), collapse = "")
    }
    if (type == "HB") {
      afl <- .75
      aco <- .4
    }
    if (type == "EM") {
      afl <- NULL
      aco <- NULL
    }
    res <- list(
      "pop.mod" = pop.mod,
      "mod" = mod,
      "x" = x,
      "type" = type,
      "standardized" = standardized,
      "afl" = afl,
      "aco" = aco
    )
    return(res)
  }
