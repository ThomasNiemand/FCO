#' Dataset from Babakus & Boller (1992)
#'
#' Data from Babakus & Boller (1992) who investigated the dimensionality of the SERVQUAL scale based on a sample of N = 502.
#' The data is available as a data.frame (simulated via mvrnorm in package MASS based on the correlation matrix provided by the authors) and used in the vignette.
#' @docType data
#' @usage data(bb1992)
#' @format A data.frame of 22 variables (Q1-Q22) with 502 observations
#' @keywords datasets
#' @references Babakus, E., & Boller, G. W. (1992). An empirical assessment of the SERVQUAL scale. Journal of Business Research, 24(3), 253–268.
#' @examples
#'data(bb1992)
#'head(bb1992, 3)
#' @source Provided in paper
"bb1992"
#'
#' Obtain factor loadings as in Hu & Bentler (1999)
#'
#' @param x Vector of length (1:10) containing the loadings per factor
#' @return Vector of factor loadings for popluation model
#' @references Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis: Conventional criteria versus new alternatives. Structural Equation Modeling, 6(1), 1–55.
#' @noRd
hb_load <- function(x) {
  checkmate::assertInteger(
    length(x),
    lower = 1,
    upper = 10,
    any.missing = FALSE,
    null.ok = FALSE
  )
  if (length(x) == 1)
    y <- 1
  else {
    l <- c(.7, .7, .7, .7, .75, .75, .8, .8, .8, .8)
    m <-
      list(
        c(5:6),
        c(4, 6, 8),
        c(4:7),
        c(3:4, 6, 8:9),
        c(3:6, 8:9),
        c(2:4, 6, 8:10),
        c(2:6, 8:10),
        c(1:4, 6, 7:10),
        c(1:10)
      )
    y <- l[m[[length(x) - 1]]]
  }
  return(y)
}

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
#' @references Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis: Conventional criteria versus new alternatives. Structural Equation Modeling, 6(1), 1–55.
#' @references Niemand, T., & Mai, R. (2018). Flexible cutoff values for fit indices in the evaluation of structural equation models. Journal of the Academy of Marketing Science, 46(6), 1148—1172.
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
      for (i in 1:ncol(lam)) {
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
    for (i in 1:ks) {
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
    for (i in 1:nrow(eg)) {
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
#' Obtain fit statistics from one or two models
#'
#' @param mod1 A lavaan model to specifiy the CFA.
#' @param mod2 Another lavaan model for a model comparison. If missing and merge.mod = TRUE, a merged model from function merge_factors is estimated based on mod1.
#' @param x A dataset for the model of nrow observations (minimum: 50) and ncol indicators (minimum: 4)
#' @param rep Number of replications to be simulated (default: 500, minimum: 10, maximum: 5000)
#' @param type Type of underlying population model. Based on the model(s) provided, a population model is derived to simulate the fit indices by function pop_mod. The type determines
#' the factor loadings and covariances assumed for this population model. NM (the default when only one model is provided): Uses the factor loadings and
#' covariances from Niemand & Mai's (2018) simulation study. HB: Uses the factor loadings and covariances from Hu & Bentler's (1999) simulation study.
#' EM: Empirical (the default when two models are provided or merge.mod is TRUE), uses the given factor loadings and covariances.
#' @param dv Should the fit statistics be calculated for discriminant validity testing? If no (the default), this is not assumed. If yes, consider the arguments of merge.mod, dv.factors and cutoff.
#' So far, two options of discriminant validity testing are supported. Constraining: A factor correlation between two factors can be constrained as selected by the dv.factors argument. In this case, dv.cutoff applies and merge.mod is not required.
#' Merging: Two factors can be merged into one, again controlled by the dv.factors argument. In this case, merge.mod applies and dv.cutoff is not required (as cutoff = 1 is implied).
#' @param dv.factors Names of the factors to be considered. Must be equal to 2. If missing (the default), the first and second factor of the model are selected.
#' @param merge.mod This is used for merging. If FALSE (the default), fit measures for mod1 are estimated for a single model as long as no mod2 is provided. If TRUE, a merged model from function merge_factors is estimated based on mod1. In this case, no mod2 is required.
#' @param dv.cutoff This is used for constraining. It determines the critical correlation assumed to be a cutoff for discriminant validity testing.
#' For example, based on Rönkkö & Cho (2020), a cutoff of .9 indicates a severe issue in discriminant validity between the selected factors. Cutoffs between .8 and 1 are recommended.
#' The function returns a warning, if the cutoff is below .8.
#' @param standardized Are factor loadings assumed to be standardized and covariances to be correlations (default: TRUE)?
#' @param assume.mvn Should multivariate normality (mvn) be assumed? If TRUE (the default), kurtosis and skewness are set to 1 for simulated data.
#' If FALSE, kurtosis and skewness are estimated from dataset x via semTools::mardiaKurtosis and semTools::mardiaSkew.
#' @param multi.core Should multiple cores be used to simulate fit indices?
#' If TRUE (the default), mclapply (on Linux or Mac machines) or parLapply (on Windows machines) from parallel package with the number of specified cores is used. If FALSE, a single core is used.
#' @param cores How many cores should be used for multiple cores? The default is 2. Consider the available number of cores of your system.
#' @param pop.mod1 For flexibility reasons, an optional lavaan population model can be provided.
#' @param pop.mod2 Another optional lavaan population model.
#' @return A list of simulated fit statistics (fco) and all previously defined parameters.
#' @references Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis: Conventional criteria versus new alternatives. Structural Equation Modeling, 6(1), 1–55.
#' @references Niemand, T., & Mai, R. (2018). Flexible cutoff values for fit indices in the evaluation of structural equation models. Journal of the Academy of Marketing Science, 46(6), 1148—1172.
#' @references Rönkkö, M., & Cho, E. (2020). An updated guideline for assessing discriminant validity. Organizational Research Methods. doi: 10.1177/1094428120968614.
#' @examples
#'
#' #A single model to obtain fit indices for
#'mod <- "
#'F1 =~ Q5 + Q7 + Q8
#'F2 =~ Q2 + Q4
#'F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
#'F4 =~ Q1 + Q17
#'F5 =~ Q6 + Q14 + Q15 + Q16
#'"
#' fits.single <- gen_fit(mod1 = mod, x = bb1992, rep = 100, standardized = FALSE)
#'
#' #Two models, an unconstrained and a constrained model to compare fit indices
#'mod.con <- "
#'F1 =~ Q5 + Q7 + Q8
#'F2 =~ Q2 + Q4
#'F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
#'F4 =~ Q1 + Q17
#'F5 =~ Q6 + Q14 + Q15 + Q16
#'F1 ~~ 0 * F2
#'"
#'fits.con <- gen_fit(
#'  mod1 = mod,
#'  mod2 = mod.con,
#'  x = bb1992,
#'  rep = 100
#')

#' #Two models for discriminant validity testing, this resembles constraining with a cutoff of .9
#'fits.dv.con <- gen_fit(
#'  mod1 = mod,
#'  x = bb1992,
#'  rep = 100,
#'  dv = TRUE,
#'  dv.factors = c("F4", "F5"),
#'  dv.cutoff = .9
#')
#'
#' #Two models for discriminant validity testing, this resembles merging.
#'fits.dv.merge <- gen_fit(
#'  mod1 = mod,
#'  x = bb1992,
#'  rep = 100,
#'  dv = TRUE,
#'  dv.factors = c("F4", "F5"),
#'  merge.mod = TRUE
#')
#' @export
gen_fit <-
  function(mod1,
           mod2 = NULL,
           x,
           rep = 500,
           type = "NM",
           dv = FALSE,
           dv.factors = NULL,
           merge.mod = FALSE,
           dv.cutoff = .9,
           standardized = TRUE,
           assume.mvn = TRUE,
           multi.core = TRUE,
           cores = 2,
           pop.mod1 = NULL,
           pop.mod2 = NULL) {
    #Checks
    checkmate::assertCharacter(mod1,
                               fixed = "=~")
    checkmate::assertCharacter(mod2,
                               fixed = "=~", null.ok = TRUE)
    checkmate::assertDataFrame(x,
                               min.rows = 50,
                               min.cols = 4,
                               col.names = "unique")
    checkmate::assertNumeric(rep, lower = 10, upper = 5000)
    checkmate::assert(
      checkmate::checkCharacter(type,
                                pattern = "EM"),
      checkmate::checkCharacter(type,
                                pattern = "HB"),
      checkmate::checkCharacter(type,
                                pattern = "NM")
    )
    checkmate::assertLogical(dv)
    checkmate::assertVector(dv.factors, len = 2, null.ok = TRUE)
    checkmate::assertLogical(merge.mod)
    checkmate::assertVector(dv.cutoff, len = 1, null.ok = FALSE)
    if (dv.cutoff > 1 |
        dv.cutoff < 0)
      stop("Cutoff not between 0 and 1. Please revise.")
    if (dv.cutoff < .8)
      warning("Cutoff below recommended standards for discriminant validity. Consider with care.")
    checkmate::assertLogical(standardized)
    checkmate::assertLogical(assume.mvn)
    checkmate::assertLogical(multi.core)
    checkmate::assertNumeric(cores, lower = 1)
    #Check the structure of pop.mods
    if (!is.null(pop.mod1) &
        is.list(pop.mod1))
      pop.mod1 <- pop.mod1$pop.mod
    if (!is.null(pop.mod2) &
        is.list(pop.mod2))
      pop.mod2 <- pop.mod2$pop.mod
    checkmate::assertCharacter(pop.mod1,
                               fixed = "=~",
                               null.ok = TRUE)
    checkmate::assertCharacter(pop.mod2,
                               fixed = "=~",
                               null.ok = TRUE)
    #Check the current no. of fit indices provided by lavaan
    nf <-
      length(lavaan::fitmeasures(
        lavaan::cfa(
          mod = "F =~ x1 +x2 +x3",
          data = lavaan::HolzingerSwineford1939,
          estimator = "MLM"
        )
      ))
    #Set normality
    if (!assume.mvn) {
      k <-
        unname(semTools::mardiaKurtosis(x)["b2d"]) / (ncol(x) * (ncol(x) + 2))
      #Centered kurtosis
      s <- unname(semTools::mardiaSkew(x)["b1d"])
    }
    if (assume.mvn) {
      k <- 1
      s <- 1
    }
    seeds <- 111111:116110 #old: 999999
    RNGkind("L'Ecuyer-CMRG")
    #Seeds are hold constant for replication
    if (multi.core) {
      if (parallel::detectCores() < cores)
        stop(
          "The number of available cores is lower than the number of selected cores. Please revise."
        )
      if (.Platform$OS.type == "windows") {
        clus <- parallel::makePSOCKcluster(cores)
      }
    }
    if (!multi.core) {
      cores <- 1
    }
    g <- expand.grid(nullm2 = c(T, F),
                     mm = c(T, F),
                     dv = c(T, F))
    g$opt <- c(rep("dv", 5), rep("cfa", 3))
    g$mode <-
      c(
        "merging",
        "constraining",
        "constraining",
        "constraining",
        "merging",
        "dual",
        "single",
        "dual"
      )
    wh <- which(g$nullm2 == is.null(mod2) &
                  g$mm == merge.mod & g$dv == dv)
    if (wh == 2 | wh == 3 | wh == 4)
      message(
        "Constraining is selected as the discriminant validity testing option given the provided arguments."
      )
    if (wh == 1)
      message(
        "Merging is selected as the discriminant validity testing option given the provided arguments."
      )
    if (wh == 5)
      message("Merge.factors is TRUE, but dv is FALSE. Merging is selected while assuming dv to be TRUE.")
    if (wh == 6)
      message(
        "Two models are provided, but merge.factors is TRUE. Estimation continues while assuming merge.factors to be FALSE."
      )
    mode <- g[wh, "mode"]
    if (mode == "single") {
      if (is.null(pop.mod1)) {
        pop.mod <-
          pop_mod(
            mod = mod1,
            x = x,
            type = type,
            standardized = standardized
          )$pop.mod
      }
      if (!is.null(pop.mod1)) {
        pop.mod <- pop.mod1
      }
      free <- mod1
      if (multi.core & .Platform$OS.type != "windows") {
        fits <-
          parallel::mclapply(1:rep, function(r) {
            generator(
              x = x,
              seed = seeds[r],
              mode = mode,
              pop.mod1 = pop.mod,
              free1 = free,
              s = s,
              k = k,
              nf = nf
            )
          }, mc.cores = cores)
      }
      if (multi.core & .Platform$OS.type == "windows") {
        fits <-
          parallel::parLapply(clus, 1:rep, function(r) {
            generator(
              x = x,
              seed = seeds[r],
              mode = mode,
              pop.mod1 = pop.mod,
              free1 = free,
              s = s,
              k = k,
              nf = nf
            )
          })
      }
      if (!multi.core) {
        fits <-
          lapply(1:rep, function(r) {
            generator(
              x = x,
              seed = seeds[r],
              mode = mode,
              pop.mod1 = pop.mod,
              free1 = free,
              s = s,
              k = k,
              nf = nf
            )
          })
      }
    }
    if (mode != "single") {
      type <- "EM"
      if (is.null(pop.mod1)) {
        pop.mod1 <-
          pop_mod(
            mod = mod1,
            x = x,
            type = type,
            standardized = standardized
          )$pop.mod
      }
      free1 <- mod1
      if (mode == "dual" | mode == "constraining") {
        #Dual models, no dv
        if (is.null(pop.mod2) & mode == "dual") {
          if (is.null(mod2))
            stop("No second model provided. Please revise.")
          free2 <- mod2
        }
        #Constraining:
        if (is.null(pop.mod2) & mode == "constraining") {
          free2 <-
            ifelse(
              is.null(mod2),
              constr_mod(mod1, dv.factors = dv.factors, dv.cutoff = dv.cutoff),
              mod2
            )
          mod2 <- free2
        }
        if (!is.null(pop.mod2) & mode == "constraining") {
          #This is required as otherwise the simulation would take fit measures from the population model without the cutoff.
          free2 <-
            ifelse(is.null(mod2),
                   get_free(lavaan::lavaanify(
                     pop_mod_dv(
                       pop.mod2,
                       dv.factors = dv.factors,
                       dv.cutoff = dv.cutoff
                     )
                   ), dv.factors, mode),
                   mod2)
          mod2 <- free2
        }
      }
      #Merging:
      if (mode == "merging") {
        pt2 <-
          try(merge_factors(lavaan::cfa(pop.mod1, x, warn = FALSE)), silent = TRUE)
        if (inherits(pt2, "try-error"))
          stop("Merging two-factors not successful. Please check.")
        free2 <-
          ifelse(is.null(mod2), get_free(pt2, dv.factors, mode), mod2)
        mod2 <- free2
      }
      if (multi.core & .Platform$OS.type != "windows") {
        fits <-
          parallel::mclapply(1:rep, function(r) {
            generator(
              x = x,
              seed = seeds[r],
              mode = mode,
              pop.mod1 = pop.mod1,
              free1 = free1,
              free2 = free2,
              s = s,
              k = k,
              nf = nf
            )
          }, mc.cores = cores)
      }
      if (multi.core & .Platform$OS.type == "windows") {
        fits <-
          parallel::parLapply(clus, 1:rep, function(r) {
            generator(
              x = x,
              seed = seeds[r],
              mode = mode,
              pop.mod1 = pop.mod1,
              free1 = free1,
              free2 = free2,
              s = s,
              k = k,
              nf = nf
            )
          })
      }
      if (!multi.core) {
        fits <-
          lapply(1:rep, function(r) {
            generator(
              x = x,
              seed = seeds[r],
              mode = mode,
              pop.mod1 = pop.mod1,
              free1 = free1,
              free2 = free2,
              s = s,
              k = k,
              nf = nf
            )
          })
      }
    }
    if (multi.core & .Platform$OS.type == "windows") {
      parallel::stopCluster(clus)
    }
    res <- list(
      "fco" = fits,
      "mod1" = mod1,
      "mod2" = mod2,
      "x" = x,
      "rep" = rep,
      "type" = type,
      "dv" = dv,
      "dv.factors" = dv.factors,
      "merge.mod" = merge.mod,
      "dv.cutoff" = dv.cutoff,
      "standardized" = standardized,
      "assume.mvn" = assume.mvn,
      "multi.core" = multi.core,
      "cores" = cores,
      "pop.mod1" = pop.mod1,
      "pop.mod2" = pop.mod2
    )
    return(res)
  }

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

#' Helper function that guesses GoF or BoF from a given index name
#'
#' @param index A fit index or measure provided by function fitmeasures in package lavaan
#' @return Returns GoF (Goodness-of-Fit index) or BoF (Badness of Fit index).
#' @examples
#' index_guess("cfi")
#' index_guess("tli")
#' index_guess("rmsea")
#' index_guess("srmr")
#' @export
index_guess <- function(index) {
  #Expand later
  bof <- c("rmsea", "rmr", "srmr", "crmr")
  gof <-
    c("cfi",
      "tli",
      "nnfi",
      "rfi",
      "nfi",
      "pnfi",
      "ifi",
      "rni",
      "gfi",
      "agfi",
      "pgfi",
      "mfi")
  if (grepl(".", index)) {
    index <- strsplit(index, "[.]")[[1]][1]
  }
  if (grepl("_", index)) {
    index <- strsplit(index, "[_]")[[1]][1]
  }
  idx <- tolower(index)
  r <-
    ifelse(idx %in% gof, "GoF", ifelse(idx %in% bof, "BoF", "not a fit index"))
  return(r)
}

#' Helper function that gets free model from a lavaan parameter table.
#'
#' @param pt A lavaan parameter table. Please use factors / latent variables with uppercase names (e.g., F1). The function is an addition to simstandard:fixed2free which only works with models.
#' @param dv.factors The selected factors relevant in case of retaining constraints
#' @param mode Mode to be a relevant for constraints. Only if mode is constraining, constraints are kept.
#' @return The syntax of a free lavaan model
#' @noRd
get_free <- function(pt, dv.factors, mode) {
  # vars <-
  #   unique(pt[which(grepl("^[[:upper:]]", pt$lhs) == TRUE &
  #                     grepl("^[[:upper:]]", pt$rhs) == TRUE), "lhs"])
  vars <-
    unique(pt[which(grepl("^[[:upper:]]", pt$lhs) == TRUE &
                      pt$op == "=~"), "lhs"])
  if (mode == "constraining")
    mod <- rep(NA, length = length(vars) + 1)
  if (mode != "constraining")
    mod <- rep(NA, length = length(vars))
  for (i in 1:length(vars)) {
    wh <- which(grepl(vars[i], pt$lhs) == TRUE & pt$op == "=~")
    mod[i] <-
      paste0(vars[i], " =~ ", paste(pt[wh, "rhs"], collapse = " + "))
  }
  if (mode == "constraining") {
    cv <-
      which(
        grepl(dv.factors[1], pt$lhs) == TRUE &
          pt$op == "~~" & grepl(dv.factors[2], pt$rhs) == TRUE
      )
    #Maybe, the dv.factors are reversed
    if (length(cv) == 0) {
      dv.factors <- dv.factors[c(2, 1)]
      cv <-
        which(
          grepl(dv.factors[1], pt$lhs) == TRUE &
            pt$op == "~~" & grepl(dv.factors[2], pt$rhs) == TRUE
        )
    }
    if (length(cv) == 0)
      stop("Constrained correlation for selected factors not found. Perhaps misspelled?")
    mod[length(mod)] <-
      paste0(dv.factors[1], " ~~ ", paste(pt[cv, "ustart"]), "*", dv.factors[2])
  }
  mod <- paste(mod, collapse = "\n")
  return(mod)
}

#' Internal function that takes a model and determines an alternative population model with a fixed correlation between selected factors for discriminant validity testing (constraining)
#'
#' @param pop.mod A population model, potentially from function pop_mod.
#' @param dv.factors (same as constr_mod)
#' @param dv.cutoff (same as constr_mod)
#' @return An alternative population model with the cutoff as correlation between the selected factors. This population model can be used in function gen_fit to generate flexible cutoffs.
#' @noRd
pop_mod_dv <-
  function(pop.mod,
           dv.factors = NULL,
           dv.cutoff) {
    pop.mod <- gsub(" ", "", pop.mod)
    pms <- unlist(strsplit(pop.mod, "\n"))
    vars <- gsub("=~.*", "", pms[grep("=~", pms, fixed = TRUE)])
    if (!is.null(dv.factors))
      mf <- vars[match(dv.factors, vars)]
    if (is.null(dv.factors))
      mf <- vars[c(1, 2)]
    if (any(is.na(mf)))
      stop("At least one of the factors to be merged is not a factor in the model. Please revise.")
    lhs <- grep(paste0(mf[2], "~~"), pms)
    rhs <- grep(paste0("*" , mf[1]), pms)
    #Maybe the order of factors is inversed in pms?
    if (!any(lhs %in% rhs)) {
      lhs2 <- grep(paste0(mf[1], "~~"), pms)
      rhs2 <- grep(paste0("*" , mf[2]), pms)
      if (any(lhs2 %in% rhs2)) {
        cvr <- pms[rhs2[match(lhs2, rhs2)[1]]]
        cvr <-
          as.numeric(unlist(strsplit(gsub(
            ".*~~", "", cvr
          ), "*", fixed = TRUE))[1])
        pop.mod2 <-
          gsub(
            paste0(mf[1], "~~", cvr, "*", mf[2]),
            paste0(mf[1], "~~", dv.cutoff, "*", mf[2]),
            pop.mod,
            fixed = TRUE
          )
      }
    }
    if (any(lhs %in% rhs)) {
      cvr <- pms[rhs[match(lhs, rhs)[1]]]
      cvr <-
        as.numeric(unlist(strsplit(gsub(".*~~", "", cvr), "*", fixed = TRUE))[1])
      pop.mod2 <-
        gsub(
          paste0(mf[2], "~~", cvr, "*", mf[1]),
          paste0(mf[2], "~~", dv.cutoff, "*", mf[1]),
          pop.mod,
          fixed = TRUE
        )
    }
    return(pop.mod2)
  }

#' Internal function that checks standardization and changes cutoff if needed
#'
#' @param fit A fitted lavaan model.
#' @param dv.factors ...
#' @param dv.cutoff ...
#' @return A cutoff for discriminant validity testing if model is not standardized
#' @noRd
check_std <- function(fit, dv.factors, dv.cutoff) {
  psi <- lavaan::inspect(fit, "free")$psi
  std <- any(diag(psi) == 0)
  if (!std) {
    sc <-
      (lavaan::inspect(fit, "cor.lv") / lavaan::inspect(fit, "cov.lv"))[dv.factors[1], dv.factors[2]]
    dv.cutoff <- dv.cutoff / sc
  }
  return(round(dv.cutoff, 3))
}

#' Helper function that constraints a given model for discriminant validity testing (constraining)

#' @param mod A lavaan model to be constrained.
#' @param dv.factors Names of the factors to be considered. Must be equal to 2. If missing (the default), the first and second factor of the model are selected.
#' @param dv.cutoff Critical correlation assumed to be a cutoff for discriminant validity testing.
#' For example, based on Rönkkö & Cho (2020), a cutoff of .9 indicates a severe issue in discriminant validity between the selected factors. Cutoffs between .8 and 1 are recommended.
#' The function returns a warning, if the cutoff is below .8.
#' @return An alternative model with the cutoff as correlation between the selected factors. This model can be used in function gen_fit to generate flexible cutoffs. It will be automatically generated in gen_fit, if not provided manually.
#' @references Rönkkö, M., & Cho, E. (2020). An updated guideline for assessing discriminant validity. Organizational Research Methods. doi: 10.1177/1094428120968614.
#' @noRd
constr_mod <- function(mod, dv.factors = NULL, dv.cutoff) {
  mod <- gsub(" ", "", mod)
  pms <- unlist(strsplit(mod, "\n"))
  vars <- gsub("=~.*", "", pms[grep("=~", pms, fixed = TRUE)])
  if (!is.null(dv.factors))
    mf <- vars[match(dv.factors, vars)]
  if (is.null(dv.factors))
    mf <- vars[c(1, 2)]
  if (any(is.na(mf)))
    stop("At least one of the factors to be merged is not a factor in the model. Please revise.")
  lhs <- grep(paste0(mf[2], "~~"), pms)
  rhs <- grep(paste0("*" , mf[1]), pms)
  mt <- match(lhs, rhs)
  if (length(mt) == 0) {
    lhs2 <- grep(paste0(mf[1], "~~"), pms)
    rhs2 <- grep(paste0("*" , mf[2]), pms)
    mt <- match(lhs2, rhs2)
  }
  if (length(mt) == 0) {
    mod2 <-
      paste(mod, paste0(mf[2], "~~", dv.cutoff, "*", mf[1]), sep = "\n")
  }
  if (length(mt) != 0) {
    if (exists("lhs2")) {
      pms[stats::na.omit(c(lhs2[mt], rhs2[mt]))] <-
        paste0(mf[2], "~~", dv.cutoff, "*", mf[1])
    }
    else {
      pms[stats::na.omit(c(lhs[mt], rhs[mt]))] <-
        paste0(mf[2], "~~", dv.cutoff, "*", mf[1])
    }
    mod2 <- paste(pms, sep = "\n", collapse = "\n")
  }
  return(mod2)
}

#' Internal function that takes a model and merges factors for discriminant validity testing (merging)
#'
#' @param fit A fitted lavaan model.
#' @param merged.factors Names of the factors to be merged. Must be equal to 2. If missing (the default), the first and second factor of the model are selected.
#' The first factor named will be retained while the second factor will be dropped.
#' @return A lavaan parameter table of the merged factors.
#' @noRd
merge_factors <- function(fit, merged.factors = NULL) {
  if (class(fit) != "lavaan")
    stop("Object fit is not a fitted model from lavaan. Please revise.")
  checkmate::assertVector(merged.factors, len = 2, null.ok = TRUE)
  std.lv <- ifelse(fit@Options$std.lv, TRUE, FALSE)
  pt <- as.data.frame(fit@ParTable)
  # vars <-
  #   unique(pt[which(grepl("^[[:upper:]]", pt$lhs) == TRUE &
  #                     grepl("^[[:upper:]]", pt$rhs) == TRUE), "lhs"])
  vars <- rownames(lavaan::inspect(fit, "veta"))
  if (!is.null(merged.factors))
    mf <- match(merged.factors, vars)
  if (is.null(merged.factors))
    mf <- c(1, 2)
  if (any(is.na(mf)))
    stop("At least one of the factors to be merged is not a factor in the model. Please revise.")
  pt$lhs[which(pt$lhs == vars[mf[2]] &
                 pt$op == "=~")] <- vars[mf[1]]
  pt2 <-
    lapply(pt, function(x)
      x[seq(1:length(pt$lhs))[-which(pt$rhs == vars[mf[2]] |
                                       pt$lhs == vars[mf[2]])]])
  pt2$id <- seq(1:length(pt2$id))
  if (std.lv) {
    fw <- which(pt2$free == 0)
  }
  if (!std.lv) {
    lw <- which(pt2$lhs == vars[mf[1]] & pt2$op == "=~")
    pt2$ustart[lw] <- c(1, rep(NA, length(lw) - 1))
    fw <- which(pt2$free == 0)[-2]
  }
  pt2$free[-fw] <- seq(1:(length(pt2$free) - length(fw)))
  pt2$start <- NULL
  pt2$est <- NULL
  pt2$se <- NULL
  pt2 <- as.data.frame(pt2)
  return(pt2)
}

#' Obtain flexible cutoffs for one or two models
#'
#' @param fits A list of simulated fit indices obtained from gen_fit. Based on the structure of fits, the number of models is derived.
#' @param index A vector of fit indices or measures provided by function fitmeasures in package lavaan
#' @param alpha.lev The predefined uncertainty
#' For example, if the default uncertainty of .05 (5 percent) is accepted a-priori, the 5 percent stats::quantile (of type 8, see ?stats::quantile) of the simulated distribution for correctly specified CFA models with the given model and sample characteristics determines the flexible cutoff.
#' Options are .001, .01, .05, and .10. Higher values are more conservative.
#' @param gof An optional vector as to whether the indices are GoF (Goodness-of-Fit index)? If TRUE, a GoF is assumed. If FALSE, a BoF is assumed.
#' Depending on the nature of the underlying fit index, the appropriate lower (GoF) or upper (BoF) width of the respective confidence interval as defined by the stats::quantile is used to derive the flexible cutoff.
#' If not provided or not equal to the number of fit indices, the function guesses the type for known fit indices (e.g., SRMR is a BoF).
#' @return A list of information regarding the selected fit index providing its flexible cutoff for the given parameters.
#' @examples
#' #A single model to obtain fit indices for
#'mod <- "
#'F1 =~ Q5 + Q7 + Q8
#'F2 =~ Q2 + Q4
#'F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
#'F4 =~ Q1 + Q17
#'F5 =~ Q6 + Q14 + Q15 + Q16
#'"
#'fits.single <- gen_fit(mod1 = mod, x = bb1992, rep = 100, standardized = FALSE)
#'flex_co(fits = fits.single, index = c("CFI", "SRMR"))
#' #Two models, an unconstrained and a constrained model to compare fit indices
#'mod.con <- "
#'F1 =~ Q5 + Q7 + Q8
#'F2 =~ Q2 + Q4
#'F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
#'F4 =~ Q1 + Q17
#'F5 =~ Q6 + Q14 + Q15 + Q16
#'F1 ~~ 0 * F2
#'"
#'fits.con <- gen_fit(
#'  mod1 = mod,
#'  mod2 = mod.con,
#'  x = bb1992,
#'  rep = 100
#')
#'flex_co(fits = fits.con,
#'        index = c("CFI", "SRMR"),
#'        alpha.lev = .05)
#'
#' #Two models for discriminant validity testing, this resembles constraining with a cutoff of .9
#'fits.dv.con <- gen_fit(
#'  mod1 = mod,
#'  x = bb1992,
#'  rep = 100,
#'  dv = TRUE,
#'  dv.factors = c("F4", "F5"),
#'  dv.cutoff = .9
#')
#'flex_co(fits = fits.dv.con,
#'index = "CFI",
#'alpha.lev = .05)
#'mod.dv.con <- "
#'F1 =~ Q5 + Q7 + Q8
#'F2 =~ Q2 + Q4
#'F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
#'F4 =~ Q1 + Q17
#'F5 =~ Q6 + Q14 + Q15 + Q16
#'F4 ~~ .9 * F5
#'"
#'lavaan::fitmeasures(
#'  lavaan::cfa(
#'    model = mod.dv.con,
#'    data = bb1992,
#'    auto.fix.first = FALSE,
#'    std.lv = TRUE
#'  ),
#'  fit.measures = "cfi"
#')

#' #Two models for discriminant validity testing, this resembles merging.
#'fits.dv.merge <- gen_fit(
#'  mod1 = mod,
#'  x = bb1992,
#'  rep = 100,
#'  dv = TRUE,
#'  dv.factors = c("F4", "F5"),
#'  merge.mod = TRUE
#')
#'flex_co(fits = fits.dv.merge,
#'index = "CFI",
#'alpha.lev = .05)
#'mod.dv.merge <- "
#'F1 =~ Q5 + Q7 + Q8
#'F2 =~ Q2 + Q4
#'F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
#'F4 =~ Q1 + Q17 + Q6 + Q14 + Q15 + Q16
#'"
#'lavaan::fitmeasures(
#'  lavaan::cfa(
#'    model = mod.dv.merge,
#'    data = bb1992
#'  ),
#'  fit.measures = "cfi"
#')
#' @export
flex_co <-
  function(fits,
           index,
           alpha.lev = .05,
           gof = NULL) {
    checkmate::assertCharacter(index, min.chars = 3)
    index <- tolower(index)
    checkmate::assertNumeric(alpha.lev, len = 1)
    checkmate::assertLogical(gof, null.ok = TRUE)
    if (length(fits$fco) < 500)
      warning(
        "The number of replications is lower than the recommended minimum of 500. Consider with care."
      )
    if (!alpha.lev %in% c(.001, .01, .05, .10))
      warning("The selected alpha level is unequal to .001, .01, .05, or .10.")
    if (is.null(gof) | length(gof) != length(index)) {
      gof <- ifelse(sapply(index, index_guess) == "GoF", TRUE, FALSE)
    }
    ml <- ifelse(is.null(nrow(fits$fco[[1]])), 1, 2)
    fits <- fits$fco
    if (ml == 1) {
      fits.nna <- fits[!sapply(fits, function(x)
        all(is.na(x)))]
      na <- length(fits) - length(fits.nna)
      sh.na <- na / length(fits)
      if (!all(index %in% names(fits.nna[[1]])))
        stop("At least one selected index is not a supported fitmeasure in lavaan.")
      sf <- unname(sapply(fits.nna, function(x)
        x[index]))
      co <- rep(NA, length(index))
      probs <-
        sapply(gof, function(x)
          ifelse(x, alpha.lev, 1 - alpha.lev))
      for (i in 1:length(index)) {
        #Type of stats::quantile is set to 8
        if (length(index) > 1) {
          co[i] <-
            stats::quantile(sf[i,], probs[i], type = 8)
        }
        if (length(index) == 1) {
          co[i] <-
            stats::quantile(sf, probs, type = 8)
        }
      }
      names(co) <- toupper(index)
      names(gof) <- toupper(index)
      rco <-
        list(
          "cutoff" = co,
          "index" = toupper(index),
          "alpha" = alpha.lev,
          "gof" = gof,
          "replications" = length(fits),
          "number of non-converging models" = na,
          "share of non-converging models" = round(sh.na, 3)
        )
    }
    if (ml == 2) {
      fits.nna <- fits[!sapply(fits, function(x)
        all(is.na(x)))]
      na <- length(fits) - length(fits.nna)
      sh.na <- na / length(fits)
      if (!all(index %in% names(fits.nna[[1]][1,])))
        stop("At least one selected index is not a supported fitmeasure in lavaan.")
      vf1 <-
        unname(sapply(lapply(fits.nna, function(x)
          x[1,]), function(x)
            x[index]))
      vf2 <-
        unname(sapply(lapply(fits.nna, function(x)
          x[2,]), function(x)
            x[index]))
      co1 <- rep(NA, length(index))
      co2 <- co1
      probs <-
        sapply(gof, function(x)
          ifelse(x, alpha.lev, 1 - alpha.lev))
      for (i in 1:length(index)) {
        #Type of stats::quantile is set to 8
        if (length(index) > 1) {
          co1[i] <-
            stats::quantile(vf1[i,], probs[i], type = 8)
          co2[i] <-
            stats::quantile(vf2[i,], probs[i], type = 8)
        }
        if (length(index) == 1) {
          co1[i] <-
            stats::quantile(vf1, probs, type = 8)
          co2[i] <-
            stats::quantile(vf2, probs, type = 8)
        }
      }
      co <- rbind(co1, co2)
      dimnames(co) <- list(c("Model 1", "Model 2"), toupper(index))
      names(gof) <- toupper(index)
      rco <-
        list(
          "cutoff" = co,
          "difference" = co[1,] - co[2,],
          "index" = toupper(index),
          "alpha" = alpha.lev,
          "gof" = gof,
          "replications" = length(fits),
          "number of non-converging models" = na,
          "share of non-converging models" = round(sh.na, 3)
        )
    }
    return(rco)
  }
#' Obtain recommendations based on Mai et al. (2021)
#'
#' This function recommends pre-defined selected fit indices in case the user does not know which fit index should be used for model evaluation. Results may differ based on three settings, the sample size of the data, the research purpose of the investigated model and the focus of the model. For obvious reasons, this function only works for single models and does not accept any other model type.
#' @param fits A list of simulated fit indices obtained from gen_fit. Based on the structure of fits, the number of models is derived.
#' @param purpose The research purpose of the model investigated. Is the underlying model novel (default) or established (= established). This parameter is relevant to find the proper recommended fit indices.
#' @param focus The focus of estimation for the model. Is the focus on CFA (default) or analyzing the structural model of a theoretical model (= structural)? This parameter is relevant to find the proper recommended fit indices.
#' @param override Should the recommendations by Mai et al. (2021) overridden (default: FALSE)?  This may be useful to explore models outside of the scope of the paper. In this case, the recommended fit indices are not determined by the function, and hence need to be provided.
#' In this case, the function requires the argument index.
#' @param index An optional vector of fit indices or measures provided by function fitmeasures in package lavaan. This argument is required when override is TRUE. It is ignored otherwise.
#' @param digits An optional integer to round fit values and cutoffs (min: 1, max: 5).
#' @return A list of information regarding the recommended fit indices based on Mai et al. (2021) or when overridden, based on the provided indices.
#' @references Mai, R., Niemand, T., & Kraus, S. (2021). A Tailor-Fit Model Evaluation Strategy for Better Decisions about Structural Equation Models, Technological Forecasting & Social Change (forthcoming).
#' @examples
#'mod <- "
#'F1 =~ Q5 + Q7 + Q8
#'F2 =~ Q2 + Q4
#'F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
#'F4 =~ Q1 + Q17
#'F5 =~ Q6 + Q14 + Q15 + Q16
#'"
#'fits.single <- gen_fit(mod1 = mod, x = bb1992, rep = 100, standardized = FALSE)
#'recommend(fits.single)
#'recommend(fits.single, purpose = "established")
#'recommend(fits.single,
#'          override = TRUE,
#'          index = c("CFI", "SRMR"))
#' @export
recommend <-
  function(fits,
           purpose = "novel",
           focus = "cfa",
           override = FALSE,
           index = NULL,
           digits = 3) {
    if (!is.null(dim(fits$fco[[1]])))
      stop(
        "Only single model fit indices are supported so far for this function. Please revise or use flex_co."
      )
    checkmate::assertCharacter(fits$mod1,
                               fixed = "=~")
    checkmate::assertDataFrame(
      fits$x,
      min.rows = 50,
      min.cols = 4,
      col.names = "unique"
    )
    if (purpose != "novel")
      purpose <- "established"
    checkmate::assert(
      checkmate::checkCharacter(purpose,
                                pattern = "novel"),
      checkmate::checkCharacter(purpose,
                                pattern = "established"),
    )
    if (focus != "cfa")
      focus <- "structure"
    checkmate::assert(
      checkmate::checkCharacter(focus,
                                pattern = "cfa"),
      checkmate::checkCharacter(focus,
                                pattern = "structure"),
    )
    checkmate::assert_int(digits, lower = 1, upper = 5)
    n <- nrow(fits$x)
    fm <-
      try(lavaan::fitmeasures(
        lavaan::cfa(
          fits$mod1,
          data = fits$x,
          estimator = "MLM",
          auto.fix.first = FALSE,
          std.lv = TRUE
        )
      ), silent = TRUE)
    if (inherits(fm, "try-error"))
      stop("Invalid model or data. Please revise.")
    if (!override) {
      if (purpose == "established" & focus == "cfa" & n <= 200) {
        #C1: SRMR flex
        index <- "srmr"
        ap <- c(.001, .01, .05, .10)
        fc <-
          suppressWarnings(sapply(ap, flex_co, fits = fits, index = index))
        co <- fc[1,]
        gof <- fc[4,]
        na <- fc[6, 1]
        sh.na <- fc[7, 1]
        rf <- fm[index]
      }
      if (purpose == "established" & focus == "cfa" & n > 200) {
        #C2: CFI fix
        index <- "cfi"
        ap <- NULL
        fc <- NULL
        na <- NULL
        sh.na <- NULL
        co <- .95
        gof <- index_guess(index)
        rf <- fm[index]
      }
      if (purpose == "established" &
          focus == "structure" & n <= 200) {
        #C3: SRMR flex
        index <- "srmr"
        ap <- c(.001, .01, .05, .10)
        fc <-
          suppressWarnings(sapply(ap, flex_co, fits = fits, index = index))
        co <- fc[1,]
        gof <- fc[4,]
        na <- fc[6, 1]
        sh.na <- fc[7, 1]
        rf <- fm[index]
      }
      if (purpose == "established" &
          focus == "structure" & n > 200) {
        #C4: SRMR flex
        index <- "srmr"
        ap <- c(.001, .01, .05, .10)
        fc <-
          suppressWarnings(sapply(ap, flex_co, fits = fits, index = index))
        co <- fc[1,]
        gof <- fc[4,]
        na <- fc[6, 1]
        sh.na <- fc[7, 1]
        rf <- fm[index]
      }
      if (purpose == "novel" & focus == "cfa" & n <= 200) {
        #C5: CFI fix
        index <- "cfi"
        ap <- NULL
        fc <- NULL
        na <- NULL
        sh.na <- NULL
        co <- .95
        gof <- index_guess(index)
        rf <- fm[index]
      }
      if (purpose == "novel" & focus == "cfa" & n > 200) {
        #C6: SRMR flex
        index <- "srmr"
        ap <- c(.001, .01, .05, .10)
        fc <-
          suppressWarnings(sapply(ap, flex_co, fits = fits, index = index))
        co <- fc[1,]
        gof <- fc[4,]
        na <- fc[6, 1]
        sh.na <- fc[7, 1]
        rf <- fm[index]
      }
      if (purpose == "novel" & focus == "structure" & n <= 200) {
        #C7: CFI & SRMR fix
        index <- c("cfi", "srmr")
        ap <- NULL
        fc <- NULL
        na <- NULL
        sh.na <- NULL
        co <- c(.95, .09)
        gof <- sapply(index, index_guess)
        rf <- fm[index]
      }
      if (purpose == "novel" & focus == "structure" & n > 200) {
        #C8: SRMR flex
        index <- "srmr"
        ap <- c(.001, .01, .05, .10)
        fc <-
          suppressWarnings(sapply(ap, flex_co, fits = fits, index = index))
        co <- fc[1,]
        gof <- fc[4,]
        na <- fc[6, 1]
        sh.na <- fc[7, 1]
        rf <- fm[index]
      }
    }
    if (override) {
      if (is.null(index))
        stop(
          "You chose to override the recommendation patterns. In this case, you need to provide index names. Please revise."
        )
      index <- tolower(index)
      ap <- c(.001, .01, .05, .10)
      fc <-
        suppressWarnings(sapply(ap, flex_co, fits = fits, index = index))
      co <- fc[1,]
      gof <- fc[4,]
      na <- fc[6, 1]
      sh.na <- fc[7, 1]
      rf <- fm[index]
    }
    if (!is.null(ap)) {
      tab <-
        data.frame(matrix(
          unlist(co),
          nrow = length(ap),
          ncol = length(index),
          byrow = TRUE
        ))
      names(tab) <- toupper(index)
      rownames(tab) <- ap
      gof <-
        data.frame(matrix(
          unlist(gof),
          nrow = length(ap),
          ncol = length(index),
          byrow = TRUE
        ))
      names(gof) <- toupper(index)
      rownames(gof) <- ap
      decs <- gof
      for (i in 1:length(index)) {
        decs[, i] <- rf[i] - tab[, i]
        decs[, i] <-
          ifelse(gof[, i] == TRUE, decs[, i], decs[, i] * -1)
        decs[, i] <- ifelse(decs[, i] > 0, "confirmed", "rejected")
      }
      rownames(decs) <- paste0("cutoff ", ap)
      rownames(tab) <- paste0("cutoff ", ap)
      gof <- data.frame(unname(sapply(index, index_guess)))
      gof <- cbind(gof, unname(rf))
      rownames(gof) <- toupper(index)
      names(gof) <- c("type", "fit.values")
      gof[, "fit.values"] <-
        round(gof[, "fit.values"], digits = digits)
      tab <- round(tab, digits = digits)
      if (fits$rep < 500)
        warning(
          "The number of replications is lower than the recommended minimum of 500. Consider with care."
        )
      res <- list(
        "recommended" = gof,
        "cutoffs" = tab,
        "decisions" = decs,
        "replications" = fits$rep,
        "comment" = ifelse(
          override,
          "Override mode",
          "Recommendations based on flexible cutoffs and Mai et al. (2021)"
        )
      )
    }
    if (is.null(ap)) {
      decs <- rf - co
      decs <- ifelse(gof == "GoF", decs, decs * -1)
      decs <- ifelse(decs > 0, "confirmed", "rejected")
      gof <- data.frame(unname(sapply(index, index_guess)))
      gof <- cbind(gof, unname(rf))
      rownames(gof) <- toupper(index)
      names(gof) <- c("type", "fit.values")
      gof[, "fit.values"] <-
        round(gof[, "fit.values"], digits = digits)
      res <- list(
        "recommended" = gof,
        "decisions" = decs,
        "comment" = "Recommendations based on fixed cutoffs and Mai et al. (2021)"
      )
    }
    return(res)
  }


#' Obtain recommendations for discriminant validity testing
#'
#' This function recommends on potential issues for discriminant validity testing, based on  differences between fit values and differences between flexible cutoffs. Two approaches of testing are supported: merging and constraining.
#' @param fits A list of simulated fit indices obtained from gen_fit. Based on the structure of fits, the number of models is derived.
#' @param index A vector of fit indices or measures provided by function fitmeasures in package lavaan. The default is set to CFI.
#' @param digits An optional integer to round fit values and cutoffs (min: 1, max: 5).
#' @return A list of information regarding discriminant validity testing.
#' @examples
#'mod <- "
#'F1 =~ Q5 + Q7 + Q8
#'F2 =~ Q2 + Q4
#'F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
#'F4 =~ Q1 + Q17
#'F5 =~ Q6 + Q14 + Q15 + Q16
#'"
#' #Two models for discriminant validity testing, this resembles constraining with a cutoff of .9
#'fits.dv.con <- gen_fit(
#'  mod1 = mod,
#'  x = bb1992,
#'  rep = 100,
#'  dv = TRUE,
#'  dv.factors = c("F4", "F5"),
#'  dv.cutoff = .9
#')
#'recommend_dv(fits.dv.con)
#' #Two models for discriminant validity testing, this resembles merging.
#'fits.dv.merge <- gen_fit(
#'  mod1 = mod,
#'  x = bb1992,
#'  rep = 100,
#'  dv = TRUE,
#'  dv.factors = c("F4", "F5"),
#'  merge.mod = TRUE
#')
#'recommend_dv(fits.dv.merge)
#' @export
recommend_dv <-
  function(fits,
           index = "CFI",
           digits = 3) {
    if (is.null(dim(fits$fco[[1]])))
      stop(
        "Only dual model fit indices are supported so far for this function. Please revise or use flex_co."
      )
    checkmate::assertCharacter(fits$mod1,
                               fixed = "=~")
    checkmate::assertDataFrame(
      fits$x,
      min.rows = 50,
      min.cols = 4,
      col.names = "unique"
    )
    checkmate::assertVector(fits$dv.factors, len = 2, null.ok = TRUE)
    checkmate::assertLogical(fits$merge.mod, null.ok = TRUE)
    checkmate::assertVector(fits$dv.cutoff, len = 1, null.ok = TRUE)
    checkmate::assert_int(digits, lower = 1, upper = 5)
    mode <- ifelse(fits$merge.mod, "merging", "constraining")
    fm <-
      try(lavaan::fitmeasures(
        lavaan::cfa(
          fits$mod1,
          data = fits$x,
          estimator = "MLM",
          auto.fix.first = FALSE,
          std.lv = TRUE
        )
      ), silent = TRUE)
    if (inherits(fm, "try-error"))
      stop("Invalid model or data. Please revise.")
    # if (!tolower(index) %in% names(fm))
    #   stop(
    #     "The index names provided do not match the names of the indices provided by lavaan. Please revise."
    #   )
    if (mode == "constraining") {
      mod2 <-
        constr_mod(fits$mod1,
                   dv.factors = fits$dv.factors,
                   dv.cutoff = fits$dv.cutoff)
      fm2 <-
        try(lavaan::fitmeasures(
          lavaan::cfa(
            fits$mod2,
            data = fits$x,
            estimator = "MLM",
            auto.fix.first = FALSE,
            std.lv = TRUE
          )
        ), silent = TRUE)
      if (inherits(fm2, "try-error"))
        stop("Invalid model or data. Please revise.")
    }
    if (mode == "merging") {
      pop.mod <- pop_mod(
        mod = fits$mod1,
        x = fits$x,
        type = fits$type,
        standardized = fits$standardized
      )$pop.mod
      pt2 <-
        try(merge_factors(lavaan::cfa(pop.mod, fits$x, warn = FALSE),
                          merged.factors = fits$dv.factors),
            silent = TRUE)
      if (inherits(pt2, "try-error"))
        stop("Merging two-factors not successful. Please check.")
      mod2 <- get_free(pt2, fits$dv.factors, mode)
      fm2 <-
        try(lavaan::fitmeasures(
          lavaan::cfa(
            mod2,
            data = fits$x,
            estimator = "MLM",
            auto.fix.first = FALSE,
            std.lv = TRUE
          )
        ), silent = TRUE)
      if (inherits(fm2, "try-error"))
        stop("Invalid model or data. Please revise.")
    }
    index <- tolower(index)
    ap <- c(.001, .01, .05, .10)
    #gof <- ifelse(sapply(index, index_guess) == "GoF", TRUE, FALSE)
    fc <-
      suppressWarnings(sapply(ap, flex_co, fits = fits, index = index))
    #fc <- suppressWarnings(sapply(ap, flex_co, fits = fits, index = index, gof = gof))
    #fc2 <- suppressWarnings(sapply(ap, flex_co, fits = fits, index = index, gof = !gof))
    #for (i in 1:length(ap)) {
    #  fc[1,][[i]][2, ] <- fc2[1,][[i]][2, ]
    #}
    co <- lapply(fc[1,], as.data.frame)
    gof <- fc[5,]
    na <- fc[7, 1]
    sh.na <- fc[8, 1]
    rf1 <- fm[tolower(index)]
    rf2 <- fm2[tolower(index)]
    tab <- data.table::rbindlist(co)
    mn <- ifelse(mode == "constraining", "constrained", "merged")
    tab <-
      as.data.frame(cbind(tab, expand.grid(c("original", mn), ap)))
    colnames(tab) <- c(toupper(index), "model", "alpha")
    fi <- data.frame(rbind(rf1, rf2))
    rownames(fi) <- NULL
    alpha <- NULL
    fi$model <- c("original", mn)
    colnames(fi)[1:length(index)] <- c(toupper(index))
    diff <-
      data.frame(matrix(NA, nrow = length(ap), ncol = length(index)))
    for (i in 1:length(ap)) {
      ss <- subset(tab, alpha == ap[i])[, 1:length(index)]
      if (length(index) == 1) {
        diff[i,] <- ss[1] - ss[2]
      }
      if (length(index) > 1) {
        diff[i, ] <- ss[1, ] - ss[2, ]
      }
    }
    names(diff) <- toupper(index)
    diff <-
      t(rbind(diff, fi[1, 1:length(index)] - fi[2, 1:length(index)]))
    colnames(diff) <- c(paste0("cutoff ", ap), "fit")
    decide.dv <- function(tab, fi, diff, index) {
      model <- NULL
      ctab <- subset(tab, model == "original")[, toupper(index)]
      cfi <- subset(fi, model == "original")[, toupper(index)]
      whr <- which(sapply(colnames(ctab), index_guess) == "BoF")
      if (!is.null(nrow(ctab))) {
        decs <- data.frame(matrix(NA, nrow = nrow(ctab), ncol = ncol(ctab)))
        for (i in 1:nrow(decs)) {
          #For GoF, that is the version from the flc paper
          decs[i, ] <- ifelse(cfi <= ctab[i, ], 1,-1)
        }
        #For BoF: reverse
        decs[, whr] <- decs[, whr] * -1
        decs[decs == 1] <- "confirmed"
        decs[decs == -1] <- "rejected"
      }
      if (is.null(nrow(ctab))) {
        #For GoF, that is the version from the flc paper
        decs <- ifelse(cfi <= ctab, 1,-1)
        #For BoF: reverse
        decs[whr] <- decs[whr] * -1
        decs[decs == 1] <- "confirmed"
        decs[decs == -1] <- "rejected"
      }
      return(decs)
    }
    # decide.dv <- function(diff) {
    #   whc <- which(colnames(diff) == "fit")
    #   whr <- which(sapply(rownames(diff), index_guess) == "BoF")
    #   decs <- data.frame(matrix(NA, nrow = nrow(diff), ncol = ncol(diff) - 1))
    #   for (i in 1:ncol(decs)) {
    #     #For GoF, that is the version from the flc paper
    #     decs[,i] <- ifelse(diff[,whc] >= diff[,i], 1, -1)
    #   }
    #   #For BoF: reverse
    #   decs[whr,] <- decs[whr,] * -1
    #   decs[decs == 1] <- "confirmed"
    #   decs[decs == -1] <- "rejected"
    #   return(decs)
    # }
    #decs <- decide.dv(diff)
    decs <- decide.dv(tab, fi, diff, index)
    #decs <- data.frame(t(decs))
    diff <- data.frame(t(diff))
    decs <- data.frame(decs)
    names(decs) <- colnames(diff)
    rownames(decs) <- rownames(diff)[1:length(ap)]
    tab[, toupper(index)] <- round(tab[, toupper(index)], digits)
    fi[, toupper(index)] <- round(fi[, toupper(index)], digits)
    diff[, toupper(index)] <- round(diff[, toupper(index)], digits)
    if (fits$rep < 500)
      warning(
        "The number of replications is lower than the recommended minimum of 500. Consider with care."
      )
    res <- list(
      "cutoffs" = tab,
      "fit.values" = fi,
      "differences" = diff,
      "decisions" = decs,
      "replications" = fits$rep,
      "comment" = paste0(
        "Approach for discriminant validity testing: ",
        mode,
        ". Discriminant validity is confirmed if the fit value from the constrained/merged model is smaller (GoF) / larger (BoF) than the respective cutoff of original model."
      )
    )
    return(res)
  }
