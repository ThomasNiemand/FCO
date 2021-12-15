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
  for (i in seq_len(length(vars))) {
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
