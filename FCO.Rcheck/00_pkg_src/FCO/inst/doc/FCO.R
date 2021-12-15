## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(lavaan)
library(MASS)
library(ggplot2)

## ----install------------------------------------------------------------------
#install.packages("FCO")
#Add later when published on CRAN

## ----setup--------------------------------------------------------------------
library(FCO)

## ----data---------------------------------------------------------------------
data(bb1992)
head(bb1992, 3)

## ----model--------------------------------------------------------------------
mod <- "
F1 =~ Q5 + Q7 + Q8
F2 =~ Q2 + Q4
F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
F4 =~ Q1 + Q17
F5 =~ Q6 + Q14 + Q15 + Q16
"

## ----empirical fit------------------------------------------------------------
res <- cfa(mod, data = bb1992)
fitmeasures(res, fit.measures = c("CFI", "SRMR"))

## ----generating cutoffs-------------------------------------------------------
fits.single <- gen_fit(mod1 = mod, x = bb1992, rep = 100)
flex_co(fits = fits.single, index = c("CFI", "SRMR"))

## ----alternative alphas-------------------------------------------------------
flex_co(fits = fits.single,
        index = c("CFI", "SRMR"),
        alpha.lev = .001)
flex_co(fits = fits.single,
        index = c("CFI", "SRMR"),
        alpha.lev = .10)

## ----population models--------------------------------------------------------
pop_mod(mod, x = bb1992, type = "NM")$pop.mod
pop_mod(mod, x = bb1992, type = "HB")$pop.mod
pop_mod(mod, x = bb1992, type = "EM")$pop.mod

## ----different options of population models-----------------------------------
pop_mod(mod, x = bb1992, type = "NM", afl = .9)$pop.mod
pop_mod(mod, x = bb1992, type = "NM", aco = .5)$pop.mod
pop_mod(mod, x = bb1992, type = "EM", standardized = FALSE)$pop.mod

## ----index guessing-----------------------------------------------------------
index_guess("cfi")
index_guess("CFI")
index_guess("srmr")
index_guess("SRMR")
index_guess("mickey_mouse")

## ----wrong gof----------------------------------------------------------------
flex_co(
  fits = fits.single,
  index = c("CFI", "SRMR"),
  gof = c(TRUE, FALSE)
)
flex_co(
  fits = fits.single,
  index = c("CFI", "SRMR"),
  gof = c(FALSE, TRUE)
)

## ----visual srmr--------------------------------------------------------------
fits.1000 <- gen_fit(mod1 = mod, x = bb1992, rep = 1000)
srmr <- sapply(fits.1000$fco, function(x) unname(x[c("srmr")]))
#Note that SRMR is a gof, so we look for the right side
qu <- quantile(srmr, probs = 1 - c(.001, .01, .05, .100), type = 8)
print(qu)

ggplot(data.frame(srmr), aes(x = srmr)) +
  geom_histogram() +
  scale_color_grey() +
  theme_classic() +
  theme(legend.position = "none") +
  geom_density(alpha = .2, fill = "blue") +
  geom_vline(
    xintercept = qu[1],
    linetype = "dotted",
    color = "darkgreen",
    size = 1.0
  ) +
  geom_vline(
    xintercept = qu[2],
    linetype = "dashed",
    color = "orange",
    size = 1.0
  ) +
  geom_vline(
    xintercept = qu[3],
    linetype = "solid",
    color = "red",
    size = 1.0
  )

## ----equals two cores---------------------------------------------------------
system.time(gen_fit(mod1 = mod, x = bb1992, rep = 100))

## ----multi.core switched off--------------------------------------------------
system.time(gen_fit(
  mod1 = mod,
  x = bb1992,
  rep = 100,
  multi.core = FALSE
))

## ----first recommendation-----------------------------------------------------
recommend(fits.single)

## ----second recommendation----------------------------------------------------
recommend(fits.single, purpose = "established")

## ----override recommendation--------------------------------------------------
recommend(fits.single,
          override = TRUE,
          index = c("CFI", "SRMR"))

## ----constrained model comparison---------------------------------------------
subset(parameterestimates(res, standardized = T), lhs == "F1" &
         rhs == "F2")
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
  rep = 100
)
flex_co(fits = fits.con,
        index = c("CFI", "SRMR"),
        alpha.lev = .05)
fitmeasures(res, fit.measures = c("cfi", "srmr")) - fitmeasures(cfa(model = mod.con, data = bb1992), fit.measures = c("cfi", "srmr"))

## ----proof of equal cutoffs when type EM is used------------------------------
fits.proof <- gen_fit(
  mod1 = mod,
  x = bb1992,
  rep = 100,
  type = "EM"
)
flex_co(fits = fits.proof,
        index = c("CFI", "SRMR"),
        alpha.lev = .05)

## ----constraining and merging in discriminant validity testing----------------
subset(parameterestimates(res, standardized = TRUE), lhs == "F4" &
         rhs == "F5")
fits.dv.con <- gen_fit(
  mod1 = mod,
  x = bb1992,
  rep = 100,
  dv = TRUE,
  dv.factors = c("F4", "F5"),
  dv.cutoff = .9
)
fits.dv.merge <- gen_fit(
  mod1 = mod,
  x = bb1992,
  rep = 100,
  dv = TRUE,
  dv.factors = c("F4", "F5"),
  merge.mod = TRUE
)

flex_co(fits = fits.dv.con,
        index = "CFI",
        alpha.lev = .05)
flex_co(fits = fits.dv.merge,
        index = "CFI",
        alpha.lev = .05)

mod.dv.con <- "
F1 =~ Q5 + Q7 + Q8
F2 =~ Q2 + Q4
F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
F4 =~ Q1 + Q17
F5 =~ Q6 + Q14 + Q15 + Q16
F4 ~~ .9 * F5
"
fitmeasures(
  cfa(
    model = mod.dv.con,
    data = bb1992,
    auto.fix.first = FALSE,
    std.lv = TRUE
  ),
  fit.measures = "cfi"
)

mod.dv.merge <- "
F1 =~ Q5 + Q7 + Q8
F2 =~ Q2 + Q4
F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
F4 =~ Q1 + Q17 + Q6 + Q14 + Q15 + Q16
"
fitmeasures(
  cfa(
    model = mod.dv.merge,
    data = bb1992
  ),
  fit.measures = "cfi"
)

## ----recommend function for discriminant validity-----------------------------
recommend_dv(fits.dv.con)
recommend_dv(fits.dv.merge)

