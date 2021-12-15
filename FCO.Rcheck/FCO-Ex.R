pkgname <- "FCO"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "FCO-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('FCO')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("bb1992")
### * bb1992

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: bb1992
### Title: Dataset from Babakus & Boller (1992)
### Aliases: bb1992
### Keywords: datasets

### ** Examples

data(bb1992)
head(bb1992, 3)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("bb1992", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("flex_co")
### * flex_co

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: flex_co
### Title: Obtain flexible cutoffs for one or two models
### Aliases: flex_co

### ** Examples

#A single model to obtain fit indices for
mod <- "
F1 =~ Q5 + Q7 + Q8
F2 =~ Q2 + Q4
F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
F4 =~ Q1 + Q17
F5 =~ Q6 + Q14 + Q15 + Q16
"
fits.single <- gen_fit(mod1 = mod, x = bb1992, rep = 100, standardized = FALSE)
flex_co(fits = fits.single, index = c("CFI", "SRMR"))
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
 rep = 100
)
flex_co(fits = fits.con,
       index = c("CFI", "SRMR"),
       alpha.lev = .05)

#Two models for discriminant validity testing, this resembles constraining with a cutoff of .9
fits.dv.con <- gen_fit(
 mod1 = mod,
 x = bb1992,
 rep = 100,
 dv = TRUE,
 dv.factors = c("F4", "F5"),
 dv.cutoff = .9
)
flex_co(fits = fits.dv.con,
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
lavaan::fitmeasures(
 lavaan::cfa(
   model = mod.dv.con,
   data = bb1992,
   auto.fix.first = FALSE,
   std.lv = TRUE
 ),
 fit.measures = "cfi"
)
#Two models for discriminant validity testing, this resembles merging.
fits.dv.merge <- gen_fit(
 mod1 = mod,
 x = bb1992,
 rep = 100,
 dv = TRUE,
 dv.factors = c("F4", "F5"),
 merge.mod = TRUE
)
flex_co(fits = fits.dv.merge,
index = "CFI",
alpha.lev = .05)
mod.dv.merge <- "
F1 =~ Q5 + Q7 + Q8
F2 =~ Q2 + Q4
F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
F4 =~ Q1 + Q17 + Q6 + Q14 + Q15 + Q16
"
lavaan::fitmeasures(
 lavaan::cfa(
   model = mod.dv.merge,
   data = bb1992
 ),
 fit.measures = "cfi"
)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("flex_co", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gen_fit")
### * gen_fit

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gen_fit
### Title: Obtain fit statistics from one or two models
### Aliases: gen_fit

### ** Examples


#A single model to obtain fit indices for
mod <- "
F1 =~ Q5 + Q7 + Q8
F2 =~ Q2 + Q4
F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
F4 =~ Q1 + Q17
F5 =~ Q6 + Q14 + Q15 + Q16
"
fits.single <- gen_fit(mod1 = mod, x = bb1992, rep = 100, standardized = FALSE)

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
 rep = 100
)
#Two models for discriminant validity testing, this resembles constraining with a cutoff of .9
fits.dv.con <- gen_fit(
 mod1 = mod,
 x = bb1992,
 rep = 100,
 dv = TRUE,
 dv.factors = c("F4", "F5"),
 dv.cutoff = .9
)

#Two models for discriminant validity testing, this resembles merging.
fits.dv.merge <- gen_fit(
 mod1 = mod,
 x = bb1992,
 rep = 100,
 dv = TRUE,
 dv.factors = c("F4", "F5"),
 merge.mod = TRUE
)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gen_fit", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("index_guess")
### * index_guess

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: index_guess
### Title: Helper function that guesses GoF or BoF from a given index name
### Aliases: index_guess

### ** Examples

index_guess("cfi")
index_guess("tli")
index_guess("rmsea")
index_guess("srmr")
index_guess("cfi")
index_guess("tli")
index_guess("rmsea")
index_guess("srmr")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("index_guess", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pop_mod")
### * pop_mod

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pop_mod
### Title: Helper function to obtain population model for simulation based
###   on data and model
### Aliases: pop_mod

### ** Examples

mod <- "
F1 =~ Q5 + Q7 + Q8
F2 =~ Q2 + Q4
F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
F4 =~ Q1 + Q17
F5 =~ Q6 + Q14 + Q15 + Q16
"
pop_mod(mod, x = bb1992, type = "NM")$pop.mod
pop_mod(mod, x = bb1992, type = "HB")$pop.mod
pop_mod(mod, x = bb1992, type = "EM")$pop.mod
pop_mod(mod, x = bb1992, type = "NM", afl = .9)$pop.mod
pop_mod(mod, x = bb1992, type = "NM", aco = .5)$pop.mod
pop_mod(mod, x = bb1992, type = "EM", standardized = FALSE)$pop.mod



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pop_mod", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("recommend")
### * recommend

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: recommend
### Title: Obtain recommendations based on Mai et al. (2021)
### Aliases: recommend

### ** Examples

mod <- "
F1 =~ Q5 + Q7 + Q8
F2 =~ Q2 + Q4
F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
F4 =~ Q1 + Q17
F5 =~ Q6 + Q14 + Q15 + Q16
"
fits.single <- gen_fit(mod1 = mod, x = bb1992, rep = 100, standardized = FALSE)
recommend(fits.single)
recommend(fits.single, purpose = "established")
recommend(fits.single,
         override = TRUE,
         index = c("CFI", "SRMR"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("recommend", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("recommend_dv")
### * recommend_dv

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: recommend_dv
### Title: Obtain recommendations for discriminant validity testing
### Aliases: recommend_dv

### ** Examples

mod <- "
F1 =~ Q5 + Q7 + Q8
F2 =~ Q2 + Q4
F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
F4 =~ Q1 + Q17
F5 =~ Q6 + Q14 + Q15 + Q16
"
#Two models for discriminant validity testing, this resembles constraining with a cutoff of .9
fits.dv.con <- gen_fit(
 mod1 = mod,
 x = bb1992,
 rep = 100,
 dv = TRUE,
 dv.factors = c("F4", "F5"),
 dv.cutoff = .9
)
recommend_dv(fits.dv.con)
#Two models for discriminant validity testing, this resembles merging.
fits.dv.merge <- gen_fit(
 mod1 = mod,
 x = bb1992,
 rep = 100,
 dv = TRUE,
 dv.factors = c("F4", "F5"),
 merge.mod = TRUE
)
recommend_dv(fits.dv.merge)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("recommend_dv", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
