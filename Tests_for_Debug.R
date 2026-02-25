library(lavaan)
library(dplyr)
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
             speed   =~ x7 + x8 + x9 '

fit <- cfa(
  HS.model,
  data = HolzingerSwineford1939
)
#Note: Demonstration only! Please use higher numbers of replications for your applications (>= 500).
fits <- gen_fit2(fit = fit, rep = 100)
flex_co2(fits)
plot_fit2(fits)

#Different data types
dat <- lavaan::HolzingerSwineford1939
cdat <- dplyr::select(dat, x1, x2, x3, x4, x5, x6, x7, x8, x9)
#For demo purposes, some variables are changed:
cdat <- cdat %>% mutate(
  x1 = round(x1, digits = 0),
  x3 = round(x3, digits = 0),
  x2 = ifelse(x2 > 4, 1, 0),
  x4 = ifelse(x4 > 2, 1, 0),
  x5 = round(x5, digits = 0),
  x8 = round(x8, digits = 0)
)
cfit <- cfa(model = HS.model, data = cdat)
#Note: Demonstration only! Please use higher numbers of replications for your applications (>= 500).
cfits <- gen_fit2(fit = cfit,
                  data.types = c("C", "B", "C", "B", "O", "N", "N", "O", "N"), rep = 100)
flex_co2(cfits)
plot_fit2(cfits)

#Multiple fit indices
flex_co2(fits, index = c("cfi", "SRMR", "RMSEA"))
plot_fit2(fits, index = c("cfi", "SRMR", "RMSEA"))