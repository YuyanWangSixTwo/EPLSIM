knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(EPLSIM)

dat.cov <- data.frame(
  age = c(1.5, 2.3, 3.1, 4.8, 5.2),
  sex = c(1, 2, 1, 2, 2),
  race = c(1, 2, 3, 4, 5)
)

# specify the confounder vector
Z.name <- c("age", "sex", "race")

# set levels and make the reference level first for categorical confounders
dat.cov$sex <- factor(dat.cov$sex, 1:2, c('Male', 'Female'))
dat.cov$race <- factor(dat.cov$race, 1:5, c("NH-White", "NH-Black",
                                             "MexicanAmerican", "OtherRace", "Hispanic"))

# transform the confounder vector and check
cov_m <- confounder.trans(Z_continuous = c("age"), Z_discrete = c("sex", "race"), data = dat.cov)
Z.name <- cov_m$New.Name
dat.cov <- cov_m$Updated.data
print(Z.name)

data(nhanes.new)
dat <- nhanes.new

# outcome
Y.name <- "log.triglyceride"

# exposures making up the single index
X.name <- c("X1_trans.b.carotene", "X2_retinol", "X3_g.tocopherol", "X4_a.tocopherol",
            "X5_PCB99", "X6_PCB156", "X7_PCB206",
            "X8_3.3.4.4.5.pncb", "X9_1.2.3.4.7.8.hxcdf", "X10_2.3.4.6.7.8.hxcdf")

# confounders, already prepared as in the section above
Z.name <- c("AGE.c", "SEX.Female", "RACE.NH.Black",
            "RACE.MexicanAmerican", "RACE.OtherRace", "RACE.Hispanic")

model_auto <- plsi.lr.auto(data = dat, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
                            k = 10, bs = "cr", initial.random.num = 1, seed = 2026)

# effective degrees of freedom REML actually used for the link function
# (will be <= k; substantially less than k indicates the relationship is
# close to linear)
model_auto$si.fun.edf

# single-index coefficients: which exposures drive the index, and how much
model_auto$si.coefficient

# confounder coefficients
model_auto$confounder.coefficient

model_v2 <- plsi.lr.v2(data = dat, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
                        spline.num = 5, spline.degree = 3, initial.random.num = 1, seed = 2023)

model_v2$confounder.coefficient

si.coef.plot(model_auto$si.coefficient)

si.fun.plot(model_auto$si.fun, type = "linear")

e.main.plot(model_auto, dat, exp_name = "X4_a.tocopherol", type = "linear")
e.main.plot(model_auto, dat, exp_name = "X5_PCB99", type = "linear")
e.main.plot(model_auto, dat, exp_name = "X10_2.3.4.6.7.8.hxcdf", type = "linear")

e.interaction.plot(model_auto, dat, "X4_a.tocopherol", "X3_g.tocopherol", type = "linear")

e.interaction.plot(model_auto, dat, "X3_g.tocopherol", "X4_a.tocopherol", type = "linear")

interquartile.quartile.plot(model_auto, dat, type = "linear")

mixture.overall.plot(model_auto, dat, type = "linear")

# model_logistic <- plsi.logistic.auto(data = dat, Y.name = "some_binary_outcome",
#                                       X.name = X.name, Z.name = Z.name,
#                                       k = 10, bs = "cr", initial.random.num = 1, seed = 2026)
# 
# si.fun.plot(model_logistic$si.fun, type = "logistic")
# e.main.plot(model_logistic, dat, exp_name = "X4_a.tocopherol", type = "logistic")
