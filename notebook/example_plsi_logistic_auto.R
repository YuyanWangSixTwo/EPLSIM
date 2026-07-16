## No test: 
data(nhanes.new)
data <- nhanes.new

X.name <- c("X1_trans.b.carotene", "X2_retinol", "X3_g.tocopherol", "X4_a.tocopherol",
            "X5_PCB99", "X6_PCB156", "X7_PCB206",
            "X8_3.3.4.4.5.pncb", "X9_1.2.3.4.7.8.hxcdf", "X10_2.3.4.6.7.8.hxcdf")
Z.name <- c("AGE.c", "SEX.Female", "RACE.NH.Black",
           "RACE.MexicanAmerican", "RACE.OtherRace", "RACE.Hispanic" )

# demo binary outcome (illustrative only -- nhanes.new has no native binary
# variable). Simulated from a *true* single-index combination of the
# exposures, run through a nonlinear logit link, plus a confounder effect,
# and drawn as genuine Bernoulli noise (not a hard threshold on an existing
# variable) -- a hard threshold produces near-perfect separation and
# destabilizes the fit, whereas real classification uncertainty is both
# more realistic and numerically well-behaved.
set.seed(2026)
beta_true <- c(0.30, -0.20, 0.10, 0.40, -0.30, 0.20, -0.10, 0.25, -0.15, 0.35)
beta_true <- beta_true / sqrt(sum(beta_true^2))
x_std <- scale(data[, X.name])
single_index_true <- as.vector(x_std %*% beta_true)
log_odds <- -0.2 + 0.5 * sin(single_index_true) + 0.05 * data$AGE.c
data$high.triglyceride <- stats::rbinom(nrow(data), size = 1, prob = stats::plogis(log_odds))

Y.name <- "high.triglyceride"

k <- 10
bs <- "cr"
initial.random.num <- 1
seed <- 2026

model_logistic_auto <- plsi.logistic.auto(data = data, Y.name = Y.name, X.name = X.name,
                      Z.name = Z.name, k = k, bs = bs,
                      initial.random.num = initial.random.num, seed = seed)
## End(No test)




