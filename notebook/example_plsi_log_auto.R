## No test: 
data(nhanes.new)
data <- nhanes.new

X.name <- c("X1_trans.b.carotene", "X2_retinol", "X3_g.tocopherol", "X4_a.tocopherol",
            "X5_PCB99", "X6_PCB156", "X7_PCB206",
            "X8_3.3.4.4.5.pncb", "X9_1.2.3.4.7.8.hxcdf", "X10_2.3.4.6.7.8.hxcdf")
Z.name <- c("AGE.c", "SEX.Female", "RACE.NH.Black",
           "RACE.MexicanAmerican", "RACE.OtherRace", "RACE.Hispanic" )

# demo count outcome (illustrative only -- nhanes.new has no native count
# variable). Simulated from a *true* single-index combination of the
# exposures, run through a nonlinear log-rate link, plus a confounder
# effect -- so the example actually has an exposure-outcome relationship
# for plsi.log.auto() to recover, rather than depending on the confounder
# alone.
set.seed(2026)
beta_true <- c(0.30, -0.20, 0.10, 0.40, -0.30, 0.20, -0.10, 0.25, -0.15, 0.35)
beta_true <- beta_true / sqrt(sum(beta_true^2))         # unit norm, matching
                                                         # the model's own
                                                         # identifiability constraint
x_std <- scale(data[, X.name])                          # standardize so the
                                                         # demo index is on a
                                                         # sane, comparable scale
single_index_true <- as.vector(x_std %*% beta_true)
log_rate <- 0.3 + 0.4 * sin(single_index_true) + 0.05 * data$AGE.c
data$n.events <- stats::rpois(nrow(data), lambda = exp(log_rate))

Y.name <- "n.events"

k <- 10
bs <- "cr"
initial.random.num <- 1
seed <- 2026

model_log_auto <- plsi.log.auto(data = data, Y.name = Y.name, X.name = X.name,
                      Z.name = Z.name, family = "nb", k = k, bs = bs,
                      initial.random.num = initial.random.num, seed = seed)
## End(No test)



