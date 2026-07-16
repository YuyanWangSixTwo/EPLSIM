context('plsi.log.auto -  PLSI log-linear regression with automatic smoothness selection')

data(nhanes.new)
dat <- nhanes.new
X.name <- c("X1_trans.b.carotene", "X2_retinol", "X3_g.tocopherol", "X4_a.tocopherol",
            "X5_PCB99", "X6_PCB156", "X7_PCB206",
            "X8_3.3.4.4.5.pncb", "X9_1.2.3.4.7.8.hxcdf", "X10_2.3.4.6.7.8.hxcdf")
Z.name <- c("AGE.c", "SEX.Female", "RACE.NH.Black",
            "RACE.MexicanAmerican", "RACE.OtherRace", "RACE.Hispanic")

# Simulated count outcome from a true single-index combination of the
# exposures (nhanes.new has no native count variable) -- mirrors the
# @examples block in plsi_log_auto.R.
set.seed(2026)
beta_true <- c(0.30, -0.20, 0.10, 0.40, -0.30, 0.20, -0.10, 0.25, -0.15, 0.35)
beta_true <- beta_true / sqrt(sum(beta_true^2))
x_std <- scale(dat[, X.name])
single_index_true <- as.vector(x_std %*% beta_true)
log_rate <- 0.3 + 0.4 * sin(single_index_true) + 0.05 * dat$AGE.c
dat$n.events <- stats::rpois(nrow(dat), lambda = exp(log_rate))
Y.name <- "n.events"

k <- 5
bs <- "cr"
initial.random.num <- 1

model_log_nb <- plsi.log.auto(data = dat, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
                              family = "nb", k = k, bs = bs,
                              initial.random.num = initial.random.num, seed = 2026)

test_that('Output of plsi.log.auto (negative binomial, default)', {
  expect_true(is.list(model_log_nb))
  expect_true(is.data.frame(model_log_nb$si.coefficient))
  expect_true(is.data.frame(model_log_nb$confounder.coefficient))
  expect_true(is.data.frame(model_log_nb$si.fun))
  expect_equal(nrow(model_log_nb$si.coefficient), ncol(model_log_nb$original.data$x))
  expect_equal(model_log_nb$original.par$family, "nb")
})

test_that('plsi.log.auto also fits under family = "poisson"', {
  model_log_pois <- plsi.log.auto(data = dat, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
                                  family = "poisson", k = k, bs = bs,
                                  initial.random.num = initial.random.num, seed = 2026)
  expect_true(is.list(model_log_pois))
  expect_equal(model_log_pois$original.par$family, "poisson")
})

test_that('plsi.log.auto rejects negative or non-integer counts', {
  dat_bad <- dat
  dat_bad$n.events[1] <- -1
  expect_error(
    plsi.log.auto(data = dat_bad, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
                  family = "nb", k = k, bs = bs, initial.random.num = initial.random.num, seed = 2026),
    "non-negative integer counts"
  )
  dat_bad2 <- dat
  dat_bad2$n.events[1] <- 1.5
  expect_error(
    plsi.log.auto(data = dat_bad2, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
                  family = "nb", k = k, bs = bs, initial.random.num = initial.random.num, seed = 2026),
    "non-negative integer counts"
  )
})

test_that('plsi.log.auto si.fun stays within valid count bounds', {
  expect_true(all(c("fit", "lwr", "upr", "count.fit", "count.lwr", "count.upr")
                  %in% colnames(model_log_nb$si.fun)))
  expect_true(all(model_log_nb$si.fun$count.fit >= 0))
  expect_true(all(model_log_nb$si.fun$count.lwr >= 0))
})

test_that('plsi.log.auto confounder.coefficient reports rate ratios consistently', {
  cc <- model_log_nb$confounder.coefficient
  expect_true(all(c("Estimate", "Rate Ratio", "RR.Lower.95CI", "RR.Upper.95CI") %in% colnames(cc)))
  expect_equal(cc$`Rate Ratio`, exp(cc$Estimate), tolerance = 1e-8)
})
