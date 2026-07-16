context('plsi.logistic.auto -  PLSI logistic regression with automatic smoothness selection')

data(nhanes.new)
dat <- nhanes.new
X.name <- c("X1_trans.b.carotene", "X2_retinol", "X3_g.tocopherol", "X4_a.tocopherol",
            "X5_PCB99", "X6_PCB156", "X7_PCB206",
            "X8_3.3.4.4.5.pncb", "X9_1.2.3.4.7.8.hxcdf", "X10_2.3.4.6.7.8.hxcdf")
Z.name <- c("AGE.c", "SEX.Female", "RACE.NH.Black",
            "RACE.MexicanAmerican", "RACE.OtherRace", "RACE.Hispanic")

# Simulated binary outcome from a true single-index combination of the
# exposures (nhanes.new has no native binary variable) -- mirrors the
# @examples block in plsi_logistic_auto.R so the test exercises the same
# code path documented for users.
set.seed(2026)
beta_true <- c(0.30, -0.20, 0.10, 0.40, -0.30, 0.20, -0.10, 0.25, -0.15, 0.35)
beta_true <- beta_true / sqrt(sum(beta_true^2))
x_std <- scale(dat[, X.name])
single_index_true <- as.vector(x_std %*% beta_true)
log_odds <- -0.2 + 0.5 * sin(single_index_true) + 0.05 * dat$AGE.c
dat$high.triglyceride <- stats::rbinom(nrow(dat), size = 1, prob = stats::plogis(log_odds))
Y.name <- "high.triglyceride"

k <- 5
bs <- "cr"
initial.random.num <- 1

model_logistic <- plsi.logistic.auto(data = dat, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
                                     k = k, bs = bs, initial.random.num = initial.random.num, seed = 2026)

test_that('Output of plsi.logistic.auto', {
  expect_true(is.list(model_logistic))
  expect_true(is.data.frame(model_logistic$si.coefficient))
  expect_true(is.data.frame(model_logistic$confounder.coefficient))
  expect_true(is.data.frame(model_logistic$si.fun))
  expect_equal(nrow(model_logistic$si.coefficient), ncol(model_logistic$original.data$x))
})

test_that('plsi.logistic.auto coerces a 2-level factor outcome to 0/1', {
  dat_factor <- dat
  dat_factor$high.triglyceride <- factor(dat_factor$high.triglyceride, labels = c("no", "yes"))
  expect_message(
    model_factor <- plsi.logistic.auto(data = dat_factor, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
                                       k = k, bs = bs, initial.random.num = initial.random.num, seed = 2026),
    "coercing to 0/1"
  )
  expect_true(is.list(model_factor))
})

test_that('plsi.logistic.auto rejects non-binary outcomes', {
  dat_bad <- dat
  dat_bad$high.triglyceride <- dat_bad$high.triglyceride + 1  # now coded 1/2, not 0/1
  expect_error(
    plsi.logistic.auto(data = dat_bad, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
                       k = k, bs = bs, initial.random.num = initial.random.num, seed = 2026),
    "must be coded 0/1"
  )
})

test_that('plsi.logistic.auto si.fun stays within valid probability bounds', {
  expect_true(all(c("fit", "lwr", "upr", "prob.fit", "prob.lwr", "prob.upr")
                  %in% colnames(model_logistic$si.fun)))
  expect_true(all(model_logistic$si.fun$prob.fit >= 0 & model_logistic$si.fun$prob.fit <= 1))
  expect_true(all(model_logistic$si.fun$prob.lwr >= 0 & model_logistic$si.fun$prob.lwr <= 1))
  expect_true(all(model_logistic$si.fun$prob.upr >= 0 & model_logistic$si.fun$prob.upr <= 1))
})

test_that('plsi.logistic.auto confounder.coefficient reports odds ratios consistently', {
  cc <- model_logistic$confounder.coefficient
  expect_true(all(c("Estimate", "Odds Ratio", "OR.Lower.95CI", "OR.Upper.95CI") %in% colnames(cc)))
  expect_equal(cc$`Odds Ratio`, exp(cc$Estimate), tolerance = 1e-8)
})

test_that('plsi.logistic.auto si.fun.model predicts without needing original confounders', {
  # confounder-free, offset-based model -- offset = 0 is the documented convention
  newdat <- data.frame(single_index_estimated = 0, confounder_offset = 0)
  expect_error(
    stats::predict(model_logistic$si.fun.model, newdata = newdat, type = "link"),
    NA
  )
})
