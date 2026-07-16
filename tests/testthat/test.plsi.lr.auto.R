context('plsi.lr.auto -  PLSI linear regression with automatic smoothness selection')

data(nhanes.new)
dat <- nhanes.new
Y.name <- "log.triglyceride"
X.name <- c("X1_trans.b.carotene", "X2_retinol", "X3_g.tocopherol", "X4_a.tocopherol",
            "X5_PCB99", "X6_PCB156", "X7_PCB206",
            "X8_3.3.4.4.5.pncb", "X9_1.2.3.4.7.8.hxcdf", "X10_2.3.4.6.7.8.hxcdf")
Z.name <- c("AGE.c", "SEX.Female", "RACE.NH.Black",
            "RACE.MexicanAmerican", "RACE.OtherRace", "RACE.Hispanic")
k <- 5           # small k keeps the test fast; production use can go higher
bs <- "cr"
initial.random.num <- 1


# k = 5 / initial.random.num = 1 are deliberately small to keep this test
# file fast; at these settings the numerically-differentiated Hessian
# occasionally isn't positive definite, which triggers plsi.lr.auto()'s
# eigenvalue-flooring fallback (see plsi_lr_auto.R) and an accompanying
# warning. That fallback is the intended, tested behavior -- not a
# regression -- so it's suppressed here rather than left to print as
# unexplained noise on every test run.
model_auto <- suppressWarnings(
  plsi.lr.auto(data = dat, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
               k = k, bs = bs, initial.random.num = initial.random.num, seed = 2026)
)

test_that('Output of plsi.lr.auto', {
  expect_true(is.list(model_auto))
  expect_true(is.data.frame(model_auto$si.coefficient))
  expect_true(is.data.frame(model_auto$confounder.coefficient))
  expect_true(is.data.frame(model_auto$si.fun))
  expect_equal(nrow(model_auto$si.coefficient), ncol(model_auto$original.data$x))
  expect_equal(nrow(model_auto$confounder.coefficient), ncol(model_auto$original.data$z))
})

test_that('plsi.lr.auto uses REML smoothness selection and reports effective df', {
  expect_true(inherits(model_auto$si.fun.model, "gam"))
  expect_true(inherits(model_auto$full.model, "gam"))
  # effective df actually used should never exceed the ceiling k
  expect_true(model_auto$si.fun.edf <= k)
  expect_true(model_auto$si.fun.edf > 0)
})

test_that('plsi.lr.auto si.fun.model predicts from single_index_estimated alone', {
  # this is the confounder-free model -- predict() must NOT require Z.name columns
  newdat <- data.frame(single_index_estimated = 0)
  expect_error(
    stats::predict(model_auto$si.fun.model, newdata = newdat, type = "link"),
    NA
  )
})

test_that('plsi.lr.auto si.fun has columns required by si.fun.plot(type = "linear")', {
  expect_true(all(c("single_index_estimated", "fit", "lwr", "upr")
                  %in% colnames(model_auto$si.fun)))
})

test_that('plsi.lr.auto rejects missing mgcv gracefully is not testable here; instead check input validation', {
  expect_error(
    plsi.lr.auto(data = dat, Y.name = "not.a.real.column", X.name = X.name, Z.name = Z.name,
                 k = k, bs = bs, initial.random.num = initial.random.num, seed = 2026),
    "not found in data"
  )
})

test_that('plsi.lr.auto is reproducible given the same seed', {
  model_auto_repeat <- suppressWarnings(
    plsi.lr.auto(data = dat, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
                 k = k, bs = bs, initial.random.num = initial.random.num, seed = 2026)
  )
  expect_equal(model_auto$si.coefficient$Estimate, model_auto_repeat$si.coefficient$Estimate,
               tolerance = 1e-6)
})
