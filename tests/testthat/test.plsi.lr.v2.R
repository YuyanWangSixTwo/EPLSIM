context('plsi.lr.v2 -  PLSI linear regression version 2')
testthat::skip_on_cran()
data(nhanes.new)
dat <- nhanes.new
Y.name <- "log.triglyceride"
X.name <- c("X1_trans.b.carotene", "X2_retinol", "X3_g.tocopherol", "X4_a.tocopherol",
            "X5_PCB99", "X6_PCB156", "X7_PCB206",
            "X8_3.3.4.4.5.pncb", "X9_1.2.3.4.7.8.hxcdf", "X10_2.3.4.6.7.8.hxcdf")
Z.name <- c("AGE.c", "SEX.Female", "RACE.NH.Black",
            "RACE.MexicanAmerican", "RACE.OtherRace", "RACE.Hispanic")
spline.num <- 5
spline.degree <- 3
initial.random.num <- 1  # only for test, set any number

model_v2 <- plsi.lr.v2(data = dat, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
                       spline.num, spline.degree, initial.random.num, seed = 2023)

test_that('Output of plsi.lr.v2', {
  expect_true(is.list(model_v2))
  expect_true(is.data.frame(model_v2$si.coefficient))
  expect_true(is.data.frame(model_v2$confounder.coefficient))
  expect_true(is.data.frame(model_v2$si.fun))
  # multi-exposure dimension check -- this is the case that used to crash on
  # the t(beta_est) transposition bug in plsi.lr.v1
  expect_equal(nrow(model_v2$si.coefficient), ncol(model_v2$original.data$x))
  expect_equal(nrow(model_v2$confounder.coefficient), ncol(model_v2$original.data$z))
})

test_that('plsi.lr.v2 si.coefficient has expected columns', {
  expect_true(all(c("Estimate", "Std.Error", "t value", "Pr(>|t|)",
                    "Lower.95CI", "Upper.95CI", "Contribution proportion")
                  %in% colnames(model_v2$si.coefficient)))
  # sorted by Estimate, descending
  expect_equal(model_v2$si.coefficient$Estimate,
               sort(model_v2$si.coefficient$Estimate, decreasing = TRUE))
})

test_that('plsi.lr.v2 returns correctly named list elements (typo fix)', {
  expect_true("initial.table" %in% names(model_v2))
  expect_true("all.initial.results" %in% names(model_v2))
  expect_false("intial.table" %in% names(model_v2))
  expect_false("all.intial.results" %in% names(model_v2))
})

test_that('plsi.lr.v2 si.fun has fit/lwr/upr columns for si.fun.plot compatibility', {
  # ciTools::add_ci()'s yhatName defaults to "pred", not "fit" -- si.fun.plot()
  # requires "fit". If this fails, the yhatName = "fit" argument was dropped
  # from the ciTools::add_ci() call in plsi.lr.v2().
  expect_true(all(c("single_index_estimated", "fit", "lwr", "upr")
                  %in% colnames(model_v2$si.fun)))
})

test_that('plsi.lr.v2 rejects initial.random.num = 0 with an informative error', {
  # v1's version of this edge case silently produced an invalid index
  # sequence and could crash downstream; v2 fixes this by validating the
  # input up front via stopifnot(initial.random.num >= 1), matching the
  # pattern used by the *.auto() functions. The correct behavior here is a
  # clear error, not a silent successful fit -- this test previously
  # asserted the opposite (carried over from v1's buggy behavior) and was
  # wrong.
  expect_error(
    plsi.lr.v2(data = dat, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
               spline.num, spline.degree, initial.random.num = 0, seed = 2023),
    "initial.random.num"
  )
})

test_that('plsi.lr.v2 rejects missing columns and NAs with informative errors', {
  expect_error(
    plsi.lr.v2(data = dat, Y.name = "not.a.real.column", X.name = X.name, Z.name = Z.name,
               spline.num, spline.degree, initial.random.num, seed = 2023),
    "not found in data"
  )
  dat_na <- dat
  dat_na[1, Y.name] <- NA
  expect_error(
    plsi.lr.v2(data = dat_na, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
               spline.num, spline.degree, initial.random.num, seed = 2023),
    "Missing values"
  )
})

test_that('plsi.lr.v2 is reproducible given the same seed', {
  model_v2_repeat <- plsi.lr.v2(data = dat, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
                                spline.num, spline.degree, initial.random.num, seed = 2023)
  expect_equal(model_v2$si.coefficient$Estimate, model_v2_repeat$si.coefficient$Estimate,
               tolerance = 1e-8)
})
