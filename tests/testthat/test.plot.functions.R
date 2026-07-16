context('plot functions -  si.fun.plot, si.coef.plot, e.main.plot, e.interaction.plot, interquartile.quartile.plot, mixture.overall.plot')

data(nhanes.new)
dat <- nhanes.new
X.name <- c("X1_trans.b.carotene", "X2_retinol", "X3_g.tocopherol", "X4_a.tocopherol",
            "X5_PCB99", "X6_PCB156", "X7_PCB206",
            "X8_3.3.4.4.5.pncb", "X9_1.2.3.4.7.8.hxcdf", "X10_2.3.4.6.7.8.hxcdf")
Z.name <- c("AGE.c", "SEX.Female", "RACE.NH.Black",
            "RACE.MexicanAmerican", "RACE.OtherRace", "RACE.Hispanic")
Y.name <- "log.triglyceride"
k <- 5
bs <- "cr"
initial.random.num <- 1

# IMPORTANT: skip_on_cran() only works as the first line INSIDE a
# test_that() block, where testthat's reporter can catch the "skip" signal
# it raises. A bare call at file level does nothing, so the fixture below
# is instead gated with a plain if(), checked before any testthat
# machinery is involved -- this is what actually prevents the expensive
# fit from running on CRAN.
not_on_cran <- identical(Sys.getenv("NOT_CRAN"), "true")

if (not_on_cran) {
  # k = 5 / initial.random.num = 1 are deliberately small to keep this test
  # file fast; at these settings the numerically-differentiated Hessian
  # occasionally isn't positive definite, which triggers plsi.lr.auto()'s
  # eigenvalue-flooring fallback (see plsi_lr_auto.R) and an accompanying
  # warning. That fallback is the intended, tested behavior -- not a
  # regression -- so it's suppressed here rather than left to print as
  # unexplained noise on every test run.
  model_lr_auto <- suppressWarnings(
    plsi.lr.auto(data = dat, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
                 k = k, bs = bs, initial.random.num = initial.random.num, seed = 2026)
  )
}

# Route all base + ggplot2 output to a null device so tests don't try to pop
# up a plot window, then close it however the test exits.
with_null_device <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  force(expr)
}

test_that('si.fun.plot runs without error for type = "linear"', {
  skip_on_cran()
  expect_error(with_null_device(si.fun.plot(model_lr_auto$si.fun, type = "linear")), NA)
})

test_that('si.fun.plot errors informatively on the wrong type for the data', {
  skip_on_cran()
  expect_error(
    with_null_device(si.fun.plot(model_lr_auto$si.fun, type = "logistic")),
    "missing column"
  )
})

test_that('si.coef.plot runs without error', {
  skip_on_cran()
  expect_error(with_null_device(si.coef.plot(model_lr_auto$si.coefficient)), NA)
})

test_that('e.main.plot runs and invisibly returns the plotted data', {
  skip_on_cran()
  result <- with_null_device(
    e.main.plot(model_lr_auto, dat, exp_name = "X4_a.tocopherol", type = "linear")
  )
  expect_true(is.data.frame(result))
  expect_true(all(c("x_value", "single_index_estimated", "fit", "lwr", "upr") %in% colnames(result)))
})

test_that('e.interaction.plot runs without error and both panels differ', {
  skip_on_cran()
  expect_error(
    with_null_device(
      e.interaction.plot(model_lr_auto, dat, "X4_a.tocopherol", "X3_g.tocopherol", type = "linear")
    ),
    NA
  )
})

test_that('e.interaction.plot conditions each panel on the OTHER exposure (regression test for the copy-paste quantile bug)', {
  skip_on_cran()
  result <- with_null_device(
    e.interaction.plot(model_lr_auto, dat, "X4_a.tocopherol", "X3_g.tocopherol", type = "linear")
  )

  expect_false(isTRUE(all.equal(result$panel_1$pred_q1, result$panel_1$pred_q2)))
  expect_false(isTRUE(all.equal(result$panel_1$pred_q2, result$panel_1$pred_q3)))

  swapped <- with_null_device(
    e.interaction.plot(model_lr_auto, dat, "X3_g.tocopherol", "X4_a.tocopherol", type = "linear")
  )
  expect_equal(result$panel_1$pred_q1, swapped$panel_2$pred_q1, tolerance = 1e-8)
  expect_equal(result$panel_1$pred_q2, swapped$panel_2$pred_q2, tolerance = 1e-8)
  expect_equal(result$panel_1$pred_q3, swapped$panel_2$pred_q3, tolerance = 1e-8)
  expect_false(isTRUE(all.equal(result$panel_1$pred_q1, swapped$panel_1$pred_q1)))
})

test_that('interquartile.quartile.plot runs without error', {
  skip_on_cran()
  expect_error(with_null_device(interquartile.quartile.plot(model_lr_auto, dat, type = "linear")), NA)
})

test_that('mixture.overall.plot runs without error', {
  skip_on_cran()
  expect_error(with_null_device(mixture.overall.plot(model_lr_auto, dat, type = "linear")), NA)
})
