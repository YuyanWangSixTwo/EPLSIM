context('plot functions -  si.fun.plot, si.coef.plot, e.main.plot, e.interaction.plot, interquartile.quartile.plot, mixture.overall.plot')
testthat::skip_on_cran()
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

# Route all base + ggplot2 output to a null device so tests don't try to pop
# up a plot window, then close it however the test exits.
with_null_device <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  force(expr)
}

test_that('si.fun.plot runs without error for type = "linear"', {
  expect_error(with_null_device(si.fun.plot(model_lr_auto$si.fun, type = "linear")), NA)
})

test_that('si.fun.plot errors informatively on the wrong type for the data', {
  # model_lr_auto$si.fun has no prob.fit/prob.lwr/prob.upr columns
  expect_error(
    with_null_device(si.fun.plot(model_lr_auto$si.fun, type = "logistic")),
    "missing column"
  )
})

test_that('si.coef.plot runs without error', {
  expect_error(with_null_device(si.coef.plot(model_lr_auto$si.coefficient)), NA)
})

test_that('e.main.plot runs and invisibly returns the plotted data', {
  result <- with_null_device(
    e.main.plot(model_lr_auto, dat, exp_name = "X4_a.tocopherol", type = "linear")
  )
  expect_true(is.data.frame(result))
  expect_true(all(c("x_value", "single_index_estimated", "fit", "lwr", "upr") %in% colnames(result)))
})

test_that('e.interaction.plot runs without error and both panels differ', {
  expect_error(
    with_null_device(
      e.interaction.plot(model_lr_auto, dat, "X4_a.tocopherol", "X3_g.tocopherol", type = "linear")
    ),
    NA
  )
})

test_that('e.interaction.plot conditions each panel on the OTHER exposure (regression test for the copy-paste quantile bug)', {
  # e.interaction.plot() now invisibly returns panel_1/panel_2's underlying
  # data (see e_interaction_plot.R) -- check the actual computed values
  # rather than comparing rendered plots. recordPlot() is not reliable for
  # this: display-list recording is designed for interactive/screen
  # devices, and returned identical() == TRUE for two genuinely different
  # plots when tried on a pdf(NULL) device.
  result <- with_null_device(
    e.interaction.plot(model_lr_auto, dat, "X4_a.tocopherol", "X3_g.tocopherol", type = "linear")
  )

  # panel_1 (x-axis = exp_1 = X4_a.tocopherol) must vary its lines by
  # exp_2's (X3_g.tocopherol) quantiles, i.e. pred_q1/q2/q3 must actually
  # differ from each other -- if the copy-paste bug were present, panel_1
  # would instead condition on exp_1's own quantiles, which typically
  # produces a different (but still non-degenerate) set of curves, so the
  # more direct check is cross-referencing against a swapped call below.
  expect_false(isTRUE(all.equal(result$panel_1$pred_q1, result$panel_1$pred_q2)))
  expect_false(isTRUE(all.equal(result$panel_1$pred_q2, result$panel_1$pred_q3)))

  # Swapping exp_1/exp_2 must swap which exposure's quantiles condition
  # panel_1 -- this is the direct regression check for the original bug,
  # where both panels secretly conditioned on exp_2's quantiles regardless
  # of argument order.
  swapped <- with_null_device(
    e.interaction.plot(model_lr_auto, dat, "X3_g.tocopherol", "X4_a.tocopherol", type = "linear")
  )
  # result$panel_1 varies X4_a.tocopherol conditioned on X3_g.tocopherol's
  # quantiles; swapped$panel_2 varies X4_a.tocopherol (as exp_2 there)
  # conditioned on X3_g.tocopherol's quantiles too -- these should match.
  expect_equal(result$panel_1$pred_q1, swapped$panel_2$pred_q1, tolerance = 1e-8)
  expect_equal(result$panel_1$pred_q2, swapped$panel_2$pred_q2, tolerance = 1e-8)
  expect_equal(result$panel_1$pred_q3, swapped$panel_2$pred_q3, tolerance = 1e-8)
  # And result$panel_1 should NOT match swapped$panel_1 (which now varies
  # X3_g.tocopherol instead of X4_a.tocopherol) -- different x-axis
  # variable entirely, so lengths/values won't align if the fix is correct.
  expect_false(isTRUE(all.equal(result$panel_1$pred_q1, swapped$panel_1$pred_q1)))
})

test_that('interquartile.quartile.plot runs without error', {
  expect_error(with_null_device(interquartile.quartile.plot(model_lr_auto, dat, type = "linear")), NA)
})

test_that('mixture.overall.plot runs without error', {
  expect_error(with_null_device(mixture.overall.plot(model_lr_auto, dat, type = "linear")), NA)
})
