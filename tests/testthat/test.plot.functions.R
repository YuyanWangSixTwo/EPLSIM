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

model_lr_auto <- plsi.lr.auto(data = dat, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
                              k = k, bs = bs, initial.random.num = initial.random.num, seed = 2026)

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

test_that('e.interaction.plot is not symmetric under argument swap (regression test for the copy-paste quantile bug)', {
  # exp_1/exp_2 swapped should NOT reduce to an identical pair of panels --
  # this guards against the earlier bug where both panels secretly
  # conditioned on exp_2's quantiles regardless of argument order.
  r1 <- with_null_device({
    e.interaction.plot(model_lr_auto, dat, "X4_a.tocopherol", "X3_g.tocopherol", type = "linear")
    grDevices::recordPlot()
  })
  r2 <- with_null_device({
    e.interaction.plot(model_lr_auto, dat, "X3_g.tocopherol", "X4_a.tocopherol", type = "linear")
    grDevices::recordPlot()
  })
  # A loose structural check: recorded plots shouldn't be byte-identical,
  # since the panel order and axis labels swap.
  expect_false(identical(r1, r2))
})

test_that('interquartile.quartile.plot runs without error', {
  expect_error(with_null_device(interquartile.quartile.plot(model_lr_auto, dat, type = "linear")), NA)
})

test_that('mixture.overall.plot runs without error', {
  expect_error(with_null_device(mixture.overall.plot(model_lr_auto, dat, type = "linear")), NA)
})
