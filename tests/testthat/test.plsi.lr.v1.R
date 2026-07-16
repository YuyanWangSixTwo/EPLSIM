
test_that('plsi.lr.v1 returns correctly named list elements (typo fix)', {
  data(nhanes.new)
  dat <- nhanes.new
  Y.name <- "log.triglyceride"
  X.name <- c("X1_trans.b.carotene", "X2_retinol", "X3_g.tocopherol", "X4_a.tocopherol",
              "X5_PCB99", "X6_PCB156", "X7_PCB206",
              "X8_3.3.4.4.5.pncb", "X9_1.2.3.4.7.8.hxcdf", "X10_2.3.4.6.7.8.hxcdf")
  Z.name <- c("AGE.c", "SEX.Female", "RACE.NH.Black",
              "RACE.MexicanAmerican", "RACE.OtherRace", "RACE.Hispanic")
  set.seed(2023)
  model_1 <- plsi.lr.v1(data = dat, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
                        spline.num = 5, spline.degree = 3, initial.random.num = 1)

  # Corrected names must be present
  expect_true("initial.table" %in% names(model_1))
  expect_true("all.initial.results" %in% names(model_1))

  # Old typo'd names must NOT be present
  expect_false("intial.table" %in% names(model_1))
  expect_false("all.intial.results" %in% names(model_1))
})

test_that('plsi.lr.v1 handles initial.random.num = 0 without error', {
  data(nhanes.new)
  dat <- nhanes.new
  Y.name <- "log.triglyceride"
  X.name <- c("X1_trans.b.carotene", "X2_retinol", "X3_g.tocopherol", "X4_a.tocopherol",
              "X5_PCB99", "X6_PCB156", "X7_PCB206",
              "X8_3.3.4.4.5.pncb", "X9_1.2.3.4.7.8.hxcdf", "X10_2.3.4.6.7.8.hxcdf")
  Z.name <- c("AGE.c", "SEX.Female", "RACE.NH.Black",
              "RACE.MexicanAmerican", "RACE.OtherRace", "RACE.Hispanic")
  set.seed(2023)

  expect_error(
    model_0 <- plsi.lr.v1(data = dat, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
                          spline.num = 5, spline.degree = 3, initial.random.num = 0),
    NA  # NA means "expect no error at all"
  )
  expect_true(is.list(model_0))
})

test_that('plsi.lr.v1 logLik output is numeric', {
  data(nhanes.new)
  dat <- nhanes.new
  Y.name <- "log.triglyceride"
  X.name <- c("X1_trans.b.carotene", "X2_retinol", "X3_g.tocopherol", "X4_a.tocopherol",
              "X5_PCB99", "X6_PCB156", "X7_PCB206",
              "X8_3.3.4.4.5.pncb", "X9_1.2.3.4.7.8.hxcdf", "X10_2.3.4.6.7.8.hxcdf")
  Z.name <- c("AGE.c", "SEX.Female", "RACE.NH.Black",
              "RACE.MexicanAmerican", "RACE.OtherRace", "RACE.Hispanic")
  set.seed(2023)
  model_1 <- plsi.lr.v1(data = dat, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
                        spline.num = 5, spline.degree = 3, initial.random.num = 1)

  expect_type(model_1$logLik, "double")
  expect_false(is.list(model_1$logLik))  # catches un-coerced logLik() object
})
