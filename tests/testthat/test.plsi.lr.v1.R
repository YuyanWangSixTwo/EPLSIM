context('plsi.lr.v1 -  PLSI linear regression version 1')

test_that('Output of plsi.lr.v1', {

  data(nhanes.new)
  dat <- nhanes.new
  Y.name <- "log.triglyceride"
  X.name <- c("X1_trans.b.carotene", "X2_retinol", "X3_g.tocopherol", "X4_a.tocopherol",
              "X5_PCB99", "X6_PCB156", "X7_PCB206",
              "X8_3.3.4.4.5.pncb", "X9_1.2.3.4.7.8.hxcdf", "X10_2.3.4.6.7.8.hxcdf")
  Z.name <- c("AGE.c", "SEX.Female", "RACE.NH.Black",
              "RACE.MexicanAmerican", "RACE.OtherRace", "RACE.Hispanic" )
  spline.num = 5
  spline.degree = 3
  initial.random.num = 1
  set.seed(2023)
  model_1 <- plsi.lr.v1(data = dat, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
                        spline.num, spline.degree, initial.random.num)

  expect_true(is.list(model_1))
  expect_true(is.data.frame(model_1$si.coefficient))
  expect_true(is.data.frame(model_1$confounder.coefficient))
  expect_true(is.data.frame(model_1$si.fun))
  expect_equal(nrow(model_1$si.coefficient), ncol(model_1$original.data$x))
  expect_equal(nrow(model_1$confounder.coefficient), ncol(model_1$original.data$z))
})
