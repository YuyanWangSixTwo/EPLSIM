context('nhanes -  original data')

test_that('nhanes variable', {
  data(nhanes)
  Y.name <- c("triglyceride")
  X.name <- c("a1.trans.b.carotene", "a5.Retinol" , "a6.g.tocopherol",
              "a7.a.Tocopherol", "a10.PCB99", "a13.PCB156", "a19.PCB206",
              "a20.3.3.4.4.5.pncb", "a21.1.2.3.4.7.8.hxcdf", "a22.2.3.4.6.7.8.hxcdf")
  Z.name <- c("age", "sex", "race")
  expect_equal(length(colnames(nhanes)), length(Y.name) + length(X.name) + length(Z.name))
})
