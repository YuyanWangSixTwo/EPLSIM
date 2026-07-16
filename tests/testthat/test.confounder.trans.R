context('confounder.trans -  transform confounders ready to be used by PLSIM')

test_that('Output of confounder.trans', {
  dat.cov <- data.frame(
    age = c(1.5, 2.3, 3.1, 4.8, 5.2),
    sex = c(1, 2, 1, 2, 2),
    race = c(1, 2, 3, 4, 5)
  )
  dat.cov$sex <- factor(dat.cov$sex, 1:2, c('Male', 'Female'))
  dat.cov$race <- factor(dat.cov$race,1:5,c("NH-White", "NH-Black",
                                            "MexicanAmerican", "OtherRace", "Hispanic"))
  Z_continuous = c("age")
  Z_discrete = c("sex", "race")
  cov_m <- confounder.trans(Z_continuous = c("age"), Z_discrete = c("sex", "race"), data = dat.cov)

  expect_true(is.list(cov_m))
  expect_true(is.vector(cov_m$New.Name))
  expect_true(is.data.frame(cov_m$Updated.data))

  expect_equal(ncol(cov_m$Updated.data), ncol(dat.cov) + length(cov_m$New.Name))
})
