# example to normalize the continuous confounders and
# make dummy variables for categorical confoduners
dat.cov <- data.frame(
  age = c(1.5, 2.3, 3.1, 4.8, 5.2),
  sex = c(1, 2, 1, 2, 2),
  race = c(1, 2, 3, 4, 5)
)

# specify the confounder vector
Z.name <- c("age", "sex", "race")

# set levels and make the reference level first for categorical confounders
dat.cov$sex <- factor(dat.cov$sex, 1:2, c('Male', 'Female'))
dat.cov$race <- factor(dat.cov$race,1:5,c("NH-White", "NH-Black",
                                        "MexicanAmerican", "OtherRace", "Hispanic"))

# transform the confounder vector and check
cov_m <- confounder.trans(Z_continuous = c("age"), Z_discrete = c("sex", "race"), data = dat.cov)
Z.name <- cov_m$New.Name
dat.cov <- cov_m$Updated.data
print(Z.name)
