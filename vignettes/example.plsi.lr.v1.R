# example to run partial linear single index linear regression

data(nhanes.new)
dat <- nhanes.new

# specify variable names
Y.name <- "log.triglyceride"
X.name <- c("X1_trans.b.carotene", "X2_retinol", "X3_g.tocopherol", "X4_a.tocopherol",
            "X5_PCB99", "X6_PCB156", "X7_PCB206",
            "X8_3.3.4.4.5.pncb", "X9_1.2.3.4.7.8.hxcdf", "X10_2.3.4.6.7.8.hxcdf")
Z.name <- c("AGE.c", "SEX.Female", "RACE.NH.Black",
            "RACE.MexicanAmerican", "RACE.OtherRace", "RACE.Hispanic" )

# specify spline degree of freedom
spline.num = 5
# specify spline degree
spline.degree = 3
# specify number of random initials for estimation
initial.random.num = 1

# run the model
set.seed(2023)
model_1 <- plsi.lr.v1(data = dat, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
                      spline.num, spline.degree, initial.random.num)

# check estimated confounder coefficients
model_1$confounder.coefficient
