# example to plot estimated single index coefficients

# specify variable names and parameters
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

# run PLSI linear regression
set.seed(2023)
model_1 <- plsi.lr.v1(data = dat, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
                      spline.num, spline.degree, initial.random.num)

# plot estimated single index coefficients
si.coef.plot(model_1$si.coefficient)

# check estimated single index coefficients
model_1$si.coefficient
