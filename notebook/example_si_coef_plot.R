## No test: 
# example to plot estimated single index function -- continuous outcome
data(nhanes.new)
data <- nhanes.new

Y.name <- "log.triglyceride"
X.name <- c("X1_trans.b.carotene", "X2_retinol", "X3_g.tocopherol", "X4_a.tocopherol",
            "X5_PCB99", "X6_PCB156", "X7_PCB206",
            "X8_3.3.4.4.5.pncb", "X9_1.2.3.4.7.8.hxcdf", "X10_2.3.4.6.7.8.hxcdf")
Z.name <- c("AGE.c", "SEX.Female", "RACE.NH.Black",
           "RACE.MexicanAmerican", "RACE.OtherRace", "RACE.Hispanic" )

k <- 10
bs <- "cr"
initial.random.num <- 1
seed = 2026

model_lr_auto <- plsi.lr.auto(data = data, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
                      k = k, bs = bs, initial.random.num = initial.random.num, seed = seed)

# plot estimated single index coefficients
si.coef.plot(model_lr_auto$si.coefficient)

# check estimated single index coefficients
model_lr_auto$si.coefficient
## End(No test)



