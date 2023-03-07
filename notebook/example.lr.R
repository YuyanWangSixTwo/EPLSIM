data(nhanes.new)
dat <- nhanes.new

Y.name <- "log.triglyceride"
X.name <- c("X1_trans.b.carotene", "X2_retinol", "X3_g.tocopherol", "X4_a.tocopherol",
            "X5_PCB99", "X6_PCB156", "X7_PCB206",
            "X8_3.3.4.4.5.pncb", "X9_1.2.3.4.7.8.hxcdf", "X10_2.3.4.6.7.8.hxcdf")
Z.name <- c("AGE.c", "SEX.Female", "RACE.NH.Black", "RACE.MexicanAmerican", "RACE.OtherRace", "RACE.Hispanic" )

## Step 1: run PLSI linear model
##############################################################################################
spline.num = 5
spline.degree = 3
initial.random.num = 5
set.seed(2023)
model_1 <- plsi.lr.v1(data = dat, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
                      spline.num, spline.degree, initial.random.num)
##############################################################################################

## Step 2.1: plot single index function
##############################################################################################
si.fun.plot(model_1$si.fun)
##############################################################################################

## Step 2.2.1: estimated single index coefficients
##############################################################################################
model_1$si.coefficient
si.coef.plot(model_1$si.coefficient)
##############################################################################################

## Step 2.2.2: estimated partial linear confounder' coefficients
##############################################################################################
model_1$confounder.coefficient
##############################################################################################

## Step 2.3.1: mixture overall effect plot
##############################################################################################
mixture.overall.plot(model_1, dat)
##############################################################################################


## Step 2.3.2: exposure main effect plot
##############################################################################################
e.main.plot(model_1, dat, exp_name = c("X4_a.tocopherol"))
e.main.plot(model_1, dat, exp_name = c("X5_PCB99"))
e.main.plot(model_1, dat, exp_name = c("X10_2.3.4.6.7.8.hxcdf"))
X.name = c("X1_trans.b.carotene", "X2_retinol", "X3_g.tocopherol", "X4_a.tocopherol",
           "X5_PCB99", "X6_PCB156", "X7_PCB206",
           "X8_3.3.4.4.5.pncb", "X9_1.2.3.4.7.8.hxcdf", "X10_2.3.4.6.7.8.hxcdf")
##############################################################################################

## Step 2.4.1: interaction effect
##############################################################################################
e.interaction.plot(model_1, dat, "X4_a.tocopherol", "X3_g.tocopherol")
e.interaction.plot(model_1, dat, "X4_a.tocopherol", "X10_2.3.4.6.7.8.hxcdf")
##############################################################################################

## Step 2.4.2: interaction effect, exchange exposure
##############################################################################################
e.interaction.plot(model_1, dat, "X8_3.3.4.4.5.pncb", "X6_PCB156")
e.interaction.plot(model_1, dat, "X6_PCB156", "X8_3.3.4.4.5.pncb")
##############################################################################################

## Step 2.5: interquartile quartile plot
##############################################################################################
interquartile.quartile.plot(model_1, dat)
##############################################################################################
