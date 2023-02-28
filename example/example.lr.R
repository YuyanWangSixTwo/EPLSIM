# require("Rtools")
# require("PerformanceAnalytics")
# require("splines")
# require("ggplot2")
# require("corrplot")
# require("ciTools")
# require("MASS")

# Load raw data from .csv file
# nhanes <- read.csv("data-raw/NHANES.csv")
# Save the cleaned data in the required R package location
# usethis::use_data(nhanes, overwrite = TRUE)

## Step 0: import dataset
##############################################################################################
data(nhanes)
dat = nhanes
names(dat)
Y.name <- "triglyceride"
X.name <- c("a1.trans.b.carotene", "a5.Retinol", "a6.g.tocopherol", "a7.a.Tocopherol",
      "a10.PCB99", "a13.PCB156", "a19.PCB206",
      "a20.3.3.4.4.5.pncb", "a21.1.2.3.4.7.8.hxcdf", "a22.2.3.4.6.7.8.hxcdf")
Z.name <- c("age", "sex",  "race")
##############################################################################################

## Step 0.1.1: check original exposure distribution
##############################################################################################
par(mfrow = c(2, 5))
for (i in 1:length(X.name)) {
  hist(dat[, X.name[i]], main = X.name[i], xlab = NA, ylab = NA)
}
##############################################################################################

## Step 0.1.2:check log-transformed exposure distribution
##############################################################################################
for (i in 1:length(X.name)) {
  hist(log(dat[, X.name[i]]),main = paste("log(", X.name[i], ")", sep = ""), xlab = NA, ylab = NA)
}
##############################################################################################
par(mfrow = c(1, 1))

## Step 0.1.3: log-transform, standardize, and rename the exposure variables
##############################################################################################
dat[ , paste("log.", X.name, sep = "")] = log(dat[, X.name])
X.name = paste("log.", X.name, sep = "")
dat[, X.name] = scale(dat[, X.name])

dat$X1_trans.b.carotene = dat$a1.trans.b.carotene
dat$X2_retinol = dat$log.a5.Retinol
dat$X3_g.tocopherol = dat$log.a6.g.tocopherol
dat$X4_a.tocopherol = dat$log.a7.a.Tocopherol
dat$X5_PCB99 = dat$log.a10.PCB99
dat$X6_PCB156 = dat$log.a13.PCB156
dat$X7_PCB206 = dat$log.a19.PCB206
dat$X8_3.3.4.4.5.pncb = dat$log.a20.3.3.4.4.5.pncb
dat$X9_1.2.3.4.7.8.hxcdf = dat$log.a21.1.2.3.4.7.8.hxcdf
dat$X10_2.3.4.6.7.8.hxcdf = dat$log.a22.2.3.4.6.7.8.hxcdf

X.name = c("X1_trans.b.carotene", "X2_retinol", "X3_g.tocopherol", "X4_a.tocopherol",
      "X5_PCB99", "X6_PCB156", "X7_PCB206",
      "X8_3.3.4.4.5.pncb", "X9_1.2.3.4.7.8.hxcdf", "X10_2.3.4.6.7.8.hxcdf")
##############################################################################################

## Step 0.1.4: check exposure correlation
##############################################################################################
cor_matrix = cor(dat[,X.name])
corrplot::corrplot.mixed(cor_matrix, upper = "ellipse", lower = "number",
               tl.pos = "lt", tl.col = "black")
##############################################################################################


## Step 0.2.1: check outcome distribution
##############################################################################################
hist(dat[, Y.name], main = Y.name, xlab = NA, ylab = NA)
dat[ , paste("log.", Y.name, sep = "")] = log(dat[, Y.name])
Y.name = paste("log.", Y.name, sep = "")
dat[, Y.name] = scale(dat[, Y.name])
hist(dat[, Y.name], main = Y.name, xlab = NA, ylab = NA)
##############################################################################################

## Step 0.2.2: check outliers and delete records with outliers
##############################################################################################
nrow(dat)
dat=dat[!(dat[, Y.name] %in% boxplot(dat[, Y.name], range = 5, plot = FALSE)$out), ]
nrow(dat)
for (i in 1:length(X.name)) {
  # print(sum(dat[, X.name[i]] %in% boxplot(dat[, X.name[i]], range = 10, plot = FALSE)$out))
  dat=dat[!(dat[, X.name[i]] %in% boxplot(dat[, X.name[i]], range = 5, plot = FALSE)$out), ]
}
nrow(dat)
##############################################################################################


## Step 0.3.1:preprocess the confounders; centralize the continuous confounders; factorize the categorical confounders
##############################################################################################
dat$SEX <- factor(dat$sex,1:2,c('Male','Female'))
dat$RACE <- factor(dat$race,1:5,c("Non-Hispanic White","Non-Hispanic Black",
                                  "Mexican American","Other Race - Including Multi-Racial","Other Hispanic"))
dat$AGE <- dat$age - mean(dat$age)
cov_m <- covariate.trans(Z_continuous = c("AGE"), Z_discrete = c("SEX", "RACE"), data = dat)
Z.name <- cov_m[[1]]
dat <- cov_m[[2]]
print(Z.name)
##############################################################################################


## Step 1: run PLSI linear model
##############################################################################################
spline.num = 5
spline.degree = 3
initial.random.num = 5
model_1 <- plsi.lr.v1(data = dat, Y.name = Y.name, X.name = X.name, Z.name = Z.name, spline.num, spline.degree, initial.random.num)
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
expvar.main.plot(model_1, dat, exp_name = c("X4_a.tocopherol"))
expvar.main.plot(model_1, dat, exp_name = c("X5_PCB99"))
expvar.main.plot(model_1, dat, exp_name = c("X10_2.3.4.6.7.8.hxcdf"))
X.name = c("X1_trans.b.carotene", "X2_retinol", "X3_g.tocopherol", "X4_a.tocopherol",
           "X5_PCB99", "X6_PCB156", "X7_PCB206",
           "X8_3.3.4.4.5.pncb", "X9_1.2.3.4.7.8.hxcdf", "X10_2.3.4.6.7.8.hxcdf")
##############################################################################################


## Step 2.4.1: interaction effect
##############################################################################################
expvar.interaction.plot(model_1, dat, "X4_a.tocopherol", "X3_g.tocopherol")
##############################################################################################

## Step 2.4.1: interaction effect
##############################################################################################
expvar.interaction.plot(model_1, dat, "X4_a.tocopherol", "X10_2.3.4.6.7.8.hxcdf")
##############################################################################################

## Step 2.4.2: interaction effect, exchange exposure
##############################################################################################
expvar.interaction.plot(model_1, dat, "X8_3.3.4.4.5.pncb", "X6_PCB156")
expvar.interaction.plot(model_1, dat, "X8_3.3.4.4.5.pncb", "X6_PCB156")
##############################################################################################

## Step 2.5: interquartile quartile plot
##############################################################################################
interquartile.quartile.plot(model_1, dat)
##############################################################################################
