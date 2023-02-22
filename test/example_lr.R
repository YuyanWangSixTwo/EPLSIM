require("Rtools")
require("PerformanceAnalytics")
require("splines")
require("ggplot2")
require("corrplot")
require("ciTools")
require("MASS")

## Step 0: import dataset
##############################################################################################
dat=nhanes
names(dat)
Y <- "triglyceride"
X = c("a1.trans.b.carotene","a5.Retinol","a6.g.tocopherol","a7.a.Tocopherol",
      "a10.PCB99","a13.PCB156","a19.PCB206",
      "a20.3.3.4.4.5.pncb","a21.1.2.3.4.7.8.hxcdf","a22.2.3.4.6.7.8.hxcdf")
Z <- c("age","sex","race")
##############################################################################################

## Step 0.1.1: check original exposure distribution
##############################################################################################
par(mfrow=c(2,5))
for (i in 1:length(X)) {
  hist(dat[,X[i]],main=X[i],xlab=NA,ylab=NA)
}
##############################################################################################

## Step 0.1.2:check log-transformed exposure distribution
##############################################################################################
for (i in 1:length(X)) {
  hist(log(dat[,X[i]]),main=paste("log(",X[i],")",sep=""),xlab=NA,ylab=NA)
}
##############################################################################################
dev.off()

## Step 0.1.3: log-transform, standardize, and rename the exposure variables
##############################################################################################
dat[ , paste("log.", X, sep = "")] = log(dat[, X])
X = paste("log.", X, sep = "")
dat[, X] = scale(dat[, X])

dat$X1_trans.b.carotene=dat$a1.trans.b.carotene
dat$X2_retinol=dat$log.a5.Retinol
dat$X3_g.tocopherol=dat$log.a6.g.tocopherol
dat$X4_a.tocopherol=dat$log.a7.a.Tocopherol
dat$X5_PCB99=dat$log.a10.PCB99
dat$X6_PCB156=dat$log.a13.PCB156
dat$X7_PCB206=dat$log.a19.PCB206
dat$X8_3.3.4.4.5.pncb=dat$log.a20.3.3.4.4.5.pncb
dat$X9_1.2.3.4.7.8.hxcdf=dat$log.a21.1.2.3.4.7.8.hxcdf
dat$X10_2.3.4.6.7.8.hxcdf=dat$log.a22.2.3.4.6.7.8.hxcdf

X = c("X1_trans.b.carotene","X2_retinol","X3_g.tocopherol","X4_a.tocopherol",
      "X5_PCB99","X6_PCB156","X7_PCB206",
      "X8_3.3.4.4.5.pncb","X9_1.2.3.4.7.8.hxcdf","X10_2.3.4.6.7.8.hxcdf")
##############################################################################################

## Step 0.1.4: check exposure correlation
##############################################################################################
cor_matrix = cor(dat[,X])
corrplot.mixed(cor_matrix, upper = "ellipse", lower = "number",
               tl.pos = "lt", tl.col = "black")
##############################################################################################

## Step 0.1.5: reorder exposures to ensure sing index coefficient constraints
##############################################################################################
print(X)
X = re_order(X = X, Y = Y, data = dat)
print(X)
##############################################################################################


## Step 0.2.1: check outcome distribution
##############################################################################################
hist(dat[,Y],main=Y,xlab=NA,ylab=NA)
dat[ , paste("log.", Y, sep = "")] = log(dat[, Y])
Y = paste("log.", Y, sep = "")
dat[, Y] = scale(dat[, Y])
hist(dat[,Y],main=Y,xlab=NA,ylab=NA)
##############################################################################################

## Step 0.2.2: check outliers and delete records with outliers
##############################################################################################
nrow(dat)
dat=dat[!(dat[,Y] %in% boxplot(dat[,Y],range=5,plot=FALSE)$out),]
nrow(dat)
for (i in 1:length(X)) {
  # print(sum(dat[,X[i]] %in% boxplot(dat[,X[i]],range=10,plot=FALSE)$out))
  dat=dat[!(dat[,X[i]] %in% boxplot(dat[,X[i]],range=5,plot=FALSE)$out),]
}
nrow(dat)
##############################################################################################


## Step 0.3.1:preprocess the covariates; centralize the continuous covariates; factorize the categorical covariates
##############################################################################################
dat$SEX <- factor(dat$sex,1:2,c('Male','Female'))
dat$RACE <- factor(dat$race,1:5,c("Non-Hispanic White","Non-Hispanic Black",
                                  "Mexican American","Other Race - Including Multi-Racial","Other Hispanic"))
dat$AGE <- dat$age - mean(dat$age)
cov_m <- covariate_trans(Z_continuous = c("age"), Z_discrete = c("SEX", "RACE"), data = dat)
Z <- cov_m[[1]]
dat <- cov_m[[2]]
print(Z)
##############################################################################################


## Step 1: run PLSI linear model
##############################################################################################
spline_num = 5
spline_degree = 3
initial_random_num = 5
model_1 = plsi_lr_v1(data = dat, Y = Y, X = X, Z = Z, spline_num, spline_degree, initial_random_num)
##############################################################################################

## Step 2.1: nonparametric link function
##############################################################################################
link_plot(link_ci=model_1$link_ci)
##############################################################################################


## Step 2.2.1: estimated single index coefficients
##############################################################################################
beta_est <- model_1$beta_results
beta_plot(beta_est=beta_est)
##############################################################################################

## Step 2.2.2: estimated partial linear covariates' coefficients
##############################################################################################
alpha_est <- model_1$alpha_estimated
##############################################################################################


## Step 2.3.1: overall effect
##############################################################################################
quantile_overall_plot(fit=model_1, data=dat)
##############################################################################################


## Step 2.3.2: exposure main effect
##############################################################################################
quantile_main_plot(fit=model_1, data = dat, exp_name=c("X4_a.tocopherol"))
quantile_main_plot(fit=model_1, data = dat, exp_name=c("X5_PCB99"))
quantile_main_plot(fit=model_1, data = dat, exp_name=c("X10_2.3.4.6.7.8.hxcdf"))
X = c("X1_trans.b.carotene","X2_retinol","X3_g.tocopherol","X4_a.tocopherol",
      "X5_PCB99","X6_PCB156","X7_PCB206",
      "X8_3.3.4.4.5.pncb","X9_1.2.3.4.7.8.hxcdf","X10_2.3.4.6.7.8.hxcdf")
##############################################################################################


## Step 2.4.1: interaction effect
##############################################################################################
quantile_interaction_plot(fit=model_1, data = dat, exp_1="X4_a.tocopherol", exp_2="X3_g.tocopherol")
##############################################################################################

## Step 2.4.1: interaction effect
##############################################################################################
quantile_interaction_plot(fit=model_1, data = dat, exp_1="X4_a.tocopherol", exp_2="X10_2.3.4.6.7.8.hxcdf")
##############################################################################################

## Step 2.4.2: interaction effect, exchange exposure
##############################################################################################
quantile_interaction_plot(fit=model_1, data = dat, exp_1="X8_3.3.4.4.5.pncb", exp_2="X6_PCB156")
quantile_interaction_plot(fit=model_1, data = dat, exp_2="X8_3.3.4.4.5.pncb", exp_1="X6_PCB156")
dev.off()
##############################################################################################

## Step 2.5: interquartile quartile plot
##############################################################################################
interquartile_quartile_plot(fit = model_1, data = dat)
##############################################################################################


## interquartile of interquartile plot
# interquartile_interquartile_plot(fit = model_1, data = dat)
