require("Rtools")
require("PerformanceAnalytics")
require("splines")
require("ggplot2")
require("corrplot")
require("ciTools")
require("MASS")

# load("D:/github/EPLSIM/data/nhanes.rda")
# load("/Users/yuyanwang/Documents/GitHub/EPISIM/data/nhanes.rda")
load("C:/Users/WANGY40/github/EPLSIM/data/nhanes.rda")

#### Step 0: import dataset
##############################################################################################
dat=nhanes
names(dat)
Y <- "triglyceride"
X = c("a1.trans.b.carotene","a5.Retinol","a6.g.tocopherol","a7.a.Tocopherol",
      "a10.PCB99","a13.PCB156","a19.PCB206",
      "a20.3.3.4.4.5.pncb","a21.1.2.3.4.7.8.hxcdf","a22.2.3.4.6.7.8.hxcdf")
Z <- c("age","sex","race")
##############################################################################################

#### Step 0.1.1: check original exposure distribution
##############################################################################################
par(mfrow=c(2,5))
for (i in 1:length(X)) {
  hist(dat[,X[i]],main=X[i],xlab=NA,ylab=NA)
}
##############################################################################################

#### Step 0.1.2:check log-transformed exposure distribution
##############################################################################################
for (i in 1:length(X)) {
  hist(log(dat[,X[i]]),main=paste("log(",X[i],")",sep=""),xlab=NA,ylab=NA)
}
##############################################################################################
dev.off()

#### Step 0.1.3:log-transform, standardize, and rename the exposure variables
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

#### Step 0.1.4: check exposure correlation
##############################################################################################
cor_matrix = cor(dat[,X])
corrplot.mixed(cor_matrix, upper = "ellipse", lower = "number",
               tl.pos = "lt", tl.col = "black")
##############################################################################################

### Step 0.1.5: reorder exposures to ensure sing index coefficient constraints
##############################################################################################
print(X)
X = re_order(X = X, Y = Y, data = dat)
print(X)
##############################################################################################


### check outliers and delete records with outliers
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


### preprocess the covariates; entralize the continuous covariates; factorize the categorical covariates
##############################################################################################
dat$SEX <- factor(dat$sex,1:2,c('Male','Female'))
dat$RACE <- factor(dat$race,1:5,c("Non-Hispanic White","Non-Hispanic Black","Mexican American","Other Race - Including Multi-Racial","Other Hispanic"))
dat$AGE <- dat$age - mean(dat$age)
cov_m <- covariate_trans(Z_continuous = c("age"), Z_discrete = c("SEX", "RACE"), data = dat)
Z <- cov_m[[1]]
dat <- cov_m[[2]]
##############################################################################################


### PLSI linear regression model
##############################################################################################
model_1 = plsi_lr_v1(data = dat, Y = Y, X = X, Z = Z, spline_num = 5, spline_degree = 3, initial_random_num = 5)
##############################################################################################



### link function plot
link_plot(link_ci=model_1$link_ci, cut=0.00)


### beta plot
beta_est <- model_1$beta_results
beta_plot(beta_est=beta_est)

alpha_est <- model_1$alpha_estimated


### quantile overall prediction plot
quantile_overall_plot(fit=model_1, data=dat)


### quantile main effect plot
quantile_main_plot(fit=model_1, data = dat, exp_name=c("log.a7.a.Tocopherol"))
quantile_main_plot(fit=model_1, data = dat, exp_name=c("log.a5.Retinol"))
quantile_main_plot(fit=model_1, data = dat, exp_name=c("log.a10.PCB99"))
# "log.a7.a.Tocopherol"       "log.a6.g.tocopherol"       "log.a5.Retinol"
# "log.a20.3.3.4.4.5.pncb"    "log.a13.PCB156"            "log.a19.PCB206"
# "log.a10.PCB99"             "log.a21.1.2.3.4.7.8.hxcdf" "log.a1.trans.b.carotene"
# "log.a22.2.3.4.6.7.8.hxcdf"

### quantile interaction plot
quantile_interaction_plot(fit=model_1, data = dat, exp_1="log.a7.a.Tocopherol", exp_2="log.a6.g.tocopherol")
quantile_interaction_plot(fit=model_1, data = dat, exp_1="log.a7.a.Tocopherol", exp_2="log.a1.trans.b.carotene")
quantile_interaction_plot(fit=model_1, data = dat, exp_1="log.a20.3.3.4.4.5.pncb", exp_2="log.a13.PCB156")
quantile_interaction_plot(fit=model_1, data = dat, exp_2="log.a20.3.3.4.4.5.pncb", exp_1="log.a13.PCB156")
dev.off()

### interquartile quartile plot
interquartile_quartile_plot(fit = model_1, data = dat)




### interquartile of interquartile plot
# interquartile_interquartile_plot(fit = model_1, data = dat)



