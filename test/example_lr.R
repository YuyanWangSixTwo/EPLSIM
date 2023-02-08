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
### example 1: nahanes data
dat=nhanes
names(dat)
Y <- "triglyceride"
Z <- c("age","sex","race")
X <- c("a1.trans.b.carotene","a2.cis.b.carotene","a3.Retinyl.palmitate","a4.Retinyl.stearate","a5.Retinol",
    "a6.g.tocopherol","a7.a.Tocopherol","a8.PCB199","a9.PCB74","a10.PCB99",
    "a11.PCB138.158","a12.PCB153","a13.PCB156","a14.PCB170","a15.PCB180",
    "a16.PCB187","a17.PCB194","a18.PCB196.203","a19.PCB206","a20.3.3.4.4.5.pncb",
    "a21.1.2.3.4.7.8.hxcdf","a22.2.3.4.6.7.8.hxcdf")
dat$SEX <- factor(dat$sex,1:2,c('Male','Female'))
dat$RACE <- factor(dat$race,1:5,c("Non-Hispanic White","Non-Hispanic Black","Mexican American","Other Race - Including Multi-Racial","Other Hispanic"))

### check exposure distribution
# chart.Correlation(dat[,X], histogram = TRUE)
# dev.off()

### log transformation and standardization
dat[ , paste("log.", X, sep = "")] = log(dat[, X])
X = paste("log.", X, sep = "")
dat[, X] = scale(dat[, X])

##############################################################################################
### delete records with outliers for all continuous variables
nrow(dat)
dat=dat[!(dat[,Y] %in% boxplot(dat[,Y],range=10,plot=FALSE)$out),]
nrow(dat)
for (i in 1:length(X)) {
  # print(sum(dat[,X[i]] %in% boxplot(dat[,X[i]],range=5,plot=FALSE)$out))
  dat=dat[!(dat[,X[i]] %in% boxplot(dat[,X[i]],range=10,plot=FALSE)$out),]
}
nrow(dat)
##############################################################################################

### check exposure correlation
cor_matrix = cor(dat[,X])
corrplot::corrplot(cor_matrix)
cor_matrix_cut = cor_matrix>0.9
corrplot::corrplot(cor_matrix_cut)

### update exposure
X = c("log.a1.trans.b.carotene","log.a5.Retinol","log.a6.g.tocopherol","log.a7.a.Tocopherol",
    "log.a10.PCB99","log.a13.PCB156","log.a19.PCB206",
    "log.a20.3.3.4.4.5.pncb","log.a21.1.2.3.4.7.8.hxcdf","log.a22.2.3.4.6.7.8.hxcdf")

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

# X = c("log.a7.a.Tocopherol","log.a6.g.tocopherol","log.a3.Retinyl.palmitate","log.a5.Retinol",
#       "log.a20.3.3.4.4.5.pncb","log.a17.PCB194","log.a22.2.3.4.6.7.8.hxcdf","log.a1.trans.b.carotene")

cor_matrix = cor(dat[,X])
corrplot::corrplot(cor_matrix)
cor_matrix_cut = cor_matrix>0.9
corrplot::corrplot(cor_matrix_cut)
dev.off()

### centralize the continuous covariates; factorize the categorical covariates
dat$AGE <- dat$age - mean(dat$age)
cov_m <- covariate_trans(Z_continuous = c("age"), Z_discrete = c("SEX", "RACE"), data = dat)
Z <- cov_m[[1]]
dat <- cov_m[[2]]

### re_order exposure
X = re_order(X = X, Y = Y, data = dat)

### PLSI linear regression model
model_1 = plsi_lr_v1(data = dat, Y = Y, X = X, Z = Z, spline_num = 5, spline_degree = 3, initial_random_num = 5)

### beta plot
beta_est <- model_1$beta_results
beta_plot(beta_est=beta_est)

### link function plot
link_plot(link_ci=model_1$link_ci, cut=0.00)

### quantile overall prediction plot
quantile_overall_plot(fit=model_1, data=dat)

### quantile main effect plot
quantile_main_plot(fit=model_1, data = dat, exp_name=c("log.a7.a.Tocopherol"))
quantile_main_plot(fit=model_1, data = dat, exp_name=c("log.a5.Retinol"))
quantile_main_plot(fit=model_1, data = dat, exp_name=c("log.a21.1.2.3.4.7.8.hxcdf"))
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



