require("Rtools")
require("PerformanceAnalytics")
require("splines")
require("ggplot2")

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

### check exposure correlation
cor_matrix = cor(dat[,X])
corrplot::corrplot(cor_matrix)
cor_matrix_cut = cor_matrix>0.9
corrplot::corrplot(cor_matrix_cut)

### update exposure
X = c("log.a1.trans.b.carotene","log.a5.Retinol","log.a6.g.tocopherol","log.a7.a.Tocopherol",
    "log.a10.PCB99","log.a13.PCB156","log.a19.PCB206",
    "log.a20.3.3.4.4.5.pncb","log.a21.1.2.3.4.7.8.hxcdf","log.a22.2.3.4.6.7.8.hxcdf")
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

### link function plot
index <- model_1$index_estimated
link <- model_1$link_estimated
link_plot(index=index, link=link, cut=cut)

### beta plot
beta_est <- model_1$beta_estimated
beta_plot(beta_est=beta_est)


