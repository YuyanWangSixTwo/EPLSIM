## No test: 
# example to interquartile effect based on quartile of other exposures -- continuous outcome
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

# plot interquartile quartile -- difference in predicted (continuous) outcome
interquartile.quartile.plot(model_lr_auto, data, type = "linear")
## End(No test)



