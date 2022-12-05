require(splines)

dataset=dat[,c("triglyceride","log.a6.g.tocopherol")]



###############################################################################################
model_sr <- glm(triglyceride ~ ns(log.a6.g.tocopherol, df=10), data=dataset)

newdataset_0=dataset
predictions_sr_0 <- predict(model_sr, newdata=newdataset_0, interval="confidence")

newdataset_1=dataset[sample(nrow(dataset), 500, replace=F), ]
predictions_sr_1 <- predict(model_sr, newdata=newdataset_1, interval="confidence")
predictions_sr_1=cbind(newdataset_1,predictions_sr_1)
plot(predictions_sr_1$triglyceride,predictions_sr_1$predictions_sr_1)

newdataset_2=dataset[sample(nrow(dataset), 100, replace=F), ]
predictions_sr_2 <- predict(model_sr, newdata=newdataset_2, interval="confidence")
predictions_sr_2=cbind(newdataset_2,predictions_sr_2)
plot(predictions_sr_2$triglyceride,predictions_sr_2$predictions_sr_2)


###############################################################################################
model_sr <- lm(triglyceride ~ ns(log.a6.g.tocopherol, df=10), data=dataset)

newdataset_0=dataset
predictions_sr_0 <- predict(model_sr, newdata=newdataset_0, interval="confidence")

newdataset_1=dataset[sample(nrow(dataset), 500, replace=F), ]
predictions_sr_1 <- predict(model_sr, newdata=newdataset_1, interval="confidence")
predictions_sr_1=cbind(newdataset_1,predictions_sr_1)
plot(predictions_sr_1$triglyceride,predictions_sr_1$predictions_sr_1)

newdataset_2=dataset[sample(nrow(dataset), 100, replace=F), ]
predictions_sr_2 <- predict(model_sr, newdata=newdataset_2, interval="confidence")
predictions_sr_2=cbind(newdataset_2,predictions_sr_2)
plot(predictions_sr_2$triglyceride,predictions_sr_2$predictions_sr_2)



###############################################################################################
model_sr <- glm(triglyceride ~ ns(log.a6.g.tocopherol, df=10), data=dataset)

newdataset_0=dataset
predictions_sr_0 <- add_ci(newdataset_0, model_sr, alpha = 0.05, names = c("lwr", "upr"))

newdataset_1=dataset[sample(nrow(dataset), 500, replace=F), ]
predictions_sr_1 <- add_ci(newdataset_1, model_sr, alpha = 0.05, names = c("lwr", "upr"))
plot(predictions_sr_1$triglyceride,predictions_sr_1$pred)

newdataset_2=dataset[sample(nrow(dataset), 100, replace=F), ]
predictions_sr_2 <- add_ci(newdataset_2, model_sr, alpha = 0.05, names = c("lwr", "upr"))
plot(predictions_sr_2$triglyceride,predictions_sr_2$pred)

