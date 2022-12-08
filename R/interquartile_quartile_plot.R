#' plot interquartile effect of specific exposure based on quartile of other exposures
#'
#' @param fit Fitted model from PLSI function 'plsi_lr_v1'
#' @param data Original data set
#'
#' @return plot of main effect with other exposures at average level 0
#' @export
#' @examples
#' \dontrun{
#' sum("a")
#' }
interquartile_quartile_plot <- function(fit, data, X){
  # fit = model_1; data = dat
  m2 <- fit$link_spline
  beta_est_vec <- as.vector(fit$beta_results[,1])
  X_name <- rownames(fit$beta_results)
  x <- as.matrix(data[,X_name])

  plot_temp <- as.data.frame(matrix(NA,3*length(X_name),6))
  colnames(plot_temp) <- c("Exposrue","Other_fixed_quar","Est_75","Est_25","Est_diff")
  plot_temp$Exposrue <- rep(X_name,each=3)
  plot_temp$Other_fixed_quar <- rep(c(0.25,0.50,0.75),length(X_name))

  x_rest <- as.matrix(data[,X_name[-i]])
  beta_rest <- as.vector(beta_est_vec[-i])
  x_rest_quartiles <- apply(x_rest, 2, quantile, probs = c(0.25,0.50,0.75))
  x_rest_index <- as.vector(x_rest_quartiles %*% as.matrix(beta_rest))






  for (i in 1:length(X_name)) {
    x_temp <- as.matrix(data[,X_name[i]])
    x_index <- as.vector(x_temp*beta_est_vec[i])

    for (j in 1:3) {
      pre_temp <- as.data.frame(as.matrix(x_index+x_rest_index[j]))
      colnames(pre_temp) <- c("index_estimated")
      pre_temp <- add_ci(pre_temp, m2, alpha = 0.05, names = c("lwr", "upr"))
      eff_temp <- pre_temp

      quantile(pre_temp$pred,0.75)-quantile(pre_temp$pred,0.25)
      quantile(pre_temp$lwr,0.75)-quantile(pre_temp$lwr,0.25)
      quantile(pre_temp$upr,0.75)-quantile(pre_temp$upr,0.25)

      temp=pre_temp
      temp$dev_1=temp$upr-temp$pred
      temp$dev_2=temp$pred-temp$lwr



    }



    plot_temp[(3*(i-1)+1):(3*i),c("index_estimated")] <- x_rest_index+x_temp_index
  }



  pred_index_dat <- add_ci(plot_temp, m2, alpha = 0.05, names = c("lwr", "upr"))







  x_1_value <- data[,exp_1];beta_1 <- fit$beta_results[exp_1,1];x_1_index <- x_1_value*beta_1
  x_2_value <- data[,exp_2];beta_2 <- fit$beta_results[exp_2,1];x_2_index <- x_2_value*beta_2

  m2 <- fit$link_spline

  x_1_index_dat <- as.data.frame(cbind(x_1_value,x_1_index))
  colnames(x_1_index_dat) <- c("x_1_value",'index_estimated')
  x2_q1_index <- quantile(x_2_value,p=0.25)*beta_2;x_1_index_dat$index_q1=x_1_index_dat$index_estimated+x2_q1_index
  x2_q2_index <- quantile(x_2_value,p=0.50)*beta_2;x_1_index_dat$index_q2=x_1_index_dat$index_estimated+x2_q2_index
  x2_q3_index <- quantile(x_2_value,p=0.75)*beta_2;x_1_index_dat$index_q3=x_1_index_dat$index_estimated+x2_q3_index

  dat_temp=as.data.frame(x_1_index_dat[,c("index_q1")]);colnames(dat_temp)=c('index_estimated');pred_temp=add_ci(dat_temp,m2,alpha=0.05);x_1_index_dat$pred_q1=pred_temp$pred
  dat_temp=as.data.frame(x_1_index_dat[,c("index_q2")]);colnames(dat_temp)=c('index_estimated');pred_temp=add_ci(dat_temp,m2,alpha=0.05);x_1_index_dat$pred_q2=pred_temp$pred
  dat_temp=as.data.frame(x_1_index_dat[,c("index_q3")]);colnames(dat_temp)=c('index_estimated');pred_temp=add_ci(dat_temp,m2,alpha=0.05);x_1_index_dat$pred_q3=pred_temp$pred

  x_1_index_dat <- x_1_index_dat[order(x_1_index_dat[,1]),]

  x_2_index_dat <- as.data.frame(cbind(x_2_value,x_2_index))
  colnames(x_2_index_dat) <- c("x_2_value",'index_estimated')
  x1_q1_index <- quantile(x_2_value,p=0.25)*beta_1;x_2_index_dat$index_q1=x_2_index_dat$index_estimated+x1_q1_index
  x1_q2_index <- quantile(x_2_value,p=0.50)*beta_1;x_2_index_dat$index_q2=x_2_index_dat$index_estimated+x1_q2_index
  x1_q3_index <- quantile(x_2_value,p=0.75)*beta_1;x_2_index_dat$index_q3=x_2_index_dat$index_estimated+x1_q3_index

  dat_temp=as.data.frame(x_2_index_dat[,c("index_q1")]);colnames(dat_temp)=c('index_estimated');pred_temp=add_ci(dat_temp,m2,alpha=0.05);x_2_index_dat$pred_q1=pred_temp$pred
  dat_temp=as.data.frame(x_2_index_dat[,c("index_q2")]);colnames(dat_temp)=c('index_estimated');pred_temp=add_ci(dat_temp,m2,alpha=0.05);x_2_index_dat$pred_q2=pred_temp$pred
  dat_temp=as.data.frame(x_2_index_dat[,c("index_q3")]);colnames(dat_temp)=c('index_estimated');pred_temp=add_ci(dat_temp,m2,alpha=0.05);x_2_index_dat$pred_q3=pred_temp$pred

  x_2_index_dat <- x_2_index_dat[order(x_2_index_dat[,1]),]

  ymin=min(min(x_1_index_dat[,c("pred_q1","pred_q2","pred_q3")]),min(x_2_index_dat[,c("pred_q1","pred_q2","pred_q3")]))
  ymax=max(max(x_1_index_dat[,c("pred_q1","pred_q2","pred_q3")]),max(x_2_index_dat[,c("pred_q1","pred_q2","pred_q3")]))

  par(mfrow = c(1, 2))
  plot(x_1_index_dat[,c("x_1_value")],x_1_index_dat[,c("pred_q1")],col=1,
       type="l",xlab=exp_1,ylab="g(index)",ylim=c(ymin-10,ymax+10))
  lines(x_1_index_dat[,c("x_1_value")],x_1_index_dat[,c("pred_q2")],col=2)
  lines(x_1_index_dat[,c("x_1_value")],x_1_index_dat[,c("pred_q3")],col=3)
  axis(side=1,at=x_1_index_dat[,c("x_1_value")],labels=FALSE,NA,tck=0.016)
  legend("bottomright",xpd=TRUE,bty="n",col=c(1,2,3),lty=1,cex=0.6,legend=c(paste("Q1 of ",exp_2,sep=""),paste("Q2 of ",exp_2,sep=""),paste("Q3 of ",exp_2,sep="")))

  plot(x_2_index_dat[,c("x_2_value")],x_2_index_dat[,c("pred_q1")],col=1,
       type="l",xlab=exp_2,ylab="g(index)",ylim=c(ymin-10,ymax+10))
  lines(x_2_index_dat[,c("x_2_value")],x_2_index_dat[,c("pred_q2")],col=2)
  lines(x_2_index_dat[,c("x_2_value")],x_2_index_dat[,c("pred_q3")],col=3)
  axis(side=1,at=x_2_index_dat[,c("x_2_value")],labels=FALSE,NA,tck=0.016)
  legend("bottomright",xpd=TRUE,bty="n",col=c(1,2,3),lty=1,cex=0.6,legend=c(paste("Q1 of ",exp_1,sep=""),paste("Q2 of ",exp_1,sep=""),paste("Q3 of ",exp_1,sep="")))
}
