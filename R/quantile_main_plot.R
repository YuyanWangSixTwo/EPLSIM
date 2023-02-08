#' plot main effect based on quantile of exposures
#'
#' @param fit Fitted model from PLSI function 'plsi_lr_v1'
#' @param data Original data set
#' @param exp_name exposure name hoping to check
#'
#' @return plot of main effect with other exposures at average level 0
#' @export
#' @examples
#' \dontrun{
#' sum("a")
#' }
quantile_main_plot <- function(fit, data, exp_name){
  # fit = model_1; data = dat; exp_name=c("log.a7.a.Tocopherol")
  x_value_spe <- data[,exp_name]
  out_value <- boxplot(x_value_spe,range=10,plot=FALSE)$out
  beta_spe <- fit$beta_results[exp_name,1]
  x_index_spe <- beta_spe*x_value_spe

  x_index_dat <- as.data.frame(cbind(x_value_spe,x_index_spe))
  colnames(x_index_dat) <- c("x_value",'index_estimated')

  m2 <- fit$link_spline
  pred_x_dat <- add_ci(x_index_dat, m2, alpha = 0.05, names = c("lwr", "upr"))
  pred_x_dat <- pred_x_dat[order(pred_x_dat[,1]),]
  pred_x_dat <- pred_x_dat[!(pred_x_dat$x_value %in% out_value),]

  plot(pred_x_dat[,c("x_value")], pred_x_dat[,c("pred")], type="l",lwd=2,
       xlab=exp_name,ylab="g(index)", ylim = c(min(pred_x_dat[,c("lwr")]-0.5),max(pred_x_dat[,c("upr")]+0.5)))
  lines(pred_x_dat[,c("x_value")], pred_x_dat[,c("lwr")], type="l", lty=2)
  lines(pred_x_dat[,c("x_value")], pred_x_dat[,c("upr")], type="l", lty=2)
  axis(side=1,at=pred_x_dat[,1],labels=FALSE,NA,tck=0.016)
}
