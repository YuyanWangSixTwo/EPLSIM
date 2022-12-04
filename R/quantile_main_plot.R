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
  exp_name=c("log.a13.PCB156")
  x_value_spe <- data[,exp_name]
  beta_spe <- fit$beta_results[exp_name,1]
  x_index_spe <- beta_spe*x_value_spe
  summary(x_index_spe)

  x_basis_spe <- as.matrix(ns(x_index_spe, df = fit$link_spline$rank, intercept = TRUE))
  x_link_spe <- as.data.frame(cbind(x_value_spe,x_index_spe,x_basis_spe))

  m2 <- fit$link_spline
  x_link_ci <- add_ci(x_link_spe, m2, alpha = 0.05, names = c("lwr", "upr"))
  x_link_ci <- x_link_ci[order(x_link_ci[,1]),]

  plot(x_link_ci[,c("x_value_spe")], x_link_ci[,c("pred")], type="l",lwd=2,
       xlab=exp_name,ylab="g(index)", ylim = c(min(x_link_ci[,c("lwr")]-10),max(x_link_ci[,c("upr")]+10)))
  lines(x_link_ci[,c("x_value_spe")], x_link_ci[,c("lwr")], type="l", lty=2)
  lines(x_link_ci[,c("x_value_spe")], x_link_ci[,c("upr")], type="l", lty=2)






}
