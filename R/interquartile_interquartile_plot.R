#' plot interquartile effect of specific exposure based on quartile of other exposures
#'
#' @param fit Fitted model from PLSI function 'plsi_lr_v1'
#' @param data Original data set
#'
#' @return plot interquartile effect of specific interquartile exposure
#' @export
#' @examples
#' \dontrun{
#' sum("a")
#' }
interquartile_interquartile_plot <- function(fit, data){
  # fit = model_1; data = dat
  m2 <- fit$link_spline
  beta_est_vec <- as.vector(fit$beta_results[,1])
  X_name <- rownames(fit$beta_results)
  x <- as.matrix(data[,X_name])

  pre_temp <- as.data.frame(matrix(NA,6*length(X_name),4))
  colnames(pre_temp) <- c("Exposrue","Exp_quar","Other_fixed_quar","index_estimated")
  pre_temp$Exposrue <- rep(X_name,each=6)
  pre_temp$Exp_quar <- rep(c(rep(0.25,3),rep(0.75,3)),length(X_name))
  pre_temp$Other_fixed_quar <- rep(c(0.25,0.50,0.75),2*length(X_name))

  for (i in 1:length(X_name)) {
    x_temp <- as.matrix(data[,X_name[i]])
    x_index <- as.vector(x_temp*beta_est_vec[i])

    x_rest <- as.matrix(data[,X_name[-i]])
    beta_rest <- as.vector(beta_est_vec[-i])
    x_rest_quartiles <- apply(x_rest, 2, quantile, probs = c(0.25,0.50,0.75))
    x_rest_index <- as.vector(x_rest_quartiles %*% as.matrix(beta_rest))

    pre_temp[((i-1)*6+1):((i-1)*6+3),c("index_estimated")]=quantile(x_index,p=0.25)+x_rest_index
    pre_temp[((i-1)*6+4):((i-1)*6+6),c("index_estimated")]=quantile(x_index,p=0.75)+x_rest_index
  }

  pre_temp <- add_ci(pre_temp, m2, alpha = 0.05, names = c("lwr", "upr"))
  pre_temp$ci_diff <- pre_temp$upr-pre_temp$pred

  plot_temp <- as.data.frame(matrix(NA,3*length(X_name),6))
  colnames(plot_temp) <- c("Exposrue","Other_quartile","Diff_est","ci_diff","diff_lwr","diff_upr")
  plot_temp$Exposrue <- rep(X_name,each=3)
  plot_temp$Other_quartile <- rep(c('0.25','0.50','0.75'),length(X_name))

  for (i in 1:length(X_name)) {
    plot_temp[((i-1)*3+1):((i-1)*3+3),c("Diff_est")]=pre_temp[((i-1)*6+4):((i-1)*6+6),c("pred")]-pre_temp[((i-1)*6+1):((i-1)*6+3),c("pred")]
    plot_temp[((i-1)*3+1):((i-1)*3+3),c("ci_diff")]=(pre_temp[((i-1)*6+1):((i-1)*6+3),c("ci_diff")]+pre_temp[((i-1)*6+4):((i-1)*6+6),c("ci_diff")])/2
    plot_temp[((i-1)*3+1):((i-1)*3+3),c("diff_lwr")]=plot_temp[((i-1)*3+1):((i-1)*3+3),c("Diff_est")]-plot_temp[((i-1)*3+1):((i-1)*3+3),c("ci_diff")]
    plot_temp[((i-1)*3+1):((i-1)*3+3),c("diff_upr")]=plot_temp[((i-1)*3+1):((i-1)*3+3),c("Diff_est")]+plot_temp[((i-1)*3+1):((i-1)*3+3),c("ci_diff")]
  }

  plot_temp_2 <- as.data.frame(matrix(NA,length(X_name),5))
  colnames(plot_temp_2) <- c("Exposrue","Interq_est","ci_diff","diff_lwr","diff_upr")
  plot_temp_2$Exposrue <- X_name

  for (i in 1:length(X_name)) {
    plot_temp_2[i,c("Interq_est")]=plot_temp[(i-1)*3+3,c("Diff_est")]-plot_temp[(i-1)*3+1,c("Diff_est")]
    plot_temp_2[i,c("ci_diff")]=(plot_temp[(i-1)*3+3,c("ci_diff")]+plot_temp[(i-1)*3+1,c("ci_diff")])/2
    plot_temp_2[i,c("diff_lwr")]=plot_temp_2[i,c("Interq_est")]-plot_temp_2[i,c("ci_diff")]
    plot_temp_2[i,c("diff_upr")]=plot_temp_2[i,c("Interq_est")]+plot_temp_2[i,c("ci_diff")]
  }

  ggplot(data = plot_temp_2, aes(x = Exposrue, y = Interq_est, ymin = diff_lwr, ymax = diff_upr)) +
  # ggplot(data = plot_temp_2, aes(x = Exposrue, y = Interq_est)) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(position = position_dodge(width = 0.5), width = 0.1) +
    coord_flip() +
    # facet_wrap(~Exposrue, ncol = 1, strip.position = "left") +
    ylab("Interquartile effect") +
    xlab("Exposure")
}
