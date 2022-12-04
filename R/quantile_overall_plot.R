#' plot overall effect based on quantile of exposures
#'
#' @param fit Fitted model from PLSI function 'plsi_lr_v1'
#' @param data Original data set
#'
#' @return plot of predicted outcome based on quantile of exposures
#' @export
#' @examples
#' \dontrun{
#' sum("a")
#' }
quantile_overall_plot <- function(fit, data){
  # fit = model_1; data = dat
  beta_est_vec <- as.vector(fit$beta_results[,1])
  X_name <- rownames(fit$beta_results)
  x <- as.matrix(data[,X_name])
  quants <- seq(0.1,0.9,by=0.05)
  x_quantiles <- apply(x, 2, quantile, probs = quants)
  index_quantile <- x_quantiles %*% beta_est_vec

  link_ci <- fit$link_ci
  link_ci <- link_ci[order(link_ci[,1]),]
  index_est_vec <- link_ci[,1]

  focus_vec <- as.vector(rep(NA,length(index_quantile)))
  for (l in 1:length(index_quantile)) {
    focus_vec[l] <- index_est_vec[abs(index_est_vec-index_quantile[l])==min(abs(index_est_vec-index_quantile[l]))]
  }

  focus_pred <- link_ci[link_ci$index_est %in% focus_vec,]
  quant_overall <- cbind(quants,index_quantile,focus_pred)

  pd <- position_dodge(0.1)

  ggplot(quant_overall, aes(x=quants, y=pred)) +
    geom_errorbar(aes(ymin=lwr, ymax=upr), width=.01) +
    geom_point(size=2, shape=21, fill="white") +
    ggtitle("Overall effect") +
    labs(y = "Predicted outcome, g(index)", x = "Quantiles of Xs") +
    scale_x_continuous(breaks=seq(0.1,0.9,by=0.1))
}
