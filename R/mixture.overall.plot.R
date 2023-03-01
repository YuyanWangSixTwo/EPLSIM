#' plot mixture's overall effect based on quantile of exposures
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
mixture.overall.plot <- function(fit, data){
  # fit = model_1; data = dat
  beta_est_vec <- as.vector(fit$si.coefficient[, 1])
  X_name <- rownames(fit$si.coefficient)
  x <- as.matrix(data[, X_name])
  quants <- seq(0.1, 0.9, by = 0.05)
  x_quantiles <- apply(x, 2, stats::quantile, probs = quants)
  index_quantile <- x_quantiles %*% beta_est_vec

  index_dat <- as.data.frame(cbind(quants, index_quantile))
  colnames(index_dat) <- c("quants", 'single_index_estimated')

  m2 <- fit$si.fun.model
  pred_index_dat <- ciTools::add_ci(index_dat, m2, alpha = 0.05, names = c("lwr", "upr"))

  final_plot <- ggplot2::ggplot(pred_index_dat, ggplot2::aes(x = pred_index_dat$quants, y = pred_index_dat$pred)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = pred_index_dat$lwr, ymax = pred_index_dat$upr), width = .01) +
    ggplot2::geom_point(size = 2, shape = 21, fill = "white") +
    ggplot2::ggtitle("Mixture overall effect") +
    ggplot2::labs(y = "Predicted outcome", x = "Quantile levels") +
    ggplot2::scale_x_continuous(breaks = seq(0.1, 0.9, by =  0.1))

  suppressWarnings(print(final_plot))
}
