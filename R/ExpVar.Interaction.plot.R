#' plot interaction effect of two exposures
#'
#' @param fit Fitted model from PLSI function 'plsi.lr.v1'
#' @param data Original data set
#' @param exp_1 exposure name hoping to be checked
#' @param exp_2 exposure name hoping to be checked
#'
#' @return plot of interaction effect of two exposures with others at average level 0
#' @export
#' @examples
#' \dontrun{
#' sum("a")
#' }
expvar.interaction.plot <- function(fit, data, exp_1, exp_2){
  # fit = model_1; data = dat; exp_1 = "X4_a.tocopherol"; exp_2 = "X3_g.tocopherol"
  x_1_value <- data[, exp_1]; beta_1 <- fit$si.coefficient[exp_1, 1]; x_1_index <- x_1_value * beta_1
  x_2_value <- data[, exp_2]; beta_2 <- fit$si.coefficient[exp_2, 1]; x_2_index <- x_2_value * beta_2

  m2 <- fit$si.fun.model

  x_1_index_dat <- as.data.frame(cbind(x_1_value, x_1_index))
  colnames(x_1_index_dat) <- c("x_1_value", 'single_index_estimated')
  x2_q1_index <- stats::quantile(x_2_value, p = 0.25)*beta_2; x_1_index_dat$index_q1 = x_1_index_dat$single_index_estimated + x2_q1_index
  x2_q2_index <- stats::quantile(x_2_value, p = 0.50)*beta_2; x_1_index_dat$index_q2 = x_1_index_dat$single_index_estimated + x2_q2_index
  x2_q3_index <- stats::quantile(x_2_value, p = 0.75)*beta_2; x_1_index_dat$index_q3 = x_1_index_dat$single_index_estimated + x2_q3_index

  dat_temp = as.data.frame(x_1_index_dat[, c("index_q1")]); colnames(dat_temp) = c('single_index_estimated'); pred_temp = ciTools::add_ci(dat_temp, m2, alpha=0.05); x_1_index_dat$pred_q1 = pred_temp$pred
  dat_temp = as.data.frame(x_1_index_dat[, c("index_q2")]); colnames(dat_temp) = c('single_index_estimated'); pred_temp = ciTools::add_ci(dat_temp, m2, alpha=0.05); x_1_index_dat$pred_q2 = pred_temp$pred
  dat_temp = as.data.frame(x_1_index_dat[, c("index_q3")]); colnames(dat_temp) = c('single_index_estimated'); pred_temp = ciTools::add_ci(dat_temp, m2, alpha=0.05); x_1_index_dat$pred_q3 = pred_temp$pred

  x_1_index_dat <- x_1_index_dat[order(x_1_index_dat[, 1]), ]

  x_2_index_dat <- as.data.frame(cbind(x_2_value, x_2_index))
  colnames(x_2_index_dat) <- c("x_2_value", 'single_index_estimated')
  x1_q1_index <- stats::quantile(x_2_value, p = 0.25)*beta_1; x_2_index_dat$index_q1 = x_2_index_dat$single_index_estimated + x1_q1_index
  x1_q2_index <- stats::quantile(x_2_value, p = 0.50)*beta_1; x_2_index_dat$index_q2 = x_2_index_dat$single_index_estimated + x1_q2_index
  x1_q3_index <- stats::quantile(x_2_value, p = 0.75)*beta_1; x_2_index_dat$index_q3 = x_2_index_dat$single_index_estimated + x1_q3_index

  dat_temp = as.data.frame(x_2_index_dat[, c("index_q1")]); colnames(dat_temp) = c('single_index_estimated'); pred_temp = ciTools::add_ci(dat_temp, m2, alpha = 0.05); x_2_index_dat$pred_q1 = pred_temp$pred
  dat_temp = as.data.frame(x_2_index_dat[, c("index_q2")]); colnames(dat_temp) = c('single_index_estimated'); pred_temp = ciTools::add_ci(dat_temp, m2, alpha = 0.05); x_2_index_dat$pred_q2 = pred_temp$pred
  dat_temp = as.data.frame(x_2_index_dat[, c("index_q3")]); colnames(dat_temp) = c('single_index_estimated'); pred_temp = ciTools::add_ci(dat_temp, m2, alpha = 0.05); x_2_index_dat$pred_q3 = pred_temp$pred

  x_2_index_dat <- x_2_index_dat[order(x_2_index_dat[, 1]), ]

  ymin = min(min(x_1_index_dat[, c("pred_q1", "pred_q2", "pred_q3")]), min(x_2_index_dat[, c("pred_q1", "pred_q2", "pred_q3")]))
  ymax = max(max(x_1_index_dat[, c("pred_q1", "pred_q2", "pred_q3")]), max(x_2_index_dat[, c("pred_q1", "pred_q2", "pred_q3")]))

  graphics::par(mfrow = c(1, 2))
  plot(x_1_index_dat[, c("x_1_value")], x_1_index_dat[, c("pred_q1")], col = 1,
       type = "l", xlab = exp_1, ylab = "Predictied outcome", ylim = c(ymin - 0.5, ymax + 0.5))
  graphics::lines(x_1_index_dat[, c("x_1_value")], x_1_index_dat[, c("pred_q2")], col = 2)
  graphics::lines(x_1_index_dat[, c("x_1_value")], x_1_index_dat[, c("pred_q3")], col = 3)
  graphics::axis(side = 1, at = x_1_index_dat[, c("x_1_value")], labels = FALSE, NA, tck = 0.016)
  graphics::legend("topleft", xpd = T, bty = "n", col = c(1, 2, 3), lty = 1, cex = 0.6,
                   legend = c(paste("Q1 of ", exp_2, sep = ""), paste("Q2 of ", exp_2, sep = ""), paste("Q3 of ", exp_2, sep = "")))

  plot(x_2_index_dat[, c("x_2_value")], x_2_index_dat[, c("pred_q1")], col = 1,
       type = "l", xlab = exp_2, ylab = "Predictied outcome", ylim = c(ymin - 0.5, ymax + 0.5))
  graphics::lines(x_2_index_dat[, c("x_2_value")], x_2_index_dat[, c("pred_q2")], col = 2)
  graphics::lines(x_2_index_dat[, c("x_2_value")], x_2_index_dat[, c("pred_q3")], col = 3)
  graphics::axis(side = 1, at = x_2_index_dat[, c("x_2_value")], labels = FALSE, NA, tck = 0.016)
  graphics::legend("topleft", xpd = TRUE, bty = "n", col = c(1, 2, 3), lty = 1, cex = 0.6,
                   legend = c(paste("Q1 of ", exp_1, sep = ""), paste("Q2 of ", exp_1, sep = ""), paste("Q3 of ", exp_1, sep = "")))

  graphics::par(mfrow = c(1, 1))
}
