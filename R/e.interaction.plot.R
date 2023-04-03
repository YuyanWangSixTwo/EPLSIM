#' plot interaction effect of two exposures
#'
#' @param fit Fitted model from function 'plsi.lr.v1'
#' @param data Original data set
#' @param exp_1 exposure name hoping to be checked
#' @param exp_2 exposure name hoping to be checked
#' @importFrom graphics par
#' @return plot of interaction effect of two exposures with others at average level
#'
#' @examples
#' \donttest{
#' # example to plot interaction effect of two exposures
#' data(nhanes.new)
#' dat <- nhanes.new
#'
#' # specify variable names and parameters
#' Y.name <- "log.triglyceride"
#' X.name <- c("X1_trans.b.carotene", "X2_retinol", "X3_g.tocopherol", "X4_a.tocopherol",
#'             "X5_PCB99", "X6_PCB156", "X7_PCB206",
#'             "X8_3.3.4.4.5.pncb", "X9_1.2.3.4.7.8.hxcdf", "X10_2.3.4.6.7.8.hxcdf")
#' Z.name <- c("AGE.c", "SEX.Female", "RACE.NH.Black",
#'            "RACE.MexicanAmerican", "RACE.OtherRace", "RACE.Hispanic" )
#' spline.num = 5
#' spline.degree = 3
#' initial.random.num = 1
#'
#' # run PLSI linear regression
#' set.seed(2023)
#' model_1 <- plsi.lr.v1(data = dat, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
#'                       spline.num, spline.degree, initial.random.num)
#'
#' # plot two exposures' interaction effect
#' e.interaction.plot(model_1, dat, "X4_a.tocopherol", "X3_g.tocopherol")
#' e.interaction.plot(model_1, dat, "X4_a.tocopherol", "X10_2.3.4.6.7.8.hxcdf")
#'
#' # exchange exposures' names
#' e.interaction.plot(model_1, dat, "X8_3.3.4.4.5.pncb", "X6_PCB156")
#' e.interaction.plot(model_1, dat, "X6_PCB156", "X8_3.3.4.4.5.pncb")
#' }
#' @keywords partial linear single index
#' @keywords two exposures
#' @keywords interaction effect
#' @author Yuyan Wang
#' @export
#'
e.interaction.plot <- function(fit, data, exp_1, exp_2){
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

  opar <- par(mfrow = c(1, 2))
  on.exit(par(opar))
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
}
