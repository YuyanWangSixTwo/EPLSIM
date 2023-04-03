#' plot single exposure's main effect
#'
#' @param fit Fitted model from function 'plsi.lr.v1'
#' @param data Original data set
#' @param exp_name exposure name hoping to be plotted
#' @return plot of exposure's main effect with other exposures at average level 0
#'
#' @examples
#' \donttest{
#' # example to plot some exposure's main effect
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
#' # plot some exposure's main effect
#' e.main.plot(model_1, dat, exp_name = c("X4_a.tocopherol"))
#' e.main.plot(model_1, dat, exp_name = c("X5_PCB99"))
#' e.main.plot(model_1, dat, exp_name = c("X10_2.3.4.6.7.8.hxcdf"))
#' }
#' @keywords partial linear single index
#' @keywords one exposure
#' @keywords main effect
#' @author Yuyan Wang
#' @export
#'
e.main.plot <- function(fit, data, exp_name){
  # fit = model_1; data = dat; exp_name = c("X4_a.tocopherol")
  x_value_spe <- data[, exp_name]
  out_value <- graphics::boxplot(x_value_spe, range = 10, plot = FALSE)$out
  beta_spe <- fit$si.coefficient[exp_name,  1]
  x_index_spe <- beta_spe * x_value_spe

  x_index_dat <- as.data.frame(cbind(x_value_spe, x_index_spe))
  colnames(x_index_dat) <- c("x_value", 'single_index_estimated')

  m2 <- fit$si.fun.model
  pred_x_dat <- ciTools::add_ci(x_index_dat, m2, alpha = 0.05, names = c("lwr", "upr"))
  pred_x_dat <- pred_x_dat[order(pred_x_dat[, 1]), ]
  pred_x_dat <- pred_x_dat[!(pred_x_dat$x_value %in% out_value), ]

  plot(pred_x_dat[, c("x_value")], pred_x_dat[, c("pred")], type = "l", lwd = 2,
       xlab = exp_name, ylab = "Predicted outcome",
       ylim = c(min(pred_x_dat[, c("lwr")] - 0.5), max(pred_x_dat[, c("upr")] + 0.5)))
  graphics::lines(pred_x_dat[, c("x_value")], pred_x_dat[, c("lwr")], type = "l", lty = 2)
  graphics::lines(pred_x_dat[, c("x_value")], pred_x_dat[, c("upr")], type = "l", lty = 2)
  graphics::axis(side = 1, at = pred_x_dat[, 1], labels = FALSE, NA, tck = 0.016)
}
