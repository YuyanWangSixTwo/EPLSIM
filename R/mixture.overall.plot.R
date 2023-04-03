#' plot mixture's overall effect based on quantile of exposures
#'
#' @param fit Fitted model from function 'plsi.lr.v1'
#' @param data Original data set
#' @return plot of predicted outcomes based on quantile of exposures
#'
#' @examples
#' \donttest{
#' # example to plot mixture's overall effect
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
#' # plot mixture overall effect
#' mixture.overall.plot(model_1, dat)
#' }
#' @keywords partial linear single index
#' @keywords mixture overall effect
#' @author Yuyan Wang
#' @export
#'
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
