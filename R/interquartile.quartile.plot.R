#' plot interquartile effect of specific exposure based on quartile of other exposures
#'
#' @param fit Fitted model from function 'plsi.lr.v1'
#' @param data Original data set
#' @importFrom stats quantile
#' @return plot of main interquartile effect of exposure based on quartile of other exposures
#'
#' @examples
#' \donttest{
#' # example to interquartile effect based on quartile of other exposures
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
#' # plot interquartile quartile
#' interquartile.quartile.plot(model_1, dat)
#' }
#' @keywords partial linear single index
#' @keywords interquartile quartile effect
#' @keywords interaction
#' @author Yuyan Wang
#' @export
#'
interquartile.quartile.plot <- function(fit, data){
  # fit = model_1; data = dat
  m2 <- fit$si.fun.model
  beta_est_vec <- as.vector(fit$si.coefficient[, 1])
  X_name <- rownames(fit$si.coefficient)
  x <- as.matrix(data[, X_name])

  pre_temp <- as.data.frame(matrix(NA, 6 * length(X_name), 4))
  colnames(pre_temp) <- c("Exposrue_Name", "Exp_quar", "Other_fixed_quar", "single_index_estimated")
  pre_temp$Exposrue_Name <- rep(X_name, each = 6)
  pre_temp$Exp_quar <- rep(c(rep(0.25, 3), rep(0.75, 3)), length(X_name))
  pre_temp$Other_fixed_quar <- rep(c(0.25, 0.50, 0.75), 2 * length(X_name))

  for (i in 1:length(X_name)) {
    x_temp <- as.matrix(data[, X_name[i]])
    x_index <- as.vector(x_temp * beta_est_vec[i])

    x_rest <- as.matrix(data[,X_name[-i]])
    beta_rest <- as.vector(beta_est_vec[-i])
    x_rest_quartiles <- apply(x_rest, 2, quantile, probs = c(0.25, 0.50, 0.75))
    x_rest_index <- as.vector(x_rest_quartiles %*% as.matrix(beta_rest))

    pre_temp[((i - 1)*6 + 1):((i - 1)*6 + 3), c("single_index_estimated")] = stats::quantile(x_index, p = 0.25) + x_rest_index
    pre_temp[((i - 1)*6 + 4):((i - 1)*6 + 6), c("single_index_estimated")] = stats::quantile(x_index, p = 0.75) + x_rest_index
  }

  pre_temp <- ciTools::add_ci(pre_temp, m2, alpha = 0.05, names = c("lwr", "upr"))
  pre_temp$ci_diff <- pre_temp$upr - pre_temp$pred

  plot_temp <- as.data.frame(matrix(NA, 3 * length(X_name), 6))
  colnames(plot_temp) <- c("Exposrue_Name", "Other_quartile", "Diff_est", "ci_diff", "Diff_lwr", "Diff_upr")
  plot_temp$Exposrue_Name <- rep(X_name, each = 3)
  plot_temp$Other_quartile <- rep(c('Q1', 'Q2', 'Q3'), length(X_name))

  for (i in 1:length(X_name)) {
    plot_temp[((i-1)*3+1):((i-1)*3+3),c("Diff_est")]=pre_temp[((i-1)*6+4):((i-1)*6+6),c("pred")]-pre_temp[((i-1)*6+1):((i-1)*6+3),c("pred")]
    plot_temp[((i-1)*3+1):((i-1)*3+3),c("ci_diff")]=(pre_temp[((i-1)*6+1):((i-1)*6+3),c("ci_diff")]+pre_temp[((i-1)*6+4):((i-1)*6+6),c("ci_diff")])/2
    plot_temp[((i-1)*3+1):((i-1)*3+3),c("Diff_lwr")]=plot_temp[((i-1)*3+1):((i-1)*3+3),c("Diff_est")]-plot_temp[((i-1)*3+1):((i-1)*3+3),c("ci_diff")]
    plot_temp[((i-1)*3+1):((i-1)*3+3),c("Diff_upr")]=plot_temp[((i-1)*3+1):((i-1)*3+3),c("Diff_est")]+plot_temp[((i-1)*3+1):((i-1)*3+3),c("ci_diff")]
  }

  plot_temp$Exposrue_Name = factor(plot_temp$Exposrue_Name, levels = X_name)

  final_plot <- ggplot2::ggplot(data = plot_temp, ggplot2::aes(x = plot_temp$Exposrue_Name, colour = plot_temp$Other_quartile, y = plot_temp$Diff_est, ymin = plot_temp$Diff_lwr, ymax = plot_temp$Diff_upr)) +
  # ggplot(data = plot_temp, aes(x = Exposrue_Name, colour = Other_quartile, y = Diff_est)) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.5)) +
    ggplot2::geom_errorbar(position = ggplot2::position_dodge(width = 0.5), width = 0.1) +
    ggplot2::coord_flip() +
    # facet_wrap(~Exposrue_Name, ncol = 1, strip.position = "left") +
    ggplot2::ylab("Difference of predicted outcome") +
    ggplot2::xlab("Exposure") +
    ggplot2::scale_color_manual(values = c("red", "blue", "green"), name = NULL)

  suppressWarnings(print(final_plot))
}
