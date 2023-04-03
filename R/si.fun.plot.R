#' plot estimated single index function
#'
#' @param si.ci A data set of estimated index and corresponding single index values
#' @importFrom graphics lines axis
#' @return Single index function plot
#'
#' @examples
#' \donttest{
#' # example to plot estimated single index function
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
#' # plot single index function
#' si.fun.plot(model_1$si.fun)
#' }
#' @keywords partial linear single index
#' @keywords single index function
#' @author Yuyan Wang
#' @export
#'
si.fun.plot <- function(si.ci){
  # si.ci <- model_1$si.fun
  # cut <- 0.02
  si.ci <- si.ci[order(si.ci[, 1]), ]
  plot(si.ci[, c("single_index_estimated")], si.ci[, c("pred")], type="l", lwd = 2,
       xlab = "Single index: u", ylab = paste('\u03A8', "(u)", sep = ""),
       ylim = c(min(si.ci[, c("lwr")] - 0.5), max(si.ci[, c("upr")] + 0.5)))
  graphics::lines(si.ci[, c("single_index_estimated")], si.ci[, c("lwr")], type = "l", lty = 2)
  graphics::lines(si.ci[, c("single_index_estimated")], si.ci[, c("upr")], type = "l", lty = 2)
  graphics::axis(side = 1, at = si.ci[, c("single_index_estimated")], labels = FALSE, NA, tck = 0.016)
}
