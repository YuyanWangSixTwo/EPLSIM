#' plot estimated single index function
#'
#' @param si.ci A data set of estimated index, link value, and CIs
#'
#' @return Single index function plot
#' @export
#' @examples
#' \dontrun{
#' sum("a")
#' }
si.fun.plot <- function(si.ci){
  # si.ci <- model_1$si.fun
  # cut <- 0.02
  si.ci <- si.ci[order(si.ci[, 1]), ]
  plot(si.ci[, c("single_index_estimated")], si.ci[, c("pred")], type="l", lwd = 2,
       xlab = "Single index: u", ylab = paste('\u03A8', "(u)", sep = ""),
       ylim = c(min(si.ci[, c("lwr")] - 0.5), max(si.ci[, c("upr")] + 0.5)))
  lines(si.ci[, c("single_index_estimated")], si.ci[, c("lwr")], type = "l", lty = 2)
  lines(si.ci[, c("single_index_estimated")], si.ci[, c("upr")], type = "l", lty = 2)
  axis(side = 1, at = si.ci[, c("single_index_estimated")], labels = FALSE, NA, tck = 0.016)
}
