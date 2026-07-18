#' Plot estimated single index function
#'
#' @param si.ci A data set of estimated index and corresponding single index
#'   values, typically the \code{si.fun} element returned by
#'   \code{plsi.lr.auto()}, \code{plsi.logistic.auto()}, or \code{plsi.log.auto()}
#'   (or the analogous \code{plsi.lr.v2()} output for the linear case).
#' @param type Which outcome scale to plot. \code{"linear"} (default) plots
#'   the predicted outcome \code{g(u)} using the \code{fit}/\code{lwr}/\code{upr}
#'   columns, as returned for a continuous outcome. \code{"logistic"} plots
#'   the predicted probability using the \code{prob.fit}/\code{prob.lwr}/
#'   \code{prob.upr} columns returned by \code{plsi.logistic.auto()}, with the
#'   y-axis bounded between 0 and 1. \code{"log"} plots the predicted count using
#'   the \code{count.fit}/\code{count.lwr}/\code{count.upr} columns returned
#'   by \code{plsi.log.auto()}, with the y-axis bounded below at 0.
#' @param xlab X-axis label. Default \code{"Single index: u"}.
#' @param ylab Y-axis label. If \code{NULL} (default), a label appropriate to
#'   \code{type} is used automatically.
#' @importFrom graphics lines axis
#' @return Single index function plot
#'
#' @examples
#' \dontrun{
#' # example to plot estimated single index function of continuous outcome
#' data(nhanes.new)
#' data <- nhanes.new
#'
#' Y.name <- "log.triglyceride"
#' X.name <- c("X1_trans.b.carotene", "X2_retinol", "X3_g.tocopherol", "X4_a.tocopherol",
#'             "X5_PCB99", "X6_PCB156", "X7_PCB206",
#'             "X8_3.3.4.4.5.pncb", "X9_1.2.3.4.7.8.hxcdf", "X10_2.3.4.6.7.8.hxcdf")
#' Z.name <- c("AGE.c", "SEX.Female", "RACE.NH.Black",
#'            "RACE.MexicanAmerican", "RACE.OtherRace", "RACE.Hispanic" )
#'
#' k <- 10
#' bs <- "cr"
#' initial.random.num <- 1
#' seed = 2026
#'
#' model_lr_auto <- plsi.lr.auto(data = data, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
#'                       k = k, bs = bs, initial.random.num = initial.random.num, seed = seed)
#' # plot single index function of predicted (continuous) outcome
#' si.fun.plot(model_lr_auto$si.fun, type = "linear")
#' }
#' @keywords partial linear single index
#' @keywords single index function
#' @author Yuyan Wang
#' @export
#'
si.fun.plot <- function(si.ci, type = c("linear", "logistic", "log"), xlab = "Single index: u", ylab = NULL) {
  # si.ci <- model_lr_auto$si.fun          # type = "linear"
  # si.ci <- model_logistic_auto$si.fun    # type = "logistic"
  # si.ci <- model_log_auto$si.fun         # type = "log"
  type <- match.arg(type)

  if (type == "linear") {
    fit_col <- "fit"; lwr_col <- "lwr"; upr_col <- "upr"
    needed <- c("single_index_estimated", fit_col, lwr_col, upr_col)
    if (!all(needed %in% colnames(si.ci))) {
      stop("si.ci is missing column(s) required for type = 'linear': ",
           paste(setdiff(needed, colnames(si.ci)), collapse = ", "), ". ",
           "Did you mean type = 'logistic' or type = 'log'?")
    }
    if (is.null(ylab)) ylab <- "Predicted outcome: g(u)"
    pad <- 0.5
    y_lim <- c(min(si.ci[[lwr_col]]) - pad, max(si.ci[[upr_col]]) + pad)
  } else if (type == "logistic") {
    fit_col <- "prob.fit"; lwr_col <- "prob.lwr"; upr_col <- "prob.upr"
    needed <- c("single_index_estimated", fit_col, lwr_col, upr_col)
    if (!all(needed %in% colnames(si.ci))) {
      stop("si.ci is missing column(s) required for type = 'logistic': ",
           paste(setdiff(needed, colnames(si.ci)), collapse = ", "), ". ",
           "Did you mean type = 'linear', type = 'log', or pass si.fun from plsi.logistic.auto()?")
    }
    if (is.null(ylab)) ylab <- "Predicted probability: P(Y = 1 | u)"
    # Probabilities are bounded in [0, 1] -- use a small fixed pad instead of
    # the linear case's +/-0.5, which would push the axis outside that range.
    pad <- 0.02
    y_lim <- c(max(0, min(si.ci[[lwr_col]]) - pad), min(1, max(si.ci[[upr_col]]) + pad))
  } else {
    fit_col <- "count.fit"; lwr_col <- "count.lwr"; upr_col <- "count.upr"
    needed <- c("single_index_estimated", fit_col, lwr_col, upr_col)
    if (!all(needed %in% colnames(si.ci))) {
      stop("si.ci is missing column(s) required for type = 'log': ",
           paste(setdiff(needed, colnames(si.ci)), collapse = ", "), ". ",
           "Did you mean type = 'linear', type = 'logistic', or pass si.fun from plsi.log.auto()?")
    }
    if (is.null(ylab)) ylab <- "Predicted count"
    # Counts are bounded below at 0 -- clamp the lower limit instead of the
    # linear case's symmetric +/-0.5, which could push the axis negative.
    pad <- 0.5
    y_lim <- c(max(0, min(si.ci[[lwr_col]]) - pad), max(si.ci[[upr_col]]) + pad)
  }

  si.ci <- si.ci[order(si.ci[, "single_index_estimated"]), ]

  plot(si.ci[, "single_index_estimated"], si.ci[, fit_col], type = "l", lwd = 2,
       xlab = xlab, ylab = ylab, ylim = y_lim)
  graphics::lines(si.ci[, "single_index_estimated"], si.ci[, lwr_col], type = "l", lty = 2)
  graphics::lines(si.ci[, "single_index_estimated"], si.ci[, upr_col], type = "l", lty = 2)
  graphics::axis(side = 1, at = si.ci[, "single_index_estimated"], labels = FALSE, NA, tck = 0.016)
}
