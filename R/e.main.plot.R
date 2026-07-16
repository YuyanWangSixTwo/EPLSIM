#' plot single exposure's main effect
#'
#' @param fit Fitted model from \code{plsi.lr.auto()}, \code{plsi.logistic.auto()},
#'   or \code{plsi.log.auto()}
#' @param data Original data set
#' @param exp_name exposure name hoping to be plotted
#' @param type Which outcome scale to plot. \code{"linear"} (default) plots the
#'   predicted (continuous) outcome, for a \code{fit} from \code{plsi.lr.auto()}.
#'   \code{"logistic"} plots the predicted probability, for a \code{fit} from
#'   \code{plsi.logistic.auto()}. \code{"log"} plots the predicted count, for
#'   a \code{fit} from \code{plsi.log.auto()}.
#' @param outlier.range \code{range} argument passed to \code{boxplot()} to
#'   identify and drop extreme values of the raw exposure before plotting.
#'   Default 10 (a permissive threshold; increase to drop fewer points, or set
#'   to \code{Inf} to keep all points).
#' @return plot of exposure's main effect with other exposures at their
#'   confounder-adjusted reference level. Invisibly returns the data frame
#'   used to build the plot.
#'
#' @examples
#' \dontrun{
#' # example to plot some exposure's main effect -- continuous outcome
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
#'
#' # plot some exposure's main effect -- predicted (continuous) outcome
#' e.main.plot(model_lr_auto, data, exp_name = c("X4_a.tocopherol"), type = "linear")
#' e.main.plot(model_lr_auto, data, exp_name = c("X5_PCB99"), type = "linear")
#' e.main.plot(model_lr_auto, data, exp_name = c("X10_2.3.4.6.7.8.hxcdf"), type = "linear")
#' }
#' @keywords partial linear single index
#' @keywords one exposure
#' @keywords main effect
#' @author Yuyan Wang
#' @export
#'
e.main.plot <- function(fit, data, exp_name, type = c("linear", "logistic", "log"), outlier.range = 10){
  # fit = model_lr_auto; data = data; exp_name = c("X4_a.tocopherol"); type = "linear"
  # fit = model_logistic_auto; data = data; exp_name = c("X4_a.tocopherol"); type = "logistic"
  # fit = model_log_auto; data = data; exp_name = c("X4_a.tocopherol"); type = "log"
  type <- match.arg(type)

  x_value_spe <- data[, exp_name]
  beta_spe <- fit$si.coefficient[exp_name, "Estimate"]
  x_index_spe <- beta_spe * x_value_spe

  newdat <- data.frame(single_index_estimated = x_index_spe)
  if (type %in% c("logistic", "log")) {
    # fit$si.fun.model from plsi.logistic.auto()/plsi.log.auto() has an
    # offset(confounder_offset) term in its formula, so predict() needs that
    # column in newdata too. offset = 0 matches how si.fun itself was
    # predicted -- the confounder-adjusted reference curve, consistent with
    # the training fit.
    newdat$confounder_offset <- 0
  }
  p <- stats::predict(fit$si.fun.model, newdata = newdat, type = "link", se.fit = TRUE)

  pred_x_dat <- data.frame(
    x_value = x_value_spe,
    single_index_estimated = x_index_spe,
    fit = p$fit,
    se  = p$se.fit
  )
  pred_x_dat$lwr <- pred_x_dat$fit - stats::qnorm(0.975) * pred_x_dat$se
  pred_x_dat$upr <- pred_x_dat$fit + stats::qnorm(0.975) * pred_x_dat$se

  if (type == "logistic") {
    # Back-transform the logit-scale CI (not the SE) to the probability scale,
    # so the interval stays inside [0, 1] -- same convention as si.fun.
    pred_x_dat$prob.fit <- stats::plogis(pred_x_dat$fit)
    pred_x_dat$prob.lwr <- stats::plogis(pred_x_dat$lwr)
    pred_x_dat$prob.upr <- stats::plogis(pred_x_dat$upr)
  } else if (type == "log") {
    # Back-transform the log-scale CI (not the SE) to the count scale, so the
    # interval stays non-negative -- same convention as si.fun.
    pred_x_dat$count.fit <- exp(pred_x_dat$fit)
    pred_x_dat$count.lwr <- exp(pred_x_dat$lwr)
    pred_x_dat$count.upr <- exp(pred_x_dat$upr)
  }

  # Drop extreme values of the raw exposure before plotting (previously
  # referenced an undefined 'out_value' -- restored here).
  out_value <- graphics::boxplot(x_value_spe, range = outlier.range, plot = FALSE)$out
  pred_x_dat <- pred_x_dat[!(pred_x_dat$x_value %in% out_value), ]
  pred_x_dat <- pred_x_dat[order(pred_x_dat$x_value), ]

  if (type == "linear") {
    y_col <- "fit"; lwr_col <- "lwr"; upr_col <- "upr"
    ylab <- "Predicted outcome"
    pad <- 0.5
    y_lim <- c(min(pred_x_dat[[lwr_col]]) - pad, max(pred_x_dat[[upr_col]]) + pad)
  } else if (type == "logistic") {
    y_col <- "prob.fit"; lwr_col <- "prob.lwr"; upr_col <- "prob.upr"
    ylab <- "Predicted probability"
    # Probabilities are bounded in [0, 1] -- small fixed pad, clamped, instead
    # of the linear case's +/-0.5.
    pad <- 0.02
    y_lim <- c(max(0, min(pred_x_dat[[lwr_col]]) - pad), min(1, max(pred_x_dat[[upr_col]]) + pad))
  } else {
    y_col <- "count.fit"; lwr_col <- "count.lwr"; upr_col <- "count.upr"
    ylab <- "Predicted count"
    # Counts are bounded below at 0 -- clamp the lower limit instead of the
    # linear case's symmetric +/-0.5, which could push the axis negative.
    pad <- 0.5
    y_lim <- c(max(0, min(pred_x_dat[[lwr_col]]) - pad), max(pred_x_dat[[upr_col]]) + pad)
  }

  plot(pred_x_dat$x_value, pred_x_dat[[y_col]], type = "l", lwd = 2,
       xlab = exp_name, ylab = ylab, ylim = y_lim)
  graphics::lines(pred_x_dat$x_value, pred_x_dat[[lwr_col]], type = "l", lty = 2)
  graphics::lines(pred_x_dat$x_value, pred_x_dat[[upr_col]], type = "l", lty = 2)
  graphics::axis(side = 1, at = pred_x_dat$x_value, labels = FALSE, NA, tck = 0.016)

  invisible(pred_x_dat)
}
