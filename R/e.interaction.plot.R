#' plot interaction effect of two exposures
#'
#' @param fit Fitted model from \code{plsi.lr.auto()}, \code{plsi.logistic.auto()},
#'   or \code{plsi.log.auto()}
#' @param data Original data set
#' @param exp_1 exposure name hoping to be checked
#' @param exp_2 exposure name hoping to be checked
#' @param type Which outcome scale to plot. \code{"linear"} (default) plots the
#'   predicted (continuous) outcome, for a \code{fit} from \code{plsi.lr.auto()}.
#'   \code{"logistic"} plots the predicted probability, for a \code{fit} from
#'   \code{plsi.logistic.auto()}. \code{"log"} plots the predicted count, for
#'   a \code{fit} from \code{plsi.log.auto()}.
#' @importFrom graphics par
#' @return plot of interaction effect of two exposures with others at average level
#'
#' @examples
#' \donttest{
#' # example to plot interaction effect of two exposures -- continuous outcome
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
#' # plot two exposures' interaction effect -- predicted (continuous) outcome
#' e.interaction.plot(model_lr_auto, data, "X4_a.tocopherol", "X3_g.tocopherol", type = "linear")
#' e.interaction.plot(model_lr_auto, data, "X4_a.tocopherol", "X10_2.3.4.6.7.8.hxcdf", type = "linear")
#'
#' # exchange exposures' names
#' e.interaction.plot(model_lr_auto, data, "X8_3.3.4.4.5.pncb", "X6_PCB156", type = "linear")
#' e.interaction.plot(model_lr_auto, data, "X6_PCB156", "X8_3.3.4.4.5.pncb", type = "linear")
#'
#' # example to plot interaction effect of two exposures -- binary outcome
#' data$high.triglyceride <- as.numeric(
#'   data$log.triglyceride > stats::quantile(data$log.triglyceride, 2 / 3)
#' )
#' model_logistic_auto <- plsi.logistic.auto(data = data, Y.name = "high.triglyceride",
#'                       X.name = X.name, Z.name = Z.name,
#'                       k = k, bs = bs, initial.random.num = initial.random.num, seed = seed)
#'
#' # plot two exposures' interaction effect -- predicted probability
#' e.interaction.plot(model_logistic_auto, data, "X4_a.tocopherol", "X3_g.tocopherol", type = "logistic")
#'
#' # example to plot interaction effect of two exposures -- count outcome
#' set.seed(2026)
#' beta_true <- c(0.30, -0.20, 0.10, 0.40, -0.30, 0.20, -0.10, 0.25, -0.15, 0.35)
#' beta_true <- beta_true / sqrt(sum(beta_true^2))
#' x_std <- scale(data[, X.name])
#' single_index_true <- as.vector(x_std %*% beta_true)
#' log_rate <- 0.3 + 0.4 * sin(single_index_true) + 0.05 * data$AGE.c
#' data$n.events <- stats::rpois(nrow(data), lambda = exp(log_rate))
#' model_log_auto <- plsi.log.auto(data = data, Y.name = "n.events", X.name = X.name,
#'                       Z.name = Z.name, family = "nb", k = k, bs = bs,
#'                       initial.random.num = initial.random.num, seed = seed)
#'
#' # plot two exposures' interaction effect -- predicted count
#' e.interaction.plot(model_log_auto, data, "X4_a.tocopherol", "X3_g.tocopherol", type = "log")
#' }
#' @keywords partial linear single index
#' @keywords two exposures
#' @keywords interaction effect
#' @author Yuyan Wang
#' @export
#'
e.interaction.plot <- function(fit, data, exp_1, exp_2, type = c("linear", "logistic", "log")){
  # fit = model_lr_auto; data = data; exp_1 = "X4_a.tocopherol"; exp_2 = "X3_g.tocopherol"; type = "linear"
  # fit = model_logistic_auto; data = data; exp_1 = "X4_a.tocopherol"; exp_2 = "X3_g.tocopherol"; type = "logistic"
  # fit = model_log_auto; data = data; exp_1 = "X4_a.tocopherol"; exp_2 = "X3_g.tocopherol"; type = "log"
  type <- match.arg(type)

  x_1_value <- data[, exp_1]; beta_1 <- fit$si.coefficient[exp_1, 1]; x_1_index <- x_1_value * beta_1
  x_2_value <- data[, exp_2]; beta_2 <- fit$si.coefficient[exp_2, 1]; x_2_index <- x_2_value * beta_2

  m2 <- fit$si.fun.model

  # predict.gam(se.fit=TRUE) directly, instead of ciTools::add_ci(), since
  # ciTools does not support "gam" objects from mgcv (it covers lm/glm/
  # lmerMod/glmerMod). For type = "logistic"/"log", si.fun.model has an
  # offset(confounder_offset) term, so newdata needs that column too; offset
  # = 0 is the confounder-adjusted reference curve, consistent with how
  # si.fun/e.main.plot() predict from this same model. The fitted value is
  # then back-transformed to whatever scale is actually meaningful to look at
  # (probability for logistic, count for log).
  predict_index <- function(idx_vec) {
    nd <- data.frame(single_index_estimated = idx_vec)
    if (type %in% c("logistic", "log")) nd$confounder_offset <- 0
    p <- stats::predict(m2, newdata = nd, type = "link", se.fit = TRUE)
    fit_link <- as.numeric(p$fit)
    if (type == "logistic") stats::plogis(fit_link)
    else if (type == "log") exp(fit_link)
    else fit_link
  }

  x_1_index_dat <- as.data.frame(cbind(x_1_value, x_1_index))
  colnames(x_1_index_dat) <- c("x_1_value", "single_index_estimated")
  x2_q1_index <- stats::quantile(x_2_value, p = 0.25) * beta_2; x_1_index_dat$index_q1 <- x_1_index_dat$single_index_estimated + x2_q1_index
  x2_q2_index <- stats::quantile(x_2_value, p = 0.50) * beta_2; x_1_index_dat$index_q2 <- x_1_index_dat$single_index_estimated + x2_q2_index
  x2_q3_index <- stats::quantile(x_2_value, p = 0.75) * beta_2; x_1_index_dat$index_q3 <- x_1_index_dat$single_index_estimated + x2_q3_index

  x_1_index_dat$pred_q1 <- predict_index(x_1_index_dat$index_q1)
  x_1_index_dat$pred_q2 <- predict_index(x_1_index_dat$index_q2)
  x_1_index_dat$pred_q3 <- predict_index(x_1_index_dat$index_q3)

  x_1_index_dat <- x_1_index_dat[order(x_1_index_dat[, 1]), ]

  x_2_index_dat <- as.data.frame(cbind(x_2_value, x_2_index))
  colnames(x_2_index_dat) <- c("x_2_value", "single_index_estimated")
  # FIX: these must condition on exp_1's quantiles (not exp_2's -- that was a
  # copy-paste bug that made both panels secretly condition on exp_2).
  x1_q1_index <- stats::quantile(x_1_value, p = 0.25) * beta_1; x_2_index_dat$index_q1 <- x_2_index_dat$single_index_estimated + x1_q1_index
  x1_q2_index <- stats::quantile(x_1_value, p = 0.50) * beta_1; x_2_index_dat$index_q2 <- x_2_index_dat$single_index_estimated + x1_q2_index
  x1_q3_index <- stats::quantile(x_1_value, p = 0.75) * beta_1; x_2_index_dat$index_q3 <- x_2_index_dat$single_index_estimated + x1_q3_index

  x_2_index_dat$pred_q1 <- predict_index(x_2_index_dat$index_q1)
  x_2_index_dat$pred_q2 <- predict_index(x_2_index_dat$index_q2)
  x_2_index_dat$pred_q3 <- predict_index(x_2_index_dat$index_q3)

  x_2_index_dat <- x_2_index_dat[order(x_2_index_dat[, 1]), ]

  ylab <- switch(type, linear = "Predicted outcome", logistic = "Predicted probability", log = "Predicted count")

  ymin <- min(min(x_1_index_dat[, c("pred_q1", "pred_q2", "pred_q3")]), min(x_2_index_dat[, c("pred_q1", "pred_q2", "pred_q3")]))
  ymax <- max(max(x_1_index_dat[, c("pred_q1", "pred_q2", "pred_q3")]), max(x_2_index_dat[, c("pred_q1", "pred_q2", "pred_q3")]))
  if (type == "linear") {
    y_lim <- c(ymin - 0.5, ymax + 0.5)
  } else if (type == "logistic") {
    # Probabilities are bounded in [0, 1] -- small fixed pad, clamped,
    # instead of the linear case's +/-0.5.
    y_lim <- c(max(0, ymin - 0.02), min(1, ymax + 0.02))
  } else {
    # Counts are bounded below at 0 -- clamp the lower limit instead of the
    # linear case's symmetric +/-0.5, which could push the axis negative.
    y_lim <- c(max(0, ymin - 0.5), ymax + 0.5)
  }

  opar <- graphics::par(mfrow = c(1, 2))
  on.exit(graphics::par(opar))
  plot(x_1_index_dat[, c("x_1_value")], x_1_index_dat[, c("pred_q1")], col = 1,
       type = "l", xlab = exp_1, ylab = ylab, ylim = y_lim)
  graphics::lines(x_1_index_dat[, c("x_1_value")], x_1_index_dat[, c("pred_q2")], col = 2)
  graphics::lines(x_1_index_dat[, c("x_1_value")], x_1_index_dat[, c("pred_q3")], col = 3)
  graphics::axis(side = 1, at = x_1_index_dat[, c("x_1_value")], labels = FALSE, NA, tck = 0.016)
  graphics::legend("topleft", xpd = TRUE, bty = "n", col = c(1, 2, 3), lty = 1, cex = 0.6,
                   legend = c(paste("Q1 of ", exp_2, sep = ""), paste("Q2 of ", exp_2, sep = ""), paste("Q3 of ", exp_2, sep = "")))

  plot(x_2_index_dat[, c("x_2_value")], x_2_index_dat[, c("pred_q1")], col = 1,
       type = "l", xlab = exp_2, ylab = ylab, ylim = y_lim)
  graphics::lines(x_2_index_dat[, c("x_2_value")], x_2_index_dat[, c("pred_q2")], col = 2)
  graphics::lines(x_2_index_dat[, c("x_2_value")], x_2_index_dat[, c("pred_q3")], col = 3)
  graphics::axis(side = 1, at = x_2_index_dat[, c("x_2_value")], labels = FALSE, NA, tck = 0.016)
  graphics::legend("topleft", xpd = TRUE, bty = "n", col = c(1, 2, 3), lty = 1, cex = 0.6,
                   legend = c(paste("Q1 of ", exp_1, sep = ""), paste("Q2 of ", exp_1, sep = ""), paste("Q3 of ", exp_1, sep = "")))
}
