#' plot interquartile effect of specific exposure based on quartile of other exposures
#'
#' @param fit Fitted model from \code{plsi.lr.auto()}, \code{plsi.logistic.auto()},
#'   or \code{plsi.log.auto()}
#' @param data Original data set
#' @param type Which outcome scale to plot. \code{"linear"} (default) plots the
#'   difference in predicted (continuous) outcome, for a \code{fit} from
#'   \code{plsi.lr.auto()}. \code{"logistic"} plots the difference in predicted
#'   probability, for a \code{fit} from \code{plsi.logistic.auto()}. \code{"log"}
#'   plots the difference in predicted count, for a \code{fit} from
#'   \code{plsi.log.auto()}.
#' @importFrom stats quantile
#' @return plot of main interquartile effect of exposure based on quartile of other exposures
#'
#' @examples
#' \dontrun{
#' # example to interquartile effect based on quartile of other exposures -- continuous outcome
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
#' # plot interquartile quartile -- difference in predicted (continuous) outcome
#' interquartile.quartile.plot(model_lr_auto, data, type = "linear")
#' }
#' @keywords partial linear single index
#' @keywords interquartile quartile effect
#' @keywords interaction
#' @author Yuyan Wang
#' @export
#'
interquartile.quartile.plot <- function(fit, data, type = c("linear", "logistic", "log")){
  # fit = model_lr_auto; data = data; type = "linear"
  # fit = model_logistic_auto; data = data; type = "logistic"
  # fit = model_log_auto; data = data; type = "log"
  type <- match.arg(type)

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

  # predict.gam(se.fit=TRUE) directly, instead of ciTools::add_ci(), since
  # ciTools does not support "gam" objects from mgcv. For type = "logistic"/
  # "log", si.fun.model has an offset(confounder_offset) term, so newdata
  # needs that column too; offset = 0 is the confounder-adjusted reference
  # curve, consistent with how si.fun/e.main.plot()/e.interaction.plot()
  # predict from this same model.
  nd <- data.frame(single_index_estimated = pre_temp$single_index_estimated)
  if (type %in% c("logistic", "log")) nd$confounder_offset <- 0
  p <- stats::predict(m2, newdata = nd, type = "link", se.fit = TRUE)

  if (type == "linear") {
    pre_temp$pred <- as.numeric(p$fit)
    pre_temp$ci_diff <- stats::qnorm(0.975) * as.numeric(p$se.fit)
  } else if (type == "logistic") {
    # Difference of predicted *probability* is what's actually meaningful for
    # a binary outcome (a difference on the logit scale doesn't translate
    # linearly into a probability difference). Back-transform the fit via
    # plogis(), and propagate the SE to the probability scale via the delta
    # method: se_prob ~= p(1-p) * se_logit. This keeps the same "average the
    # two half-widths" CI convention the rest of this function already uses,
    # just on the probability scale instead of the link scale.
    fit_link <- as.numeric(p$fit); se_link <- as.numeric(p$se.fit)
    pre_temp$pred <- stats::plogis(fit_link)
    pre_temp$ci_diff <- stats::qnorm(0.975) * (pre_temp$pred * (1 - pre_temp$pred) * se_link)
  } else {
    # Difference of predicted *count* is what's actually meaningful for a
    # count outcome (a difference on the log scale is a log rate ratio, not a
    # count difference). Back-transform the fit via exp(), and propagate the
    # SE to the count scale via the delta method: se_count ~= count * se_log
    # (since d(exp(x))/dx = exp(x)). Same "average the two half-widths" CI
    # convention as the other two branches, just on the count scale.
    fit_link <- as.numeric(p$fit); se_link <- as.numeric(p$se.fit)
    pre_temp$pred <- exp(fit_link)
    pre_temp$ci_diff <- stats::qnorm(0.975) * (pre_temp$pred * se_link)
  }

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

  ylab_txt <- switch(type,
                     linear = "Difference of predicted outcome",
                     logistic = "Difference of predicted probability",
                     log = "Difference of predicted count")

  final_plot <- ggplot2::ggplot(data = plot_temp, ggplot2::aes(x = plot_temp$Exposrue_Name, colour = plot_temp$Other_quartile, y = plot_temp$Diff_est, ymin = plot_temp$Diff_lwr, ymax = plot_temp$Diff_upr)) +
    # ggplot(data = plot_temp, aes(x = Exposrue_Name, colour = Other_quartile, y = Diff_est)) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.5)) +
    ggplot2::geom_errorbar(position = ggplot2::position_dodge(width = 0.5), width = 0.1) +
    ggplot2::coord_flip() +
    # facet_wrap(~Exposrue_Name, ncol = 1, strip.position = "left") +
    ggplot2::ylab(ylab_txt) +
    ggplot2::xlab("Exposure") +
    ggplot2::scale_color_manual(values = c("red", "blue", "green"), name = NULL)

  if (type %in% c("logistic", "log")) {
    final_plot <- final_plot + ggplot2::geom_hline(yintercept = 0, linetype = "dashed")
  }

  suppressWarnings(print(final_plot))
}
