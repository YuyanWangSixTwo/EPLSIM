#' plot mixture's overall effect based on quantile of exposures
#'
#' @param fit Fitted model from \code{plsi.lr.auto()}, \code{plsi.logistic.auto()},
#'   or \code{plsi.log.auto()}
#' @param data Original data set
#' @param type Which outcome scale to plot. \code{"linear"} (default) plots the
#'   predicted (continuous) outcome, for a \code{fit} from \code{plsi.lr.auto()}.
#'   \code{"logistic"} plots the predicted probability, for a \code{fit} from
#'   \code{plsi.logistic.auto()}. \code{"log"} plots the predicted count, for
#'   a \code{fit} from \code{plsi.log.auto()}.
#' @return plot of predicted outcomes based on quantile of exposures
#'
#' @examples
#' \donttest{
#' # example to plot mixture's overall effect -- continuous outcome
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
#' # plot mixture overall effect -- predicted (continuous) outcome
#' mixture.overall.plot(model_lr_auto, data, type = "linear")
#' }
#' @keywords partial linear single index
#' @keywords mixture overall effect
#' @author Yuyan Wang
#' @export
#'
mixture.overall.plot <- function(fit, data, type = c("linear", "logistic", "log")){
  # fit = model_lr_auto; data = data; type = "linear"
  # fit = model_logistic_auto; data = data; type = "logistic"
  # fit = model_log_auto; data = data; type = "log"
  type <- match.arg(type)

  beta_est_vec <- as.vector(fit$si.coefficient[, 1])
  X_name <- rownames(fit$si.coefficient)
  x <- as.matrix(data[, X_name])
  quants <- seq(0.1, 0.9, by = 0.05)
  x_quantiles <- apply(x, 2, stats::quantile, probs = quants)
  index_quantile <- x_quantiles %*% beta_est_vec
  index_dat <- as.data.frame(cbind(quants, index_quantile))
  colnames(index_dat) <- c("quants", "single_index_estimated")

  m2 <- fit$si.fun.model

  # predict.gam(se.fit=TRUE) directly, instead of ciTools::add_ci(), since
  # ciTools does not support "gam" objects from mgcv. For type = "logistic"/
  # "log", si.fun.model has an offset(confounder_offset) term, so newdata
  # needs that column too; offset = 0 is the confounder-adjusted reference
  # curve, consistent with how si.fun/e.main.plot()/e.interaction.plot()
  # predict from this same model.
  nd <- data.frame(single_index_estimated = index_dat$single_index_estimated)
  if (type %in% c("logistic", "log")) nd$confounder_offset <- 0
  p <- stats::predict(m2, newdata = nd, type = "link", se.fit = TRUE)

  if (type == "linear") {
    index_dat$pred <- as.numeric(p$fit)
    index_dat$lwr  <- index_dat$pred - stats::qnorm(0.975) * as.numeric(p$se.fit)
    index_dat$upr  <- index_dat$pred + stats::qnorm(0.975) * as.numeric(p$se.fit)
    ylab_txt <- "Predicted outcome"
  } else if (type == "logistic") {
    # Back-transform the logit-scale CI (not the SE) to the probability
    # scale, so the interval stays inside [0, 1] -- same convention as
    # si.fun/e.main.plot()/e.interaction.plot().
    fit_link <- as.numeric(p$fit); se_link <- as.numeric(p$se.fit)
    lwr_link <- fit_link - stats::qnorm(0.975) * se_link
    upr_link <- fit_link + stats::qnorm(0.975) * se_link
    index_dat$pred <- stats::plogis(fit_link)
    index_dat$lwr  <- stats::plogis(lwr_link)
    index_dat$upr  <- stats::plogis(upr_link)
    ylab_txt <- "Predicted probability"
  } else {
    # Back-transform the log-scale CI (not the SE) to the count scale, so the
    # interval stays non-negative -- same convention as si.fun/e.main.plot()/
    # e.interaction.plot().
    fit_link <- as.numeric(p$fit); se_link <- as.numeric(p$se.fit)
    lwr_link <- fit_link - stats::qnorm(0.975) * se_link
    upr_link <- fit_link + stats::qnorm(0.975) * se_link
    index_dat$pred <- exp(fit_link)
    index_dat$lwr  <- exp(lwr_link)
    index_dat$upr  <- exp(upr_link)
    ylab_txt <- "Predicted count"
  }

  final_plot <- ggplot2::ggplot(index_dat, ggplot2::aes(x = index_dat$quants, y = index_dat$pred)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = index_dat$lwr, ymax = index_dat$upr), width = .01) +
    ggplot2::geom_point(size = 2, shape = 21, fill = "white") +
    ggplot2::ggtitle("Mixture overall effect") +
    ggplot2::labs(y = ylab_txt, x = "Quantile levels of each exposure") +
    ggplot2::scale_x_continuous(breaks = seq(0.1, 0.9, by =  0.1))

  suppressWarnings(print(final_plot))
}
