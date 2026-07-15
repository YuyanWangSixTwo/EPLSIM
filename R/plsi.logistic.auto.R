#' Partial linear single index logistic regression with automatic smoothness selection
#'
#' Binary-outcome analog of \code{plsi.lr.auto()}: same partial linear single
#' index model, but with a logistic link. The link function is estimated with
#' a penalized regression spline (\code{mgcv::gam}, \code{family = binomial()},
#' REML smoothing-parameter selection), so there is no \code{spline.num} to
#' hand-tune -- only an upper bound \code{k} on basis complexity, with REML
#' shrinking unneeded wiggliness toward a straight line on the logit scale.
#'
#' @param data A data set including all needed variables
#' @param Y.name Variable name for the binary outcome. Must be coded 0/1, or a
#'   2-level factor (which will be coerced to 0/1 by level order, with a
#'   message reporting which level became 0 and which became 1).
#' @param X.name Variable name vector for exposures
#' @param Z.name Variable name vector for confounders
#' @param k Upper bound on basis dimension for the smooth link function.
#'   Default 10.
#' @param bs Smooth basis type passed to \code{mgcv::s()}; default \code{"cr"}
#'   (cubic regression spline).
#' @param initial.random.num Number of random initials for the single-index
#'   direction search
#' @param seed A single integer used to seed the RNG so results (random initials
#'   and downstream optim() convergence) are reproducible. Set to NULL to skip
#'   seeding and use whatever RNG state is already active in the calling
#'   environment. Default is 2026.
#' @return A list of model estimation and prediction results, structured
#'   analogously to \code{plsi.lr.auto()}'s output:
#'   \describe{
#'     \item{si.coefficient}{Single-index direction estimates (Wald z-tests,
#'       since inference here is normal-approximation based, not t-based).}
#'     \item{confounder.coefficient}{Confounder log-odds coefficients, plus
#'       odds ratios and their 95\% CIs.}
#'     \item{si.fun}{The estimated single-index link function, on both the
#'       logit scale (\code{fit}/\code{se}/\code{lwr}/\code{upr}) and the
#'       probability scale (\code{prob.fit}/\code{prob.lwr}/\code{prob.upr},
#'       obtained by back-transforming the logit-scale CI so it stays in
#'       [0, 1]).}
#'     \item{si.fun.model}{A confounder-free \code{gam} of the logit-scale
#'       residual on the single index alone -- predict() from this needs only
#'       \code{single_index_estimated}, not the original confounders. See
#'       details.}
#'     \item{full.model}{The full fitted \code{gam} (smooth + confounders),
#'       kept for reference/diagnostics.}
#'   }
#'
#' @details
#' Because the outcome is binary, the "confounder-free" single-index model
#' cannot be built by subtracting confounder effects from \code{y} on the
#' response scale the way \code{plsi.lr.auto()} does (probabilities aren't
#' additive), and it cannot be built by residualizing the full model's own
#' \emph{fitted} linear predictor either -- that would leave a value that is
#' already an exact smooth function of the single index, so refitting a smooth
#' to it would just retrace the same curve with near-zero (and spuriously
#' tiny) standard errors. Instead, the confounder contribution from the full
#' model is fixed as a known \code{offset()} and a second, confounder-free
#' binomial \code{gam} is refit directly against the actual observed \code{y},
#' preserving real binomial sampling variability. Predictions from this model
#' are made at \code{offset = 0}, consistent with how the offset was netted
#' out of every training point.
#'
#' @importFrom stats qnorm plogis
#' @importFrom mgcv gam
#'
#' @examples
#' \donttest{
#' data(nhanes.new)
#' data <- nhanes.new
#'
#' # demo binary outcome: upper-tertile triglycerides vs. not
#' data$high.triglyceride <- as.numeric(
#'   data$log.triglyceride > stats::quantile(data$log.triglyceride, 2 / 3)
#' )
#'
#' Y.name <- "high.triglyceride"
#' X.name <- c("X1_trans.b.carotene", "X2_retinol", "X3_g.tocopherol", "X4_a.tocopherol",
#'             "X5_PCB99", "X6_PCB156", "X7_PCB206",
#'             "X8_3.3.4.4.5.pncb", "X9_1.2.3.4.7.8.hxcdf", "X10_2.3.4.6.7.8.hxcdf")
#' Z.name <- c("AGE.c", "SEX.Female", "RACE.NH.Black",
#'            "RACE.MexicanAmerican", "RACE.OtherRace", "RACE.Hispanic" )
#'
#' k <- 10
#' bs <- "cr"
#' initial.random.num <- 1
#' seed <- 2026
#'
#' model_logistic_auto <- plsi.logistic.auto(data = data, Y.name = Y.name, X.name = X.name,
#'                       Z.name = Z.name, k = k, bs = bs,
#'                       initial.random.num = initial.random.num, seed = seed)
#' }
#'
#' @keywords partial linear single index
#' @keywords logistic regression, automatic smoothness
#' @author Yuyan Wang
#' @export
plsi.logistic.auto <- function(data, Y.name, X.name, Z.name,
                               k = 10, bs = "cr", initial.random.num = 5,
                               seed = 2026) {

  stopifnot(initial.random.num >= 1)
  if (!is.null(seed)) set.seed(seed)
  missing_cols <- setdiff(c(Y.name, X.name, Z.name), colnames(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
  }
  if (anyNA(data[, c(Y.name, X.name, Z.name)])) {
    stop("Missing values found in Y/X/Z columns; please handle NAs before calling plsi.logistic.auto().")
  }
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Package 'mgcv' is required for plsi.logistic.auto(). Please install it.")
  }

  # Validate / coerce the binary outcome ---------------------------------------
  y_raw <- data[, Y.name]
  if (is.factor(y_raw)) {
    if (nlevels(y_raw) != 2) {
      stop("Y.name must be binary (2 levels) for plsi.logistic.auto(); found ",
           nlevels(y_raw), " levels.")
    }
    message("Y.name ('", Y.name, "') is a factor; coercing to 0/1 by level order: ",
            levels(y_raw)[1], " -> 0, ", levels(y_raw)[2], " -> 1.")
    y <- as.numeric(y_raw) - 1
  } else {
    uy <- sort(unique(y_raw))
    if (!(length(uy) == 2 && all(uy == c(0, 1)))) {
      stop("Y.name must be coded 0/1 (or supplied as a 2-level factor) for plsi.logistic.auto(). ",
           "Found values: ", paste(uy, collapse = ", "))
    }
    y <- as.numeric(y_raw)
  }

  cor_linear <- stats::cor(data.frame(y = y, data[, X.name]))
  cor_linear <- as.data.frame(cor_linear[-1, 1])
  cor_linear$exposure <- rownames(cor_linear)
  X.name <- cor_linear[order(cor_linear[, 1], decreasing = TRUE), 2]

  x <- as.matrix(data[, X.name]); z_df <- data[, Z.name, drop = FALSE]
  n <- nrow(data); x_length <- length(X.name); z_length <- length(Z.name)

  m0 <- stats::glm(y ~ x + as.matrix(z_df), family = stats::binomial())

  initial_table <- as.data.frame(matrix(NA, nrow = (1 + initial.random.num), ncol = x_length))
  colnames(initial_table) <- X.name
  rownames(initial_table) <- c("linear", paste0("random_", 1:initial.random.num))
  initial_table[1, ] <- m0$coefficients[2:(1 + x_length)]
  initial_table[2:(1 + initial.random.num), ] <- stats::runif(initial.random.num * x_length, -1, 1)
  for (i in 1:nrow(initial_table)) {
    initial_table[i, ] <- initial_table[i, ] * sign(initial_table[i, 1]) / sqrt(sum(initial_table[i, ]^2))
  }

  # REML-penalized smooth logit link, refit for each candidate direction beta.
  # No spline.num / degrees of freedom to hand-tune: 'k' is just a ceiling,
  # and REML shrinks the smooth toward linear on its own.
  fit_gam <- function(beta_0) {
    u0 <- as.vector(x %*% as.vector(beta_0))
    dat_gam <- cbind(data.frame(y = y, u0 = u0), z_df)
    form <- stats::as.formula(paste0("y ~ s(u0, bs='", bs, "', k=", k, ") + ",
                                     paste(Z.name, collapse = " + ")))
    mgcv::gam(form, data = dat_gam, family = stats::binomial(), method = "REML")
  }

  fn <- function(beta_0) {
    utils::flush.console()
    tryCatch(as.numeric(stats::logLik(fit_gam(beta_0))),
             error = function(e) {
               warning("fit_gam() failed for a candidate beta and was scored as -Inf: ",
                       conditionMessage(e), call. = FALSE)
               -Inf
             })
  }

  fit_results <- vector("list", nrow(initial_table))
  crit_table <- data.frame(logLik = NA_real_, AIC = NA_real_, BIC = NA_real_)[rep(1, nrow(initial_table)), ]
  rownames(crit_table) <- rownames(initial_table)

  for (i in 1:nrow(initial_table)) {
    beta_0 <- as.numeric(initial_table[i, ])
    m_temp <- stats::optim(par = beta_0, fn = fn, method = "L-BFGS-B",
                           control = list(fnscale = -1, maxit = 100))
    fit_results[[i]] <- m_temp
    g <- fit_gam(m_temp$par)
    crit_table$logLik[i] <- as.numeric(stats::logLik(g))
    crit_table$AIC[i] <- stats::AIC(g)
    crit_table$BIC[i] <- stats::BIC(g)
  }

  order_ob <- order(crit_table$BIC)[1]
  m_selected <- fit_results[[order_ob]]
  beta_BeforeNorm <- m_selected$par

  # Standard errors for beta: fn() re-fits a gam() at every evaluation, so no
  # analytic gradient/Hessian is available from optim() directly (hessian=TRUE
  # was skipped inside the search loop to avoid paying that cost for every
  # candidate direction). Recover the Hessian numerically at the selected
  # optimum only, the same way plsi.lr.auto() derives beta_sigma.
  hess <- stats::optimHess(par = beta_BeforeNorm, fn = fn, control = list(fnscale = -1))
  info_matrix <- -hess

  eig <- eigen(info_matrix, symmetric = TRUE, only.values = TRUE)$values
  if (any(eig <= 0)) {
    warning("Observed information matrix is not positive definite at the selected optimum ",
            "(min eigenvalue = ", signif(min(eig), 3), "). Standard errors may be unreliable; ",
            "consider increasing initial.random.num.")
  }

  beta_BeforeNorm_sigma <- suppressWarnings(sqrt(diag(MASS::ginv(info_matrix))))

  beta_est <- beta_BeforeNorm * sign(beta_BeforeNorm[1]) / sqrt(sum(beta_BeforeNorm^2))
  beta_sigma <- beta_BeforeNorm_sigma / sqrt(sum(beta_BeforeNorm^2))

  final_model <- fit_gam(beta_est)
  s <- summary(final_model)

  # Wald tests here use the normal reference distribution (z), matching how
  # summary.glm()/summary.gam() label inference under family = binomial().
  si.coefficient <- data.frame(Estimate = as.numeric(beta_est),
                               `Std.Error` = as.numeric(beta_sigma),
                               row.names = X.name, check.names = FALSE)
  si.coefficient$`z value` <- si.coefficient$Estimate / si.coefficient$`Std.Error`
  si.coefficient$`Pr(>|z|)` <- ifelse(2 * stats::pnorm(-abs(si.coefficient$`z value`)) < 0.0001, "<.0001",
                                      format(round(2 * stats::pnorm(-abs(si.coefficient$`z value`)), 4), nsmall = 4))
  si.coefficient$Lower.95CI <- si.coefficient$Estimate + stats::qnorm(0.025) * si.coefficient$`Std.Error`
  si.coefficient$Upper.95CI <- si.coefficient$Estimate + stats::qnorm(0.975) * si.coefficient$`Std.Error`
  si.coefficient$`Contribution proportion` <- format(round((si.coefficient$Estimate)^2, 3), nsmall = 3)
  # Sort by Estimate, descending (largest coefficient first) -- matches plsi.lr.auto()
  si.coefficient <- si.coefficient[order(si.coefficient$Estimate, decreasing = TRUE), ]

  confounder.coefficient <- as.data.frame(s$p.table[Z.name, , drop = FALSE])
  confounder.coefficient$Lower.95CI <- confounder.coefficient$Estimate + stats::qnorm(0.025) * confounder.coefficient$`Std. Error`
  confounder.coefficient$Upper.95CI <- confounder.coefficient$Estimate + stats::qnorm(0.975) * confounder.coefficient$`Std. Error`
  # Odds ratios: interpretable here because these are genuine per-confounder
  # log-odds coefficients (unlike beta_est, which is a unit-norm direction
  # component and has no standalone odds-ratio interpretation).
  confounder.coefficient$`Odds Ratio` <- exp(confounder.coefficient$Estimate)
  confounder.coefficient$OR.Lower.95CI <- exp(confounder.coefficient$Lower.95CI)
  confounder.coefficient$OR.Upper.95CI <- exp(confounder.coefficient$Upper.95CI)
  confounder.coefficient$`Pr(>|z|)` <- ifelse(confounder.coefficient$`Pr(>|z|)` < 0.0001, "<.0001",
                                              format(round(confounder.coefficient$`Pr(>|z|)`, 4), nsmall = 4))

  single_index_estimated <- as.vector(x %*% beta_est)

  # Confounder-free single-index model for prediction -------------------------
  # `final_model` still has AGE.c / SEX.Female / etc. in its formula, so
  # predict() on it requires those columns in newdata. As in plsi.lr.auto(),
  # we want a second model that predict()s from single_index_estimated alone.
  #
  # IMPORTANT: this must be fit against the *actual observed* y, not against
  # final_model's own fitted values. Subtracting the confounder contribution
  # from predict(final_model, type="link") would leave exactly
  # intercept + s(u0) with zero residual noise -- fitting a new smooth to that
  # just re-traces the same curve and collapses every standard error to ~1e-16
  # (a spurious "perfect fit," not genuine precision). Instead, fix the
  # confounder contribution as a known offset and refit a binomial GAM
  # directly against y, so the real binomial variability is preserved and the
  # resulting SEs are meaningful.
  confounder_coefs <- stats::coef(final_model)[Z.name]
  confounder_offset <- as.vector(as.matrix(z_df) %*% confounder_coefs)

  dat_si_fun <- data.frame(single_index_estimated = single_index_estimated,
                           y = y,
                           confounder_offset = confounder_offset)
  form_si <- stats::as.formula(paste0("y ~ offset(confounder_offset) + s(single_index_estimated, bs='", bs, "', k=", k, ")"))
  m2 <- mgcv::gam(form_si, data = dat_si_fun, family = stats::binomial(), method = "REML")

  # As with plsi.lr.auto(): ciTools::add_ci() does not support "gam" objects,
  # so use predict.gam(se.fit=TRUE) directly and build the Wald interval by
  # hand. m2 has only one smooth term, so no predict(type="terms") column-
  # naming ambiguity to worry about. Predict at confounder_offset = 0: since
  # the offset was netted out of every training point too, this is the
  # confounder-adjusted reference curve, consistent with the training fit.
  ord <- order(single_index_estimated)
  newdat <- data.frame(single_index_estimated = single_index_estimated[ord],
                       confounder_offset = 0)
  pred2 <- stats::predict(m2, newdata = newdat, type = "link", se.fit = TRUE)
  si.fun <- data.frame(single_index_estimated = single_index_estimated[ord],
                       fit = pred2$fit,
                       se  = pred2$se.fit)
  si.fun$lwr <- si.fun$fit - stats::qnorm(0.975) * si.fun$se
  si.fun$upr <- si.fun$fit + stats::qnorm(0.975) * si.fun$se
  # Back-transform the logit-scale CI (not the SE) to the probability scale,
  # so the interval stays inside [0, 1].
  si.fun$prob.fit <- stats::plogis(si.fun$fit)
  si.fun$prob.lwr <- stats::plogis(si.fun$lwr)
  si.fun$prob.upr <- stats::plogis(si.fun$upr)

  return(list(original.data = list(y = y, x = x, z = z_df),
              original.par = list(k = k, bs = bs, initial.random.num = initial.random.num, seed = seed),
              si.coefficient = si.coefficient,
              model.statistics = crit_table[order_ob, ],
              initial.table = initial_table,
              confounder.coefficient = confounder.coefficient,
              si.fun.edf = s$edf[1],          # effective df actually used -- this is the "auto-tuned" answer
              si.fun = si.fun,
              si.fun.model = m2,              # confounder-free, logit scale: predict() only needs single_index_estimated
              full.model = final_model))      # full fit incl. confounders, kept for reference/diagnostics
}
