#' Partial linear single index log-linear regression with automatic smoothness selection
#'
#' Count-outcome analog of \code{plsi.lr.auto()} / \code{plsi.logistic.auto()}:
#' same partial linear single index model, but for non-negative count data via
#' a log link. The link function is estimated with a penalized regression
#' spline (\code{mgcv::gam}, REML smoothing-parameter selection), so there is
#' no \code{spline.num} to hand-tune -- only an upper bound \code{k} on basis
#' complexity, with REML shrinking unneeded wiggliness toward a straight line
#' on the log scale.
#'
#' @param data A data set including all needed variables
#' @param Y.name Variable name for the count outcome. Must be non-negative
#'   integers (0, 1, 2, ...).
#' @param X.name Variable name vector for exposures
#' @param Z.name Variable name vector for confounders
#' @param family Count distribution to use. \code{"nb"} (default) fits a
#'   negative binomial GAM (\code{mgcv::nb()}), which estimates its own
#'   dispersion parameter and is the safer default for real count data, which
#'   is usually overdispersed (variance > mean) relative to Poisson.
#'   \code{"poisson"} fits a Poisson GAM, valid only if the mean-equals-
#'   variance assumption actually holds; if unsure, leave the default.
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
#'   analogously to \code{plsi.logistic.auto()}'s output:
#'   \describe{
#'     \item{si.coefficient}{Single-index direction estimates (Wald z-tests,
#'       since inference here is normal-approximation based, not t-based).}
#'     \item{confounder.coefficient}{Confounder log-rate coefficients, plus
#'       rate ratios and their 95\% CIs.}
#'     \item{si.fun}{The estimated single-index link function, on both the
#'       log scale (\code{fit}/\code{se}/\code{lwr}/\code{upr}) and the count
#'       scale (\code{count.fit}/\code{count.lwr}/\code{count.upr}, obtained
#'       by exponentiating the log-scale CI so it stays non-negative).}
#'     \item{si.fun.model}{A confounder-free \code{gam} of the count outcome
#'       on the single index alone (via a fixed offset for the confounder
#'       contribution) -- predict() from this needs only
#'       \code{single_index_estimated}, not the original confounders. See
#'       details.}
#'     \item{full.model}{The full fitted \code{gam} (smooth + confounders),
#'       kept for reference/diagnostics.}
#'   }
#'
#' @details
#' As with \code{plsi.logistic.auto()}, the "confounder-free" single-index
#' model cannot be built by subtracting confounder effects from \code{y} on
#' the response scale (counts aren't additive that way either), and it cannot
#' be built by residualizing the full model's own \emph{fitted} linear
#' predictor -- that would leave a value that is already an exact smooth
#' function of the single index, so refitting a smooth to it would just
#' retrace the same curve with near-zero (and spuriously tiny) standard
#' errors. Instead, the confounder contribution from the full model is fixed
#' as a known \code{offset()} and a second, confounder-free count GAM (same
#' \code{family} as the full model) is refit directly against the actual
#' observed \code{y}, preserving real count sampling variability. Predictions
#' from this model are made at \code{offset = 0}, consistent with how the
#' offset was netted out of every training point.
#'
#' @importFrom stats qnorm
#' @importFrom mgcv gam nb
#'
#' @examples
#' \donttest{
#' data(nhanes.new)
#' data <- nhanes.new
#'
#' X.name <- c("X1_trans.b.carotene", "X2_retinol", "X3_g.tocopherol", "X4_a.tocopherol",
#'             "X5_PCB99", "X6_PCB156", "X7_PCB206",
#'             "X8_3.3.4.4.5.pncb", "X9_1.2.3.4.7.8.hxcdf", "X10_2.3.4.6.7.8.hxcdf")
#' Z.name <- c("AGE.c", "SEX.Female", "RACE.NH.Black",
#'            "RACE.MexicanAmerican", "RACE.OtherRace", "RACE.Hispanic" )
#'
#' # demo count outcome (illustrative only -- nhanes.new has no native count
#' # variable). Simulated from a *true* single-index combination of the
#' # exposures, run through a nonlinear log-rate link, plus a confounder
#' # effect -- so the example actually has an exposure-outcome relationship
#' # for plsi.log.auto() to recover, rather than depending on the confounder
#' # alone.
#' set.seed(2026)
#' beta_true <- c(0.30, -0.20, 0.10, 0.40, -0.30, 0.20, -0.10, 0.25, -0.15, 0.35)
#' beta_true <- beta_true / sqrt(sum(beta_true^2))         # unit norm, matching
#'                                                          # the model's own
#'                                                          # identifiability constraint
#' x_std <- scale(data[, X.name])                          # standardize so the
#'                                                          # demo index is on a
#'                                                          # sane, comparable scale
#' single_index_true <- as.vector(x_std %*% beta_true)
#' log_rate <- 0.3 + 0.4 * sin(single_index_true) + 0.05 * data$AGE.c
#' data$n.events <- stats::rpois(nrow(data), lambda = exp(log_rate))
#'
#' Y.name <- "n.events"
#'
#' k <- 10
#' bs <- "cr"
#' initial.random.num <- 1
#' seed <- 2026
#'
#' model_log_auto <- plsi.log.auto(data = data, Y.name = Y.name, X.name = X.name,
#'                       Z.name = Z.name, family = "nb", k = k, bs = bs,
#'                       initial.random.num = initial.random.num, seed = seed)
#' }
#' @keywords partial linear single index
#' @keywords log-linear regression, automatic smoothness
#' @keywords count outcome
#' @author Yuyan Wang
#' @export
plsi.log.auto <- function(data, Y.name, X.name, Z.name,
                          family = c("nb", "poisson"),
                          k = 10, bs = "cr", initial.random.num = 5,
                          seed = 2026) {

  family <- match.arg(family)
  stopifnot(initial.random.num >= 1)
  if (!is.null(seed)) set.seed(seed)
  missing_cols <- setdiff(c(Y.name, X.name, Z.name), colnames(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
  }
  if (anyNA(data[, c(Y.name, X.name, Z.name)])) {
    stop("Missing values found in Y/X/Z columns; please handle NAs before calling plsi.log.auto().")
  }
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Package 'mgcv' is required for plsi.log.auto(). Please install it.")
  }

  # Validate the count outcome ---------------------------------------------
  y_raw <- data[, Y.name]
  if (any(y_raw < 0) || any(y_raw != round(y_raw))) {
    stop("Y.name must be non-negative integer counts for plsi.log.auto(). ",
         "Found negative and/or non-integer values.")
  }
  y <- as.numeric(y_raw)

  fam_obj <- if (family == "nb") mgcv::nb() else stats::poisson()

  cor_linear <- stats::cor(data.frame(y = y, data[, X.name]))
  cor_linear <- as.data.frame(cor_linear[-1, 1])
  cor_linear$exposure <- rownames(cor_linear)
  X.name <- cor_linear[order(cor_linear[, 1], decreasing = TRUE), 2]

  x <- as.matrix(data[, X.name]); z_df <- data[, Z.name, drop = FALSE]
  n <- nrow(data); x_length <- length(X.name); z_length <- length(Z.name)

  # Initial linear direction: MASS::glm.nb() for family = "nb" (matches the
  # dispersion structure we're about to fit), stats::glm(family=poisson())
  # otherwise. Only used to seed the direction search, not for inference.
  if (family == "nb") {
    m0 <- suppressWarnings(MASS::glm.nb(y ~ x + as.matrix(z_df)))
  } else {
    m0 <- stats::glm(y ~ x + as.matrix(z_df), family = stats::poisson())
  }

  initial_table <- as.data.frame(matrix(NA, nrow = (1 + initial.random.num), ncol = x_length))
  colnames(initial_table) <- X.name
  rownames(initial_table) <- c("linear", paste0("random_", 1:initial.random.num))
  initial_table[1, ] <- m0$coefficients[2:(1 + x_length)]
  initial_table[2:(1 + initial.random.num), ] <- stats::runif(initial.random.num * x_length, -1, 1)
  for (i in 1:nrow(initial_table)) {
    initial_table[i, ] <- initial_table[i, ] * sign(initial_table[i, 1]) / sqrt(sum(initial_table[i, ]^2))
  }

  # REML-penalized smooth log link, refit for each candidate direction beta.
  # No spline.num / degrees of freedom to hand-tune: 'k' is just a ceiling,
  # and REML shrinks the smooth toward linear on its own.
  fit_gam <- function(beta_0) {
    u0 <- as.vector(x %*% as.vector(beta_0))
    dat_gam <- cbind(data.frame(y = y, u0 = u0), z_df)
    form <- stats::as.formula(paste0("y ~ s(u0, bs='", bs, "', k=", k, ") + ",
                                     paste(Z.name, collapse = " + ")))
    mgcv::gam(form, data = dat_gam, family = fam_obj, method = "REML")
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
  # optimum only, the same way plsi.lr.auto()/plsi.logistic.auto() derive
  # beta_sigma.
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
  # summary.glm()/summary.gam() label inference under a Poisson/NB family.
  si.coefficient <- data.frame(Estimate = as.numeric(beta_est),
                               `Std.Error` = as.numeric(beta_sigma),
                               row.names = X.name, check.names = FALSE)
  si.coefficient$`z value` <- si.coefficient$Estimate / si.coefficient$`Std.Error`
  si.coefficient$`Pr(>|z|)` <- ifelse(2 * stats::pnorm(-abs(si.coefficient$`z value`)) < 0.0001, "<.0001",
                                      format(round(2 * stats::pnorm(-abs(si.coefficient$`z value`)), 4), nsmall = 4))
  si.coefficient$Lower.95CI <- si.coefficient$Estimate + stats::qnorm(0.025) * si.coefficient$`Std.Error`
  si.coefficient$Upper.95CI <- si.coefficient$Estimate + stats::qnorm(0.975) * si.coefficient$`Std.Error`
  si.coefficient$`Contribution proportion` <- format(round((si.coefficient$Estimate)^2, 3), nsmall = 3)
  # Sort by Estimate, descending (largest coefficient first) -- matches plsi.lr.auto()/plsi.logistic.auto()
  si.coefficient <- si.coefficient[order(si.coefficient$Estimate, decreasing = TRUE), ]

  confounder.coefficient <- as.data.frame(s$p.table[Z.name, , drop = FALSE])
  confounder.coefficient$Lower.95CI <- confounder.coefficient$Estimate + stats::qnorm(0.025) * confounder.coefficient$`Std. Error`
  confounder.coefficient$Upper.95CI <- confounder.coefficient$Estimate + stats::qnorm(0.975) * confounder.coefficient$`Std. Error`
  # Rate ratios: interpretable here because these are genuine per-confounder
  # log-rate coefficients (unlike beta_est, which is a unit-norm direction
  # component and has no standalone rate-ratio interpretation).
  confounder.coefficient$`Rate Ratio` <- exp(confounder.coefficient$Estimate)
  confounder.coefficient$RR.Lower.95CI <- exp(confounder.coefficient$Lower.95CI)
  confounder.coefficient$RR.Upper.95CI <- exp(confounder.coefficient$Upper.95CI)
  confounder.coefficient$`Pr(>|z|)` <- ifelse(confounder.coefficient$`Pr(>|z|)` < 0.0001, "<.0001",
                                              format(round(confounder.coefficient$`Pr(>|z|)`, 4), nsmall = 4))

  single_index_estimated <- as.vector(x %*% beta_est)

  # Confounder-free single-index model for prediction -------------------------
  # `final_model` still has AGE.c / SEX.Female / etc. in its formula, so
  # predict() on it requires those columns in newdata. As in plsi.logistic.
  # auto(), the fix is to hold the confounder contribution fixed as a known
  # offset and refit a confounder-free count GAM directly against the actual
  # observed y (NOT against final_model's own fitted values, which would
  # collapse the SEs to ~machine epsilon -- see @details).
  confounder_coefs <- stats::coef(final_model)[Z.name]
  confounder_offset <- as.vector(as.matrix(z_df) %*% confounder_coefs)

  dat_si_fun <- data.frame(single_index_estimated = single_index_estimated,
                           y = y,
                           confounder_offset = confounder_offset)
  form_si <- stats::as.formula(paste0("y ~ offset(confounder_offset) + s(single_index_estimated, bs='", bs, "', k=", k, ")"))
  m2 <- mgcv::gam(form_si, data = dat_si_fun, family = fam_obj, method = "REML")

  # As with plsi.lr.auto()/plsi.logistic.auto(): ciTools::add_ci() does not
  # support "gam" objects, so use predict.gam(se.fit=TRUE) directly and build
  # the Wald interval by hand. m2 has only one smooth term, so no
  # predict(type="terms") column-naming ambiguity to worry about. Predict at
  # confounder_offset = 0: since the offset was netted out of every training
  # point too, this is the confounder-adjusted reference curve, consistent
  # with the training fit.
  ord <- order(single_index_estimated)
  newdat <- data.frame(single_index_estimated = single_index_estimated[ord],
                       confounder_offset = 0)
  pred2 <- stats::predict(m2, newdata = newdat, type = "link", se.fit = TRUE)
  si.fun <- data.frame(single_index_estimated = single_index_estimated[ord],
                       fit = pred2$fit,
                       se  = pred2$se.fit)
  si.fun$lwr <- si.fun$fit - stats::qnorm(0.975) * si.fun$se
  si.fun$upr <- si.fun$fit + stats::qnorm(0.975) * si.fun$se
  # Back-transform the log-scale CI (not the SE) to the count scale, so the
  # interval stays non-negative.
  si.fun$count.fit <- exp(si.fun$fit)
  si.fun$count.lwr <- exp(si.fun$lwr)
  si.fun$count.upr <- exp(si.fun$upr)

  return(list(original.data = list(y = y, x = x, z = z_df),
              original.par = list(family = family, k = k, bs = bs,
                                  initial.random.num = initial.random.num, seed = seed),
              si.coefficient = si.coefficient,
              model.statistics = crit_table[order_ob, ],
              initial.table = initial_table,
              confounder.coefficient = confounder.coefficient,
              si.fun.edf = s$edf[1],          # effective df actually used -- this is the "auto-tuned" answer
              si.fun = si.fun,
              si.fun.model = m2,              # confounder-free, log scale: predict() only needs single_index_estimated
              full.model = final_model))      # full fit incl. confounders, kept for reference/diagnostics
}
