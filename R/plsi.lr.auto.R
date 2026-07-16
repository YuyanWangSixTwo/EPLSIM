#' Partial linear single index regression with automatic smoothness selection
#'
#' Same model as \code{plsi.lr.v2()}, but the link function is estimated with a
#' penalized regression spline (\code{mgcv::gam}, REML smoothing-parameter
#' selection) instead of a fixed-df natural spline. This removes the need to
#' hand-tune \code{spline.num}: you supply an upper bound on basis complexity
#' (\code{k}), and REML shrinks unneeded wiggliness toward a straight line
#' automatically. Effective degrees of freedom actually used by the fitted
#' link function are reported in \code{si.fun.edf}.
#'
#' @param data A data set including all needed variables
#' @param Y.name Variable name for scalar outcome
#' @param X.name Variable name vector for exposures
#' @param Z.name Variable name vector for confounders
#' @param k Upper bound on basis dimension for the smooth link function
#'   (analogous to \code{spline.num} in \code{plsi.lr.v2}, but not a fixed df —
#'   REML penalizes down from this ceiling). Default 10.
#' @param bs Smooth basis type passed to \code{mgcv::s()}; default \code{"cr"}
#'   (cubic regression spline).
#' @param initial.random.num Number of random initials for the single-index
#'   direction search
#' @param seed A single integer used to seed the RNG so results (random initials
#'   and downstream optim() convergence) are reproducible. Set to NULL to skip
#'   seeding and use whatever RNG state is already active in the calling
#'   environment. Default is 2026.
#' @return A list of model estimation and prediction results, structured
#'   analogously to \code{plsi.lr.v2()}'s output.
#' @importFrom stats qnorm
#' @importFrom mgcv gam
#'
#' @examples
#' \donttest{
#' # example to run plsi.lr.auto
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
#' }
#' @keywords partial linear single index
#' @keywords linear regression, version 2
#' @author Yuyan Wang
#' @export
plsi.lr.auto <- function(data, Y.name, X.name, Z.name,
                         k = 10, bs = "cr", initial.random.num = 5,
                         seed = 2026) {

  stopifnot(initial.random.num >= 1)
  if (!is.null(seed)) set.seed(seed)
  missing_cols <- setdiff(c(Y.name, X.name, Z.name), colnames(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
  }
  if (anyNA(data[, c(Y.name, X.name, Z.name)])) {
    stop("Missing values found in Y/X/Z columns; please handle NAs before calling plsi.lr.auto().")
  }
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Package 'mgcv' is required for plsi.lr.auto(). Please install it.")
  }

  cor_linear <- stats::cor(data[, c(Y.name, X.name)])
  cor_linear <- as.data.frame(cor_linear[-1, 1])
  cor_linear$exposure <- rownames(cor_linear)
  X.name <- cor_linear[order(cor_linear[, 1], decreasing = TRUE), 2]

  y <- data[, Y.name]; x <- as.matrix(data[, X.name]); z_df <- data[, Z.name, drop = FALSE]
  n <- nrow(data); x_length <- length(X.name); z_length <- length(Z.name)

  m0 <- stats::glm(y ~ x + as.matrix(z_df), family = "gaussian")

  initial_table <- as.data.frame(matrix(NA, nrow = (1 + initial.random.num), ncol = x_length))
  colnames(initial_table) <- X.name
  rownames(initial_table) <- c("linear", paste0("random_", 1:initial.random.num))
  initial_table[1, ] <- m0$coefficients[2:(1 + x_length)]
  initial_table[2:(1 + initial.random.num), ] <- stats::runif(initial.random.num * x_length, -1, 1)
  for (i in 1:nrow(initial_table)) {
    initial_table[i, ] <- initial_table[i, ] * sign(initial_table[i, 1]) / sqrt(sum(initial_table[i, ]^2))
  }

  # REML-penalized smooth link, refit for each candidate direction beta.
  # No spline.num / degrees of freedom to hand-tune: 'k' is just a ceiling,
  # and REML shrinks the smooth toward linear on its own.
  fit_gam <- function(beta_0) {
    u0 <- as.vector(x %*% as.vector(beta_0))
    dat_gam <- cbind(data.frame(y = y, u0 = u0), z_df)
    form <- stats::as.formula(paste0("y ~ s(u0, bs='", bs, "', k=", k, ") + ",
                                     paste(Z.name, collapse = " + ")))
    mgcv::gam(form, data = dat_gam, method = "REML")
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
  # optimum only, the same way plsi.lr.v2() derives beta_sigma from optim()'s
  # Hessian.
  hess <- stats::optimHess(par = beta_BeforeNorm, fn = fn, control = list(fnscale = -1))
  info_matrix <- -hess

  eig_decomp <- eigen(info_matrix, symmetric = TRUE)
  eig_vals <- eig_decomp$values
  if (any(eig_vals <= 0)) {
    warning("Observed information matrix is not positive definite at the selected optimum ",
            "(min eigenvalue = ", signif(min(eig_vals), 3), "). Standard errors are computed from ",
            "an eigenvalue-floored version of the information matrix and should be treated as ",
            "conservative approximations; consider increasing initial.random.num for a more ",
            "reliable optimum.")
    # optimHess() differentiates fn(), which itself refits a penalized gam()
    # (an inner iterative optimization) at every evaluation -- small
    # discontinuities from that inner fit landing in slightly different places
    # for nearby beta vectors can corrupt the finite-difference Hessian's
    # eigenvalues even when the underlying likelihood surface is well-behaved.
    # Floor tiny/negative eigenvalues to a small positive value (relative to
    # the largest eigenvalue) so the matrix is positive definite and
    # invertible, rather than silently producing NaN/nonsensical SEs from
    # ginv() on an indefinite matrix -- this makes the affected SEs
    # conservative (larger) rather than wrong.
    floor_val <- max(eig_vals) * 1e-6
    eig_vals <- pmax(eig_vals, floor_val)
    info_matrix <- eig_decomp$vectors %*% diag(eig_vals, nrow = length(eig_vals)) %*% t(eig_decomp$vectors)
  }

  beta_BeforeNorm_sigma <- suppressWarnings(sqrt(diag(MASS::ginv(info_matrix))))

  beta_est <- beta_BeforeNorm * sign(beta_BeforeNorm[1]) / sqrt(sum(beta_BeforeNorm^2))
  beta_sigma <- beta_BeforeNorm_sigma / sqrt(sum(beta_BeforeNorm^2))

  final_model <- fit_gam(beta_est)
  s <- summary(final_model)

  si.coefficient <- data.frame(Estimate = as.numeric(beta_est),
                               `Std.Error` = as.numeric(beta_sigma),
                               row.names = X.name, check.names = FALSE)
  si.coefficient$`t value` <- si.coefficient$Estimate / si.coefficient$`Std.Error`
  si.coefficient$`Pr(>|t|)` <- ifelse(2 * stats::pnorm(-abs(si.coefficient$`t value`)) < 0.0001, "<.0001",
                                      format(round(2 * stats::pnorm(-abs(si.coefficient$`t value`)), 4), nsmall = 4))
  si.coefficient$Lower.95CI <- si.coefficient$Estimate + stats::qnorm(0.025) * si.coefficient$`Std.Error`
  si.coefficient$Upper.95CI <- si.coefficient$Estimate + stats::qnorm(0.975) * si.coefficient$`Std.Error`
  si.coefficient$`Contribution proportion` <- format(round((si.coefficient$Estimate)^2, 3), nsmall = 3)
  # Sort by Estimate, descending (largest coefficient first) -- matches plsi.lr.v2()
  si.coefficient <- si.coefficient[order(si.coefficient$Estimate, decreasing = TRUE), ]

  confounder.coefficient <- as.data.frame(s$p.table[Z.name, , drop = FALSE])
  confounder.coefficient$Lower.95CI <- confounder.coefficient$Estimate + stats::qnorm(0.025) * confounder.coefficient$`Std. Error`
  confounder.coefficient$Upper.95CI <- confounder.coefficient$Estimate + stats::qnorm(0.975) * confounder.coefficient$`Std. Error`
  confounder.coefficient$`Pr(>|t|)` <- ifelse(confounder.coefficient$`Pr(>|t|)` < 0.0001, "<.0001",
                                              format(round(confounder.coefficient$`Pr(>|t|)`, 4), nsmall = 4))

  single_index_estimated <- as.vector(x %*% beta_est)

  # Confounder-free single-index model for prediction -------------------------
  # `final_model` still has AGE.c / SEX.Female / etc. in its formula, so
  # predict() on it requires those columns in newdata. That's fine for the
  # in-sample diagnostics above, but breaks any downstream use that only has
  # an index value and no confounder values to supply -- e.g. plotting the
  # response curve for a single exposure via beta[exposure] * x_value.
  # Mirror plsi.lr.v2()'s approach: residualize y on the confounders using
  # final_model's parametric coefficients, then fit a second, confounder-free
  # smooth of the residualized outcome on the single index alone. Predict from
  # THIS model (si.fun.model) for any new/custom single-index values.
  confounder_coefs <- stats::coef(final_model)[Z.name]
  y_single_index <- y - as.vector(as.matrix(z_df) %*% confounder_coefs)

  dat_si_fun <- data.frame(single_index_estimated = single_index_estimated,
                           y_single_index = y_single_index)
  form_si <- stats::as.formula(paste0("y_single_index ~ s(single_index_estimated, bs='", bs, "', k=", k, ")"))
  m2 <- mgcv::gam(form_si, data = dat_si_fun, method = "REML")

  # NOTE: ciTools::add_ci() does not support "gam" objects from mgcv (it
  # covers lm/glm/lmerMod/glmerMod) -- that mismatch is what produced the
  # "object 'AGE.c' not found" error upstream. Use predict.gam(se.fit=TRUE)
  # directly and build the Wald interval by hand instead. This also sidesteps
  # the predict(type="terms") column-naming fragility entirely, since m2 has
  # only one smooth term.
  ord <- order(single_index_estimated)
  pred2 <- stats::predict(m2, newdata = dat_si_fun[ord, "single_index_estimated", drop = FALSE],
                          type = "link", se.fit = TRUE)
  si.fun <- data.frame(single_index_estimated = single_index_estimated[ord],
                       y_single_index = y_single_index[ord],
                       fit = pred2$fit,
                       se  = pred2$se.fit)
  si.fun$lwr <- si.fun$fit - stats::qnorm(0.975) * si.fun$se
  si.fun$upr <- si.fun$fit + stats::qnorm(0.975) * si.fun$se

  return(list(original.data = list(y = y, x = x, z = z_df),
              original.par = list(k = k, bs = bs, initial.random.num = initial.random.num, seed = seed),
              si.coefficient = si.coefficient,
              model.statistics = crit_table[order_ob, ],
              initial.table = initial_table,
              confounder.coefficient = confounder.coefficient,
              si.fun.edf = s$edf[1],          # effective df actually used -- this is the "auto-tuned" answer
              si.fun = si.fun,
              si.fun.model = m2,              # confounder-free: predict() only needs single_index_estimated
              full.model = final_model))      # full fit incl. confounders, kept for reference/diagnostics
}
