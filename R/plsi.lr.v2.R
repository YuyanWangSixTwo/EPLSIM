#' Partial linear single index linear regression for scalar outcome
#'
#' @param data A data set including all needed variables
#' @param Y.name Variable name for scalar outcome
#' @param X.name Variable name vector for exposures
#' @param Z.name Variable name vector for confounders
#' @param spline.num A number representing the degree of freedom of B-spline basis for link function
#' @param spline.degree A number representing the degree of the piece-wise polynomial of B-spline basis for link function
#' @param initial.random.num A number representing the number of random initials used in the function
#' @param seed A single integer used to seed the RNG so results (random initials
#'   and downstream optim() convergence) are reproducible. Set to NULL to skip
#'   seeding and use whatever RNG state is already active in the calling
#'   environment. Default is 2023.
#' @importFrom stats qnorm
#' @return A list of model estimation and prediction results
#'
#' @examples
#' \dontrun{
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
#' spline.num <- 5
#' spline.degree <- 3
#' initial.random.num <- 1
#' seed = 2026
#'
#' model_lr_v2 <- plsi.lr.v2(data = data, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
#'                       spline.num, spline.degree, initial.random.num, seed = seed)
#' }
#' @keywords partial linear single index
#' @keywords regression models
#' @keywords new version
#' @author Yuyan Wang
#' @export
plsi.lr.v2 <- function(data, Y.name, X.name, Z.name,
                       spline.num, spline.degree, initial.random.num,
                       seed = 2026) {

  stopifnot(initial.random.num >= 1)
  if (!is.null(seed)) set.seed(seed)
  missing_cols <- setdiff(c(Y.name, X.name, Z.name), colnames(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
  }
  if (anyNA(data[, c(Y.name, X.name, Z.name)])) {
    stop("Missing values found in Y/X/Z columns; please handle NAs before calling plsi.lr.v2().")
  }

  # order exposures by correlation with outcome
  cor_linear <- stats::cor(data[, c(Y.name, X.name)])
  cor_linear <- as.data.frame(cor_linear[-1, 1])
  cor_linear$exposure <- rownames(cor_linear)
  X.name <- cor_linear[order(cor_linear[, 1], decreasing = TRUE), 2]

  y <- data[, Y.name]; x <- as.matrix(data[, X.name]); z <- as.matrix(data[, Z.name])

  n <- nrow(data); x_length <- length(X.name); z_length <- length(Z.name)
  m0 <- stats::glm(y ~ x + z, data, family = "gaussian")

  initial_table <- as.data.frame(matrix(NA, nrow = (1 + initial.random.num), ncol = (x_length + 3)))
  colnames(initial_table) <- c(X.name, "-2 log L", "AIC", "BIC")
  rownames(initial_table) <- c("linear", paste("random_", 1:initial.random.num, sep = ""))

  initial_table[1, 1:x_length] <- m0$coefficients[(1 + 1):(1 + x_length)]
  initial_table[2:(1 + initial.random.num), 1:x_length] <-
    stats::runif(initial.random.num * x_length, -1, 1)

  for (i in 1:nrow(initial_table)) {
    initial_table[i, 1:x_length] <- initial_table[i, 1:x_length] * sign(initial_table[i, 1]) /
      sqrt(sum(initial_table[i, 1:x_length]^2))
  }

  fn <- function(beta_0) {
    utils::flush.console()
    u0 <- as.matrix(x) %*% as.matrix(beta_0)
    x_new <- as.matrix(splines::ns(u0, df = spline.num, intercept = TRUE))
    m1 <- stats::glm(y ~ -1 + x_new + z)
    as.numeric(stats::logLik(m1))
  }

  initial_results_list <- vector("list", nrow(initial_table))
  for (i in 1:nrow(initial_table)) {
    beta_0 <- as.numeric(initial_table[i, 1:x_length])   # keep: fixes the matrix-shape crash
    m2_temp <- stats::optim(par = beta_0, fn = fn, method = "L-BFGS-B",
                            hessian = TRUE, control = list(fnscale = -1, maxit = 100))
    initial_results_list[[i]] <- m2_temp
    initial_table[i, "-2 log L"] <- -2 * m2_temp$value
    initial_table[i, "AIC"] <- -2 * m2_temp$value + 2 * (x_length + z_length + spline.num - 1)
    initial_table[i, "BIC"] <- -2 * m2_temp$value + (x_length + z_length + spline.num - 1) * log(n)
  }

  order_ob <- order(initial_table$BIC)[1]
  model_statistics <- initial_table[order_ob, c("-2 log L", "AIC", "BIC")]
  m_selected <- initial_results_list[[order_ob]]
  beta_BeforeNorm_est <- as.numeric(m_selected$par)

  # Original sign convention — empirically confirmed correct on real data.
  info_matrix <- -m_selected$hessian

  # Defensive check only — doesn't alter the numerics, just surfaces a warning
  # if a future dataset/initial lands somewhere the Hessian isn't trustworthy.
  eig <- eigen(info_matrix, symmetric = TRUE, only.values = TRUE)$values
  if (any(eig <= 0)) {
    warning("Observed information matrix is not positive definite at the selected optimum ",
            "(min eigenvalue = ", signif(min(eig), 3), "). Standard errors may be unreliable; ",
            "consider increasing initial.random.num.")
  }

  beta_BeforeNorm_sigma <- suppressWarnings(sqrt(diag(MASS::ginv(info_matrix))))

  beta_est <- beta_BeforeNorm_est * sign(beta_BeforeNorm_est[1]) / sqrt(sum(beta_BeforeNorm_est^2))
  beta_sigma <- beta_BeforeNorm_sigma / sqrt(sum(beta_BeforeNorm_est^2))

  # FIX: cbind() with a transposed row-vector silently recycled/broke column count
  # for x_length > 1. Build the data frame from named vectors directly instead.
  beta_results <- data.frame(Estimate = as.numeric(beta_est),
                             `Std.Error` = as.numeric(beta_sigma),
                             row.names = X.name,
                             check.names = FALSE)
  beta_results$`t value` <- beta_results$Estimate / beta_results$`Std.Error`
  beta_results$`Pr(>|t|)` <- ifelse(2 * stats::pnorm(-abs(beta_results$`t value`)) < 0.0001, "<.0001",
                                    format(round(2 * stats::pnorm(-abs(beta_results$`t value`)), 4), nsmall = 4))
  beta_results$Lower.95CI <- beta_results$Estimate + stats::qnorm(0.025) * beta_results$`Std.Error`
  beta_results$Upper.95CI <- beta_results$Estimate + stats::qnorm(0.975) * beta_results$`Std.Error`
  beta_results$`Contribution proportion` <- format(round((beta_results$Estimate)^2, 3), nsmall = 3)
  # Sort by Estimate, descending (largest coefficient first)
  beta_results <- beta_results[order(beta_results$Estimate, decreasing = TRUE), ]

  single_index_estimated <- as.vector(x %*% as.vector(beta_est))
  m1 <- stats::glm(y ~ splines::ns(single_index_estimated, df = spline.num) + z)
  y_single_index <- y - as.vector(z %*% as.vector(m1$coefficients[(spline.num + 2):(spline.num + 1 + z_length)]))
  si_fun_coef_estimated <- as.data.frame(summary(m1)$coefficients)[2:(1 + spline.num), ]
  dat_si_fun <- as.data.frame(cbind(single_index_estimated, y_single_index))
  m2 <- stats::glm(y_single_index ~ splines::ns(single_index_estimated, df = spline.num))
  # yhatName = "fit" (not ciTools' default "pred") keeps si.fun's column
  # naming consistent with plsi.lr.auto()/plsi.logistic.auto()/plsi.log.auto(),
  # which si.fun.plot(type = "linear") relies on.
  dat_si_fun_ci <- ciTools::add_ci(dat_si_fun, m2, alpha = 0.05, names = c("lwr", "upr"), yhatName = "fit")

  confounder_coef_estimated <- as.data.frame(summary(m1)$coefficients)[(spline.num + 2):(spline.num + 1 + z_length), ]
  confounder_coef_estimated$Lower.95CI <- confounder_coef_estimated$Estimate + stats::qnorm(0.025) * confounder_coef_estimated$`Std. Error`
  confounder_coef_estimated$Upper.95CI <- confounder_coef_estimated$Estimate + stats::qnorm(0.975) * confounder_coef_estimated$`Std. Error`
  confounder_coef_estimated$`Pr(>|t|)` <- ifelse(confounder_coef_estimated$`Pr(>|t|)` < 0.0001, "<.0001",
                                                 format(round(confounder_coef_estimated$`Pr(>|t|)`, 4), nsmall = 4))

  return(list(original.data = list(y = y, x = x, z = z),
              original.par = list(spline.num = spline.num, spline.degree = spline.degree,
                                  initial.random.num = initial.random.num, seed = seed),
              si.coefficient = beta_results, model.statistics = model_statistics,
              initial.table = initial_table,
              all.initial.results = initial_results_list,
              confounder.coefficient = confounder_coef_estimated, si.fun.bs.coef = si_fun_coef_estimated,
              si.fun = dat_si_fun_ci, si.fun.model = m2))
}
