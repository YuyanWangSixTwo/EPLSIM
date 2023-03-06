#' Partial linear single index linear regression for scalar outcome
#'
#' @param data A data set
#' @param Y.name Variable name for scalar outcome
#' @param X.name Variable name vector for exposures
#' @param Z.name Variable name vector for confounders
#' @param spline.num A number representing the degree of freedom of B-spline basis for link function
#' @param spline.degree A number representing the degree of the piece-wise polynomial of B-spline basis for link function
#' @param initial.random.num A number representing the number of random initials used in the function
#' @importFrom stats qnorm
#' @return A list of model estimation and prediction results
#'
#' @example vignettes/example.plsi.lr.v1.R
#' @keywords plsi.lr
#' @author Yuyan Wang
#' @export
#'
plsi.lr.v1 <- function(data, Y.name, X.name, Z.name, spline.num, spline.degree, initial.random.num)
{
  # data = dat; Y.name = Y.name; X.name = X.name; Z.name = Z.name
  # spline.num = 5 ; spline.degree = 3 ; initial.random.num = 5

  # recorder the exposure
  cor_linear <- stats::cor(data[ , c(Y.name, X.name)])
  cor_linear <- as.data.frame(cor_linear[-1, 1])
  cor_linear$exposure <- rownames(cor_linear)
  X.name <- cor_linear[order(cor_linear[, 1], decreasing = TRUE), 2]

  # get the values
  y <- data[,Y.name]; x <- as.matrix(data[,X.name]); z <- as.matrix(data[,Z.name])

  # get the length and linear model
  n <- nrow(data); x_length <- length(X.name); z_length <- length(Z.name)
  m0 <- stats::glm(y~x+z, data, family = "gaussian")

  # initial tables
  initial_table = as.data.frame(matrix(NA, nrow = (1 + initial.random.num), ncol = (x_length + 3)))
  colnames(initial_table) <- c(X.name, "-2 log L", "AIC", "BIC")
  rownames(initial_table) <- c("linear", paste("random_", 1:initial.random.num, sep=""))
  # initial from linear regression
  initial_table[1, 1:x_length] <- m0$coefficients[(1 + 1):(1 + x_length)]
  # random initials
  # set.seed(2022)
  # set.seed()
  initial_table[2:(1+initial.random.num), 1:x_length] = stats::runif(initial.random.num*(x_length), -1, 1)
  for (i in 1:nrow(initial_table)) {
    initial_table[i, 1:x_length] <- initial_table[i, 1:x_length]*sign(initial_table[i, 1])/sqrt(sum(initial_table[i, 1:x_length]^2))
  }

  # objective function
  fn <- function(beta_0){
    utils::flush.console()
    u0 <- as.matrix(x) %*% as.matrix(beta_0)
    x_new <- as.matrix(splines::ns(u0, df = spline.num, intercept = TRUE))
    m1 <- stats::glm(y ~ -1 + x_new + z)
    Ln <- stats::logLik(m1)
    Ln
  }

  # get estimates from different initials
  initial_results_list <- list()
  for (i in 1:nrow(initial_table)) {
    beta_0 <- as.matrix(initial_table[i, 1:x_length])
    m2_temp <- stats::optim(par = beta_0, fn = fn, gr = NULL, hessian = TRUE, control = list("fnscale" = -1, maxit = 100))
    initial_results_list <- c(initial_results_list, m2_temp)
    initial_table[i, c("-2 log L")] = -2 * m2_temp$value
    initial_table[i, c("AIC")] = -2 * m2_temp$value + 2 * (x_length + z_length + spline.num - 1)
    initial_table[i, c("BIC")] = -2 * m2_temp$value + (x_length + z_length + spline.num - 1)*log(n)
  }
  length_temp = length(m2_temp)

  # select the estimate with minimum BIC
  order_ob = order(initial_table$BIC)[1]

  # get the selected estimated results
  model_statistics = initial_table[order_ob, c("-2 log L", "AIC", "BIC")]
  m_selected <- initial_results_list[(length_temp * (order_ob - 1) + 1):(length_temp * order_ob)]
  beta_BeforeNorm_est <- m_selected$par
  beta_BeforeNorm_sigma <- suppressWarnings(sqrt(diag(MASS::ginv(-m_selected$hessian))))

  # get the single index coefficient estimation
  beta_est <- beta_BeforeNorm_est * sign(beta_BeforeNorm_est[1])/sqrt(sum(beta_BeforeNorm_est^2))
  beta_sigma <- beta_BeforeNorm_sigma/sqrt(sum(beta_BeforeNorm_est^2))
  beta_results <- as.data.frame(cbind(t(beta_est), beta_sigma))
  colnames(beta_results) <- c("Estimate", "Std.Error")
  beta_results$`t value` <- beta_results$Estimate / beta_results$`Std.Error`
  beta_results$`Pr(>|t|)` <- ifelse(2 * stats::pnorm(-abs(beta_results$`t value`)) < 0.0001, "<.0001",
                                format(round(2 * stats::pnorm(-abs(beta_results$`t value`)), 4), nsmall = 4))
  beta_results$Lower.95CI <- beta_results$Estimate + stats::qnorm(0.025) * beta_results$`Std.Error`
  beta_results$Upper.95CI <- beta_results$Estimate + stats::qnorm(0.975) * beta_results$`Std.Error`

  beta_results$`Contribution proportion` <- format(round((beta_results$Estimate)^2, 3), nsmall = 3)
  beta_results <- beta_results[order(beta_results$Estimate, decreasing = T), ]

  # get sing index function estimation
  single_index_estimated <- as.vector(x %*% as.vector(beta_est))
  m1 <- stats::glm(y ~ splines::ns(single_index_estimated, df = spline.num) + z)
  y_single_index = y - as.vector(z %*% as.vector(m1$coefficients[(spline.num + 2):(spline.num + 1 + z_length)]))
  si_fun_coef_estimated <- as.data.frame(summary(m1)$coefficients)[2:(1 + spline.num), ]
  dat_si_fun <- as.data.frame(cbind(single_index_estimated, y_single_index))
  m2 <- stats::glm(y_single_index ~ splines::ns(single_index_estimated, df = spline.num))
  dat_si_fun_ci <- ciTools::add_ci(dat_si_fun, m2, alpha = 0.05, names = c("lwr", "upr"))

  # partial linear coefficient estimation
  confounder_coef_estimated <- as.data.frame(summary(m1)$coefficients)[(spline.num + 2):(spline.num + 1 + z_length),]
  confounder_coef_estimated$Lower.95CI <- confounder_coef_estimated$Estimate + qnorm(0.025)*confounder_coef_estimated$`Std. Error`
  confounder_coef_estimated$Upper.95CI <- confounder_coef_estimated$Estimate + qnorm(0.975)*confounder_coef_estimated$`Std. Error`
  confounder_coef_estimated$`Pr(>|t|)` <- ifelse(confounder_coef_estimated$`Pr(>|t|)`< 0.0001, "<.0001",
                                   format(round(confounder_coef_estimated$`Pr(>|t|)`, 4), nsmall = 4))

  # output
  return(list(original.data = list(y = y, x = x, z = z),
       original.par = list(spline.num = spline.num, spline.degree = spline.degree, initial.random.num = initial.random.num),
       si.coefficient = beta_results, model.statistics = model_statistics,
       intial.table = initial_table,
       all.intial.results = initial_results_list,
       confounder.coefficient = confounder_coef_estimated, si.fun.bs.coef = si_fun_coef_estimated,
       si.fun = dat_si_fun_ci, si.fun.model = m2))
}
