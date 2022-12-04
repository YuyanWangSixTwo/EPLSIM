#' version 1: partial linear single index linear regression for scalar outcome
#'
#' @param data A data set
#' @param Y Character for scalar outcome
#' @param X A character vector for exposures
#' @param Z A character vector for covariates
#' @param spline_num A number representing the degree of freedom of B-spline basis for link function
#' @param spline_degree A number representing the degree of the piece-wise polynomial of B-spline basis for link function
#' @param initial_random_num A number representing the number of random initials used in the function
#'
#' @return A list of model estimation and prediction results
#' @export
#' @examples
#' \dontrun{
#' sum("a")
#' }
plsi_lr_v1 <- function(data, Y, X, Z, spline_num, spline_degree, initial_random_num)
{
  # data = dat; Y = Y; X = X; Z = Z ; spline_num = 5 ; spline_degree = 3 ; initial_random_num = 5
  y = data[,Y]; x = as.matrix(data[,X]); z = as.matrix(data[,Z])
  n = nrow(data); x_length = length(X); z_length = length(Z)
  m0 = glm(y~x+z, data = dat)

  ### initial tables
  initial_table = as.data.frame(matrix(NA, nrow = (1 + initial_random_num), ncol = (x_length + 3)))
  colnames(initial_table) = c(X, "-2 log L", "AIC", "BIC")
  rownames(initial_table) = c("linear", paste("random_", 1:initial_random_num, sep=""))
  ### initial: linear
  initial_table[1, 1:x_length] = m0$coefficients[(1+1):(1+x_length)]
  ### random initials
  set.seed(2022)
  initial_table[2:(1+initial_random_num), 1:x_length]=runif(initial_random_num*(x_length),-1,1)
  for (i in 1:nrow(initial_table)) {
    initial_table[i,1:x_length] = initial_table[i,1:x_length]*sign(initial_table[i,1])/sqrt(sum(initial_table[i,1:x_length]^2))
  }

  ### objective function
  fn<-function(beta_0){
    # beta_0 = as.matrix(t(initial_table[1, 1:x_length]))
    flush.console()
    u0 = as.matrix(data[,X]) %*% as.matrix(beta_0)
    B_link = ns(u0, df = spline_num, intercept = TRUE)
    x_new = as.matrix(B_link)
    m1 = glm(y~-1+x_new+z)
    Ln = logLik(m1)
    Ln
  }

  ### get estimates from different initials
  # plsi.lr.est.beta = as.data.frame(matrix(NA, nrow = (1 + initial_random_num), ncol = (x_length)))
  # colnames(plsi.lr.est.beta) = X
  # rownames(plsi.lr.est.beta) = c("linear", paste("random_", 1:initial_random_num, sep=""))
  for (i in 1:nrow(initial_table)) {
    beta_0 = as.matrix(initial_table[i,1:x_length])
    m2 = optim(par=beta_0, fn=fn, gr=NULL, hessian=TRUE, control=list("fnscale"=-1,maxit=1000))
    initial_table[i, c("-2 log L")] =-2* m2$value
    initial_table[i, c("AIC")] =  -2*m2$value + 2*(x_length+z_length+spline_num-1)
    initial_table[i, c("BIC")] =  -2*m2$value + (x_length+z_length+spline_num-1)*log(n)
    # beta_est = m2$par
    # plsi.lr.est.beta[i, ] = beta_est*sign(beta_est[1])/sqrt(sum(beta_est^2))
  }

  ### select the estimate with minimum BIC
  order_ob=order(initial_table$BIC)[1]
  model_statistics=initial_table[order_ob,c("-2 log L","AIC","BIC")]

  ### get beta estimate
  beta_selected_initial <- initial_table[order_ob, 1:x_length]
  m_optim <- optim(par=beta_selected_initial,fn=fn, gr=NULL, hessian=TRUE, control=list("fnscale"=-1,maxit=1000))
  beta_BeforeNorm_est <- m_optim$par
  beta_BeforeNorm_sigma <-sqrt(diag(solve(-m_optim$hessian)))
  beta_est <- beta_BeforeNorm_est*sign(beta_BeforeNorm_est[1])/sqrt(sum(beta_BeforeNorm_est^2))
  beta_sigma <- beta_BeforeNorm_sigma/sqrt(sum(beta_BeforeNorm_est^2))
  beta_results <- as.data.frame(cbind(beta_est,beta_sigma))
  beta_results$lower <- beta_results$beta_est + qnorm(0.025)*beta_results$beta_sigma
  beta_results$upper <- beta_results$beta_est + qnorm(0.975)*beta_results$beta_sigma

  beta_results$tvalue <- beta_results$beta_est/beta_results$beta_sigma
  beta_results$pvalue <- ifelse(2*pnorm(-abs(beta_results$tvalue))<0.0001, "<.0001",
                                format(round(2*pnorm(-abs(beta_results$tvalue)),4), nsmall = 4))
  beta_results$contri_proportion <- format(round((beta_results$beta_est)^2, 3), nsmall = 3)
  beta_results <- beta_results[order(beta_results$beta_est),]

  ### get link function estimate
  index_estimated <- as.vector(x %*% as.vector(beta_est))
  link_bspline_estimated = ns(index_estimated, df = spline_num, intercept = TRUE)
  x_new_est = as.matrix(link_bspline_estimated)
  m1 = glm(y~-1+x_new_est+z)
  y_new = y - as.vector(z %*% as.vector(m1$coefficients[(spline_num+1):(spline_num+z_length)]))
  m2 = glm(y_new~-1+x_new_est)
  lambda_estimated <- m2$coefficients
  dat_link <- as.data.frame(cbind(y_new,x_new_est))
  link_ci <- add_ci(dat_link, m2, alpha = 0.05, names = c("lwr", "upr"))
  link_ci$index_est <- index_estimated
  link_ci <- link_ci[,c("index_est","y_new","pred","lwr","upr")]

  ### for partial linear coefficients
  alpha_estimated <- as.data.frame(summary(m1)$coefficients)[(spline_num+1):(spline_num+z_length),]
  alpha_estimated$lower <- alpha_estimated$Estimate + qnorm(0.025)*alpha_estimated$`Std. Error`
  alpha_estimated$upper <- alpha_estimated$Estimate + qnorm(0.975)*alpha_estimated$`Std. Error`
  alpha_estimated$pvalue <- ifelse(alpha_estimated$`Pr(>|t|)`<0.0001, "<.0001",
                                   format(round(alpha_estimated$`Pr(>|t|)`,4), nsmall = 4))

  ### output
  list(beta_results=beta_results, model_statistics=model_statistics, beta_selected_initial=beta_selected_initial,
       alpha_estimated=alpha_estimated, lambda_estimated=lambda_estimated,
       index_estimated = index_estimated, link_bspline_estimated=link_bspline_estimated, link_ci=link_ci)
}

