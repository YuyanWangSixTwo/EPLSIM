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
  # data = dat; Y = Y; X = X; Z = Z
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

  fn<-function(beta_0){
    # beta_0 = as.matrix(t(initial_table[1, 1:x_length]))
    flush.console()
    u0 = as.matrix(data[,X]) %*% as.matrix(beta_0)
    B_link = ns(u0, df = spline_num, intercept = TRUE)
    x_new = as.matrix(B_link)
    m1 = glm(y~-1+x_new+z)
    Ln = -2*logLik(m1)
    Ln
  }

  plsi.lr.est.beta = as.data.frame(matrix(NA, nrow = (1 + initial_random_num), ncol = (x_length)))
  colnames(plsi.lr.est.beta) = X
  rownames(plsi.lr.est.beta) = c("linear", paste("random_", 1:initial_random_num, sep=""))
  for (i in 1:nrow(initial_table)) {
    beta_0 = as.matrix(initial_table[i,1:x_length])
    m2 = optim(par=beta_0, fn=fn, gr=NULL)
    initial_table[i, c("-2 log L")] = m2$value
    initial_table[i, c("AIC")] =  m2$value + 2*(x_length+z_length+spline_num-1)
    initial_table[i, c("BIC")] =  m2$value + (x_length+z_length+spline_num-1)*log(n)
    beta_est = m2$par
    plsi.lr.est.beta[i, ] = beta_est*sign(beta_est[1])/sqrt(sum(beta_est^2))
  }
  order_ob=order(initial_table$BIC)[1]

  beta_selected_initial <- initial_table[order_ob, 1:x_length]
  model_statistics <- initial_table[order_ob, (x_length+1):(x_length+3)]
  beta_estimated <- plsi.lr.est.beta[i, ]
  index_estimated <- as.vector(x %*% t(beta_estimated))
  link_bspline_estimated = ns(index_estimated, df = spline_num, intercept = TRUE)
  x_new_est = as.matrix(link_bspline_estimated)
  m1 = glm(y~-1+x_new_est+z)
  theta_estimated <- m1$coefficients
  lambda_estimated <- theta_estimated[1:spline_num]
  alpha_estimated <- theta_estimated[(spline_num+1):(spline_num+z_length)]
  link_estimated <- as.vector(link_bspline_estimated %*% as.matrix(lambda_estimated))

  list(beta_estimated=beta_estimated, model_statistics=model_statistics, beta_selected_initial=beta_selected_initial,
       alpha_estimated=alpha_estimated, lambda_estimated=lambda_estimated,
       index_estimated=index_estimated, link_estimated=link_estimated, link_bspline_estimated=link_bspline_estimated)
}

