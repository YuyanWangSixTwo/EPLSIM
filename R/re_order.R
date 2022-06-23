#' change the order of the exposure vector
#'
#' @param X A character vector for exposure mixture
#' @param Y A character for outcome
#' @param data A data set
#'
#' @return Re-ordered character vector for exposure mixture.
#' @export
#' @examples
#' \dontrun{
#' sum("a")
#' }
re_order <- function(X, Y, data){
  cor_linear = cor(data[ , c(Y, X)])
  cor_linear = as.data.frame(cor_linear[-1, 1])
  cor_linear$exposure = rownames(cor_linear)
  re_order_vec = cor_linear[order(cor_linear[,1], decreasing = TRUE),2]
  return(re_order_vec)
}
