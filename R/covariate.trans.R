#' confounder vector Z transformation
#'
#' @param Z_continuous A character name vector for continuous confounder
#' @param Z_discrete A character name vector for discrete confounder
#' @param data A data set
#'
#' @return updated confounder vector and data set.
#' @export
#' @examples
#' \dontrun{
#' sum("a")
#' }
covariate.trans <- function(Z_continuous, Z_discrete, data){
  Z_c = paste(Z_continuous, ".c", sep = "")
  data[, Z_c] = scale(data[, Z_continuous])
  Z_d = NULL
  for (j in 1:length(Z_discrete)) {
    ll = length(levels(data[, Z_discrete[j]]))
    Z_new = paste(Z_discrete[j], ".", levels(data[, Z_discrete[j]])[2:ll], sep = "")
    Z_d = c(Z_d, Z_new)
    for (k in 1:length(Z_new)) {
      data[, Z_new[k]] = ifelse(data[, Z_discrete[j]]==levels(data[, Z_discrete[j]])[k + 1], 1, 0)
    }
  }
  Z_new = c(Z_c, Z_d)
  return(list(Z_new, data))
}
