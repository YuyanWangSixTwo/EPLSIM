#' Transformation for confounder vector Z
#'
#' @param Z_continuous A character name vector for continuous confounders
#' @param Z_discrete A character name vector for discrete confounders
#' @param data Orginial data set
#' @return Transformed confounder vector and data set ready for further analysis.
#'
#' @example vignettes/example.confounder.trans.R
#' @keywords confounder
#' @author Yuyan Wang
#' @export
#'
confounder.trans <- function(Z_continuous, Z_discrete, data){
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
  return(list(New.Name = Z_new, Updated.data = data))
}
