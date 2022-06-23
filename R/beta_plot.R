#' plot estimated single index beta value
#'
#' @param beta_est A data set for beta plot
#'
#' @return beta value plot
#' @export
#' @examples
#' \dontrun{
#' sum("a")
#' }
beta_plot <- function(beta_est){
  beta_plot = as.data.frame(t(beta_est))
  colnames(beta_plot) = "beta_est"
  beta_plot$exp = rownames(beta_plot)
  beta_plot$pos = (beta_plot$beta_est>=0)
  beta_plot = beta_plot[order(beta_plot$beta_est),]

  ggplot(beta_plot, aes(x = reorder(exp, -beta_est), y = beta_est, fill = pos)) +
    geom_col(position = "identity") +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(beta_est,3)), vjust = 0.5) +
    ggtitle("Relative effect in single index") +
    labs(y = "relative effect (beta)", x = "exposure") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")
}
