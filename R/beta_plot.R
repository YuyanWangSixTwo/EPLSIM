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
  beta_plot = beta_est
  beta_plot$exp = rownames(beta_plot)
  beta_plot$pos = (beta_plot$beta_est>=0)
  beta_plot = beta_plot[order(beta_plot$beta_estimate),]

  ggplot(beta_plot, aes(x = reorder(exp, -beta_estimate), y = beta_estimate, fill = pos)) +
    geom_col(position = "identity") +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(x = reorder(exp, -beta_estimate), ymin = lower, ymax = upper), colour="black", width=0.4, alpha=0.9) +
    geom_text(aes(label = round(beta_estimate,3)), hjust = 1.0) +
    ggtitle("Relative effect in single index") +
    labs(y = "Eelative effect (beta)", x = "Exposure") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none",
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text = element_text(colour = "black", size = rel(1.0))) +
    scale_fill_manual(values=c("green", "orange"))
}
