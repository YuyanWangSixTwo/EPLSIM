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
  beta_plot$pos = (beta_plot$Estimate>=0)
  beta_plot = beta_plot[order(beta_plot$Estimate),]

  ggplot(beta_plot, aes(x = reorder(exp, -Estimate), y = Estimate, fill = pos)) +
    geom_col(position = "identity", width = 0.6) +
    geom_bar(stat = "identity", width = 0.6) +
    geom_errorbar(aes(x = reorder(exp, -Estimate), ymin = Lower, ymax = Upper), colour="black", width=0.3, alpha=0.6) +
    geom_text(aes(label = format(round(Estimate, 3), nsmall = 3)), hjust = 1.0) +
    ggtitle("Relative effect in single index") +
    labs(y = "Eelative effect (beta)", x = "Exposure") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none",
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text = element_text(colour = "black", size = rel(1.0))) +
    scale_fill_manual(values=c("#12ffac", "#ff8112")) +
    geom_hline(yintercept=0, linetype="dashed")
}
