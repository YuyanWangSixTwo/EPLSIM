#' plot estimated single index coefficients
#'
#' @param si.coef.est A data set of single index coefficient estimations
#' @importFrom stats reorder
#'
#' @return single index coefficient plot
#' @export
#' @examples
#' \dontrun{
#' sum("a")
#' }
si.coef.plot <- function(si.coef.est){
  beta_plot = si.coef.est
  beta_plot$exp = rownames(beta_plot)
  beta_plot$pos = (beta_plot$Estimate>=0)
  beta_plot = beta_plot[order(beta_plot$Estimate), ]

  ggplot2::ggplot(beta_plot, ggplot2::aes(x = reorder(exp, -beta_plot$Estimate), y = beta_plot$Estimate, fill = beta_plot$pos)) +
    ggplot2::geom_col(position = "identity", width = 0.6) +
    ggplot2::geom_bar(stat = "identity", width = 0.6) +
    ggplot2::geom_errorbar(ggplot2::aes(x = reorder(exp, -beta_plot$Estimate), ymin = beta_plot$Lower.95CI, ymax = beta_plot$Upper.95CI), colour = "black", width = 0.3, alpha = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = format(round(beta_plot$Estimate, 3), nsmall = 3)), hjust = 1.0) +
    ggplot2::ggtitle("Relative effect in single index") +
    ggplot2::labs(y = paste("Relative effect ", "\u03B2" ,sep = ""), x = "Exposure") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "none",
          panel.background = ggplot2::element_blank(),
          axis.line = ggplot2::element_line(colour = "black"),
          axis.text = ggplot2::element_text(colour = "black", size = ggplot2::rel(1.0))) +
    ggplot2::scale_fill_manual(values = c("#12ffac", "#ff8112")) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed")
}
