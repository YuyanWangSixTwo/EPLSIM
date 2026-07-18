#' Plot estimated single index coefficients frpm PLSIM
#'
#' @param si.coef.est A data set of estimated single index coefficients
#' @importFrom stats reorder
#' @return Single index coefficient plot
#'
#' @examples
#' \dontrun{
#' # example to plot estimated single index function of continuous outcome
#' data(nhanes.new)
#' data <- nhanes.new
#'
#' Y.name <- "log.triglyceride"
#' X.name <- c("X1_trans.b.carotene", "X2_retinol", "X3_g.tocopherol", "X4_a.tocopherol",
#'             "X5_PCB99", "X6_PCB156", "X7_PCB206",
#'             "X8_3.3.4.4.5.pncb", "X9_1.2.3.4.7.8.hxcdf", "X10_2.3.4.6.7.8.hxcdf")
#' Z.name <- c("AGE.c", "SEX.Female", "RACE.NH.Black",
#'            "RACE.MexicanAmerican", "RACE.OtherRace", "RACE.Hispanic" )
#'
#' k <- 10
#' bs <- "cr"
#' initial.random.num <- 1
#' seed = 2026
#'
#' model_lr_auto <- plsi.lr.auto(data = data, Y.name = Y.name, X.name = X.name, Z.name = Z.name,
#'                       k = k, bs = bs, initial.random.num = initial.random.num, seed = seed)
#'
#' # plot estimated single index coefficients
#' si.coef.plot(model_lr_auto$si.coefficient)
#'
#' # check estimated single index coefficients
#' model_lr_auto$si.coefficient
#' }
#' @keywords partial linear single index
#' @keywords single index coefficients
#' @author Yuyan Wang
#' @export
#'
si.coef.plot <- function(si.coef.est){
  beta_plot = si.coef.est
  beta_plot$exp = rownames(beta_plot)
  beta_plot$pos = (beta_plot$Estimate>=0)
  beta_plot = beta_plot[order(beta_plot$Estimate), ]

  beta_plot <- beta_plot[order(beta_plot$Estimate, decreasing = T), ]
  final_plot <- ggplot2::ggplot(beta_plot, ggplot2::aes(x = reorder(exp, -beta_plot$Estimate), y = beta_plot$Estimate, fill = beta_plot$pos)) +
    ggplot2::geom_col(position = "identity", width = 0.6) +
    ggplot2::geom_bar(stat = "identity", width = 0.6) +
    ggplot2::geom_errorbar(ggplot2::aes(x = reorder(exp, -beta_plot$Estimate), ymin = beta_plot$Lower.95CI, ymax = beta_plot$Upper.95CI), colour = "black", width = 0.3, alpha = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = format(round(beta_plot$Estimate, 3), nsmall = 3)), hjust = 1.0) +
    ggplot2::ggtitle("") +
    ggplot2::labs(y = "Sing index coefficient", x = "Exposure") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "none",
                   panel.background = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black"),
                   axis.text = ggplot2::element_text(colour = "black", size = ggplot2::rel(1.0))) +
    ggplot2::scale_fill_manual(values = c("#12ffac", "#ff8112")) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed")

  suppressWarnings(print(final_plot))
}
