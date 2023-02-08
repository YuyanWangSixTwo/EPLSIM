#' plot estimated single index link function
#'
#' @param link_ci A data set of estimated index, link value, and CIs
#' @param cut A numeric value of percentile cut in two tails of x-axis
#'
#' @return Link function plot
#' @export
#' @examples
#' \dontrun{
#' sum("a")
#' }
link_plot <- function(link_ci, cut){
  # link_ci = model_1$link_ci
  # cut <- 0.02
  link_plot = link_ci[order(link_ci[,1]),]
  m = floor(nrow(link_plot)*cut/2)
  link_plot = link_plot[-(1:m),]
  link_plot = link_plot[1:(nrow(link_plot)-m),]
  plot(link_plot[,c("index_estimated")], link_plot[,c("pred")], type="l",lwd=2,
       xlab="Index",ylab="g(index)", ylim = c(min(link_plot[,c("lwr")]-0.5),max(link_plot[,c("upr")]+0.5)))
  lines(link_plot[,c("index_estimated")], link_plot[,c("lwr")], type="l", lty=2)
  lines(link_plot[,c("index_estimated")], link_plot[,c("upr")], type="l", lty=2)
  axis(side=1,at=link_plot[,1],labels=FALSE,NA,tck=0.016)
}
