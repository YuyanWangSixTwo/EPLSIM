#' plot estimated single index link function
#'
#' @param index A numeric vector of estimated single index
#' @param link A numeric vector of link function value
#' @param cut A numeric value of percentile cut in two tails of x-axis
#'
#' @return Link function plot
#' @export
#'
#' @examples
#' index <- index
#' link <- link
#' cut <- 0.02
#' link_plot(index=index, link=link, cut=cut)
link_plot <- function(index, link, cut){
  cut <- 0.02
  link_plot = as.data.frame(cbind(index, link))
  link_plot = link_plot[order(link_plot[,1]),]
  m = floor(nrow(link_plot)*cut/2)
  link_plot = link_plot[-(1:m),]
  link_plot = link_plot[1:(nrow(link_plot)-m),]
  plot(link_plot[,1], link_plot[,2], type="l",lwd=2,col=2,xlab="Index",ylab="g(index)")
  axis(side=1,at=link_plot[,1],labels=FALSE,NA,tck=0.026)
}
