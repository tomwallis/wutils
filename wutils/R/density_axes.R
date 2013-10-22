#' make a ggplot2 figure with density axes (Tufte).
#' 
#' This function accepts a ggplot object and returns the same object with the 
#' axes adjusted to be minimal density values of the data points.
#' Assumes that there is x and y data in the plot.
#' 
#' @export  
#' 
#' @return returns a ggplot2 object.
#' 
#' @param fig a ggplot2 object.
#' @param base_size   the base text size of the plain theme.
#' @param plot_line   if false, don't plot axis lines (density plots of data alone)
#'
#' @author Thomas Wallis (with colours collected from around the web)
#' @examples
#' library(ggplot2)
#' data(diamonds)
#' diamonds <- diamonds[sample(nrow(diamonds),size=2000),]
#' diamonds <- subset(diamonds,cut==c('Good','Very Good','Premium','Ideal'))
#' fig <- ggplot(diamonds,aes(x=carat,y=price,colour=cut,fill=cut,shape=cut)) + geom_point(size=3)
#' fig <- greyfig(fig)
#' fig <- density_axes(fig,plot_line=FALSE)
#' fig

density_axes <- function(fig,base_size=12,plot_line=TRUE){
  fig <- fig + theme_classic(base_size=base_size)
  
  # get x and y data from plots:
  dat <- fig$data
  text <- paste0('x <- dat$',as.character(fig$mapping$x))
  eval(parse(text=text))
  text <- paste0('y <- dat$',as.character(fig$mapping$y))
  eval(parse(text=text))
  
  # not clear how this will interact with faceting. Probably won't allow free axes.
  
  # x labels should just show min and max:
  x_labels <- rep('',times=length(x))
  x_labels[1] <- min(x)
  x_labels[length(x)] <- max(x)
  
  y_labels <- rep('',times=length(y))
  y_labels[1] <- min(y)
  y_labels[length(y)] <- max(y)
  
  fig <- fig + scale_x_continuous(limits=c(min(x),max(x)),breaks=sort(x),labels=x_labels)
  fig <- fig + scale_y_continuous(limits=c(min(y),max(y)),breaks=sort(y),labels=y_labels)
  
  if (plot_line == FALSE) fig <- fig + theme(axis.line = element_blank())
  
  return(fig)
}