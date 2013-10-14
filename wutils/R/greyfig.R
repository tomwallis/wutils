#' make a ggplot2 figure into a discernable greyscale plot.
#' 
#' This function accepts a ggplot object and returns the same object with colour, fill and shape scales
#' adjusted to produce up to four discernable greyscale discrete data sets.
#' 
#' @export  
#' @aliases grayfig
#'
#' 
#' @return returns a ggplot2 object with adjusted manual colour scale, fill scale and linetypes.
#' 
#' @param fig a ggplot2 object.
#' @param ... further arguments passed to scale_colour_manual and scale_fill_manual (e.g. could pass name='Scale name').
#'
#' @author Thomas Wallis (with colours collected from around the web)
#' @examples
#' library(ggplot2)
#' data(diamonds)
#' diamonds <- diamonds[sample(nrow(diamonds),size=2000),]
#' diamonds <- subset(diamonds,cut==c('Good','Very Good','Premium','Ideal'))
#' fig <- ggplot(diamonds,aes(x=carat,y=price,colour=cut,fill=cut,shape=cut)) + geom_point()
#' fig <- greyfig(fig)
#' fig

greyfig <- function(fig,...){

  colour_values <- c('#000000','#000000','#000000','#000000')
  fill_values <- c('#000000','#ffffff','#cccccc','#666666')
  shape_values <- c(21,22,23,24)
  
  fig <- fig + scale_colour_manual(values=colour_values,...) + scale_fill_manual(values=fill_values,...) + scale_shape_manual(values=shape_values,...)
  return(fig)
}