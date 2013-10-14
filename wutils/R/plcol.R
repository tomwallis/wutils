#' Add Tom's favourite plot colours to a ggplot object.
#' 
#' This function accepts a ggplot object and returns the object with the colour and fill scales
#' adjusted to the specified colour scheme.
#' 
#' @export  
#' 
#' @details
#' Possible arguments: 
#' \itemize{
#' \item \code{'cb'}: return colourblind friendly palette (default).
#' \item \code{'pleasing'}: return hand-tuned pleasing colours.
#' }
#' 
#' If you want to make a nice greyscale ggplot2 figure, use \link{greyfig}.
#' 
#' @return returns a ggplot2 object with adjusted manual colour scale and fill scale.
#' 
#' @param fig a ggplot2 object.
#' @param palette a string argument for which colour palette to return.
#' @param ... further arguments passed to scale_colour_manual and scale_fill_manual (e.g. could pass name='Scale name').
#'
#' @author Thomas Wallis (with colours collected from around the web)
#' @examples
#' library(ggplot2)
#' data(diamonds)
#' diamonds <- diamonds[sample(nrow(diamonds),size=2000),]
#' diamonds <- subset(diamonds,cut==c('Good','Very Good','Premium','Ideal'))
#' fig <- ggplot(diamonds,aes(x=carat,y=price,colour=cut)) + geom_point()
#' fig <- plcol(fig,palette='pleasing')
#' fig

plcol <- function(fig,palette='cb',...){
  # hand-tuned pleasing colours
  if(palette == 'pleasing') plcol <- c('#A6CEE3','#1F78B4','#B2DF8A','#33A02C')
  # Colourblind-friendly palette with grey:
  if(palette == 'cb') plcol <- c("#E69F00", "#56B4E9", "#009E73","#0072B2", "#D55E00", "#CC79A7","#F0E442")
  
  fig <- fig + scale_colour_manual(values=plcol,...) + scale_fill_manual(values=plcol,...)
  return(fig)
}

