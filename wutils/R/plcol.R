#' Generate Tom's favourite plot colours.
#' 
#' This function can be called with no inputs to generate Tom's commonly used colour palettes for plotting.
#' These are returned as character arrays in the workspace for use in adding scales.
#' @export  
#' 
#' @details
#' Possible arguments: 
#' \itemize{
#' \item \code{'pleasing'}: return hand-tuned pleasing colours.
#' \item \code{'cbPalette'}: return colourblind friendly palette with grey.
#' \item \code{'cbbPalette'}: return colourblind friendly palette with black.
#' \item \code{'greyscale'}: return greyscale palette.
#' }
#' 
#' @return A character vector that can be passed to ggplot2's manual colour scales.
#' 
#' @param palette a string argument for which colour palette to return.
#' @author Thomas Wallis (with colours collected from around the web)
#' @examples
#' data(diamonds)
#' diamonds <- diamonds[sample(nrow(diamonds),size=2000),]
#' fig <- ggplot(diamonds,aes(x=carat,y=price,colour=cut)) + geom_point()
#' my_colours <- plcol(palette='cbbPalette')
#' fig <- fig + scale_colour_manual(values=my_colours)
#' fig
#' 
#' # change to greyscale:
#' my_colours <- plcol(palette='greyscale')
#' fig <- fig + scale_colour_manual(values=my_colours)
#' fig

plcol <- function(palette='cbPalette'){
  # hand-tuned pleasing colours
  if(palette == 'pleasing') return(c("#3C4884","#DD5525","#8C2212","#E18942","#425227","#94117E"))
  # Colourblind-friendly palette with grey:
  if(palette == 'cbPalette') return(c("#999999", "#E69F00", "#56B4E9", "#009E73","#0072B2", "#D55E00", "#CC79A7","#F0E442"))
  # Colourblind-friendly palette with black:
  if(palette == 'cbbPalette') return(c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7","#F0E442"))
  # greyscale palette:
  if(palette == 'greyscale') return(c('#222222','#555555','#777777','#999999','#bbbbbb','#dddddd'))  
}

