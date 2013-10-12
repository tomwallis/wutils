\name{plcol}
\alias{plcol}
\title{Generate Tom's favourite plot colours.}
\usage{
  plcol(palette = "cbPalette")
}
\arguments{
  \item{palette}{a string argument for which colour palette
  to return.}
}
\value{
  A character vector that can be passed to ggplot2's manual
  colour scales.
}
\description{
  This function can be called with no inputs to generate
  Tom's commonly used colour palettes for plotting. These
  are returned as character arrays in the workspace for use
  in adding scales.
}
\details{
  Possible arguments: \itemize{ \item \code{'pleasing'}:
  return hand-tuned pleasing colours. \item
  \code{'cbPalette'}: return colourblind friendly palette
  with grey. \item \code{'cbbPalette'}: return colourblind
  friendly palette with black. \item \code{'greyscale'}:
  return greyscale palette. }
}
\examples{
data(diamonds)
diamonds <- diamonds[sample(nrow(diamonds),size=2000),]
fig <- ggplot(diamonds,aes(x=carat,y=price,colour=cut)) + geom_point()
my_colours <- plcol(palette='cbbPalette')
fig <- fig + scale_colour_manual(values=my_colours)
fig

# change to greyscale:
my_colours <- plcol(palette='greyscale')
fig <- fig + scale_colour_manual(values=my_colours)
fig
}
\author{
  Thomas Wallis (with colours collected from around the
  web)
}

