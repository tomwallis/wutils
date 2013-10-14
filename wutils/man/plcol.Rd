\name{plcol}
\alias{plcol}
\title{Add Tom's favourite plot colours to a ggplot object.}
\usage{
  plcol(fig, palette = "cb", ...)
}
\arguments{
  \item{fig}{a ggplot2 object.}

  \item{palette}{a string argument for which colour palette
  to return.}

  \item{...}{further arguments passed to
  scale_colour_manual and scale_fill_manual (e.g. could
  pass name='Scale name').}
}
\value{
  returns a ggplot2 object with adjusted manual colour
  scale and fill scale.
}
\description{
  This function accepts a ggplot object and returns the
  object with the colour and fill scales adjusted to the
  specified colour scheme.
}
\details{
  Possible arguments: \itemize{ \item \code{'pleasing'}:
  return hand-tuned pleasing colours. \item \code{'cb'}:
  return colourblind friendly palette. \item
  \code{'greyscale'}: return greyscale palette. }
}
\examples{
library(ggplot2)
data(diamonds)
diamonds <- diamonds[sample(nrow(diamonds),size=2000),]
diamonds <- subset(diamonds,cut==c('Good','Very Good','Premium','Ideal'))
fig <- ggplot(diamonds,aes(x=carat,y=price,colour=cut)) + geom_point()
fig <- plcol(fig,palette='pleasing')
fig

# change to greyscale:
fig <- plcol(fig,palette='greyscale')
fig
}
\author{
  Thomas Wallis (with colours collected from around the
  web)
}

