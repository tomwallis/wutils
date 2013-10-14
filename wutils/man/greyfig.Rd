\name{greyfig}
\alias{grayfig}
\alias{greyfig}
\title{make a ggplot2 figure into a discernable greyscale plot.}
\usage{
  greyfig(fig, ...)
}
\arguments{
  \item{fig}{a ggplot2 object.}

  \item{...}{further arguments passed to
  scale_colour_manual and scale_fill_manual (e.g. could
  pass name='Scale name').}
}
\value{
  returns a ggplot2 object with adjusted manual colour
  scale, fill scale and linetypes.
}
\description{
  This function accepts a ggplot object and returns the
  same object with colour, fill and shape scales adjusted
  to produce up to four discernable greyscale discrete data
  sets.
}
\examples{
library(ggplot2)
data(diamonds)
diamonds <- diamonds[sample(nrow(diamonds),size=2000),]
diamonds <- subset(diamonds,cut==c('Good','Very Good','Premium','Ideal'))
fig <- ggplot(diamonds,aes(x=carat,y=price,colour=cut,fill=cut,shape=cut)) + geom_point()
fig <- greyfig(fig)
fig
}
\author{
  Thomas Wallis (with colours collected from around the
  web)
}

