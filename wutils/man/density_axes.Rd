\name{density_axes}
\alias{density_axes}
\title{make a ggplot2 figure with density axes (Tufte).}
\usage{
  density_axes(fig, base_size = 12, plot_line = TRUE)
}
\arguments{
  \item{fig}{a ggplot2 object.}

  \item{base_size}{the base text size of the plain theme.}

  \item{plot_line}{if false, don't plot axis lines (density
  plots of data alone)}
}
\value{
  returns a ggplot2 object.
}
\description{
  This function accepts a ggplot object and returns the
  same object with the axes adjusted to be minimal density
  values of the data points. Assumes that there is x and y
  data in the plot.
}
\examples{
library(ggplot2)
data(diamonds)
diamonds <- diamonds[sample(nrow(diamonds),size=2000),]
diamonds <- subset(diamonds,cut==c('Good','Very Good','Premium','Ideal'))
fig <- ggplot(diamonds,aes(x=carat,y=price,colour=cut,fill=cut,shape=cut)) + geom_point(size=3)
fig <- greyfig(fig)
fig <- density_axes(fig,plot_line=FALSE)
fig
}
\author{
  Thomas Wallis (with colours collected from around the
  web)
}

