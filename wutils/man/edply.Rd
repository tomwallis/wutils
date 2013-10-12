\name{edply}
\alias{edply}
\title{edply: combining plyr and expand.grid.}
\usage{
  edply(vars, fun, ...)
}
\arguments{
  \item{vars}{a list of named parameter values to test.}

  \item{fun}{a function that accepts the parameter values
  in vars and generates the quantity to be plotted.}

  \item{...}{other arguments to elply function.}
}
\description{
  This function allows the combination of plyr and
  expand.grid. This can be really handy when you want to,
  for example, examine the effect of various parameters on
  the shape of a function.
}
\details{
  Additional arguments include \code{.progress} which
  defaults to "none". This can accept a value from
  \code{\link{create_progress_bar}}. Second possible
  argument is \code{.parallel}, which defaults to
  \code{FALSE}. If set to \code{TRUE}, applies function in
  parallel using \code{\link{foreach}}.
}
\examples{
fun <- function(x,y) dnorm(x)*dnorm(y)*sin(x)
d <- edply(list(x=seq(-3,3,l=40),y=seq(-3,3,l=40)),fun)
colnames(d)[3] <- 'fxy'
ggplot(d,aes(x,y))+geom_raster(aes(fill=fxy)) #Heatmap of f(x,y))
}
\author{
  Simon Barthelmé, University of Geneva.

  Tom Wallis coded into package form.
}

