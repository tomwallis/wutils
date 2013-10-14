\name{multiplot}
\alias{multiplot}
\title{Multiplot function from R cookbook.}
\usage{
  multiplot(..., plotlist = NULL, cols)
}
\arguments{
  \item{...}{the name of each ggplot2 object, separated by
  a comma.}

  \item{cols}{the number of columns in which to arrange the
  plots.}
}
\description{
  This function allows the printing of multiple ggplot2
  objects into one object. Can be encapsulated into a
  pdf()... dev.off() call to print to pdf.
}
\examples{
library(ggplot2)
p1 <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet, group=Chick)) + geom_line() + ggtitle("Growth curve for individual chicks")
p2 <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet)) + geom_point(alpha=.3) + geom_smooth(alpha=.2, size=1) + ggtitle("Fitted growth curve per diet")
p3 <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, colour=Diet)) + geom_density() + ggtitle("Final weight, by diet")
p4 <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, fill=Diet)) + geom_histogram(colour="black", binwidth=50) + facet_grid(Diet ~ .) + ggtitle("Final weight, by diet") + theme(legend.position="none")        # No legend (redundant in this graph)
multiplot(p1, p2, p3, p4, cols=2)

\dontrun{
pdf('chickens.pdf',width=6,height=6)
multiplot(p1,p2,p3,p4,cols=2)
dev.off()}
}
\author{
  Winston Chang (R cookbook)
}
\references{
  \url{http://wiki.stdout.org/rcookbook/Graphs/Multiple%20graphs%20on%20one%20page%20(ggplot2)/#'}
}

