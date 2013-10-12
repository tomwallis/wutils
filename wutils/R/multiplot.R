#' Multiplot function from R cookbook.
#' 
#' This function allows the printing of multiple ggplot2 objects into one object. 
#' Can be encapsulated into a pdf()... dev.off() call to print to pdf.
#' 
#' @export  
#' 
#' @param ... the name of each ggplot2 object, separated by a comma.
#' @param cols the number of columns in which to arrange the plots. 
#' @author Winston Chang (R cookbook)
#' @references \url{http://wiki.stdout.org/rcookbook/Graphs/Multiple%20graphs%20on%20one%20page%20(ggplot2)/#'} 
#' @examples
#' p1 <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet, group=Chick)) + geom_line() + ggtitle("Growth curve for individual chicks")
#' p2 <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet)) + geom_point(alpha=.3) + geom_smooth(alpha=.2, size=1) + ggtitle("Fitted growth curve per diet")
#' p3 <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, colour=Diet)) + geom_density() + ggtitle("Final weight, by diet")
#' p4 <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, fill=Diet)) + geom_histogram(colour="black", binwidth=50) + facet_grid(Diet ~ .) + ggtitle("Final weight, by diet") + theme(legend.position="none")        # No legend (redundant in this graph)    
#' multiplot(p1, p2, p3, p4, cols=2)
#' 
#' \dontrun{
#' pdf('chickens.pdf',width=6,height=6)
#' multiplot(p1,p2,p3,p4,cols=2)
#' dev.off()}


multiplot <- function(..., plotlist=NULL, cols) {
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
}
