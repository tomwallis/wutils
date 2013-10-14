#' edply: combining plyr and expand.grid.
#'
#' This function allows the combination of plyr and expand.grid. 
#' This can be really handy when you want to, for example, examine the effect of various parameters on the shape of a function.
#'
#' @export  
#' 
#' @param vars a list of named parameter values to test.
#' @param fun a function that accepts the parameter values in vars and generates the quantity to be plotted.
#' @param ... other arguments to elply function.
#' 
#' @details
#' Additional arguments: 
#' \itemize{
#' \item \code{.progress}, defaults to "none". This can accept a value from \code{\link{create_progress_bar}}.
#' \item \code{.parallel}, defaults to \code{FALSE}. If set to \code{TRUE}, applies function in parallel using \code{\link{foreach}}.
#' }
#' 
#' @return A data frame containing the parameter values and function output.
#'   
#' 
#' @author Simon Barthelmé, University of Geneva. 
#' @author Tom Wallis coded into package form.
#' 
#' @examples
#' fun <- function(x,y) dnorm(x)*dnorm(y)*sin(x)
#' d <- edply(list(x=seq(-3,3,l=40),y=seq(-3,3,l=40)),fun)
#' colnames(d)[3] <- 'fxy'
#' library(ggplot2)
#' ggplot(d,aes(x,y))+geom_raster(aes(fill=fxy)) #Heatmap of f(x,y))

edply <- function(vars, fun, ...)
{
  elply <- function(vars,fun,...,.progress="none",.parallel=FALSE)
  {
    df <- do.call("expand.grid",vars)
    if (all(names(vars) %in% names(formals(fun))))
    {
      #We assume that fun takes the variables in vars as named arguments
      funt <- function(v,...)
      {
        do.call(fun,c(v,list(...)))
      }
      res <- alply(df,1,funt,...,.progress=.progress,.parallel=.parallel)
    }
    else
    {
      #We assume that fun takes a named list as first argument
      res <- alply(df,1,fun,...,.progress=.progress,.parallel=.parallel)
    }
    res
  }
  
  res <- elply(vars, fun, ...)
  plyr:::list_to_dataframe(res,attr(res, "split_labels"))
}



