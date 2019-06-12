#' Install botometer
#' 
#' Install the Python module botometer
#' 
#' This function is a wrapper for calling \code{py_install()} from 
#' the \code{reticulate} pacakge to install the Python module 
#' \code{botometer}.  
#' 
#' @author Ryan T. Moore
#' 
#' @param method A string, defaults to \code{"auto"}
#' @param conda A string, defaults to \code{"auto"}
#' 
#' @references The source of this code is 
#' \url{https://github.com/rstudio/reticulate/blob/master/vignettes/package.Rmd}.
#'
#' @export 

install_botometer <- function(method = "auto", conda = "auto") {
  reticulate::py_install("botometer", method = method, conda = conda)
}
