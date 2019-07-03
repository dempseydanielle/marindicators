#' @title test2
#' @description This function is a test.
#' @param p default is \code{NULL}. This is a parameter.
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom lubridate year
#' @export
test2 <- function(p=Sys.Date()){
 cat(paste("hi, it is now",lubridate::year(p)))
}