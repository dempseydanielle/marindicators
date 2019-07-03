#' @title Calculates Hill's index of diversity
#' @description This function takes a dataframe with columns **** and calculates
#'   Hill's diversity index.
#' @details Hill's N1 diversity index is the exponential of the Shannon-Weiner
#'   index.\deqn{HillN1 = e^(-\Sigma(p_i ln( p_i ))} \eqn{p_i} is the proportion
#'   of the total sample contributed by the i(th) species and S is the number of
#'   species recorded in the sample. This index is sensitive to the number of
#'   species recorded in the sample. 
#'
#'   **Recommended data: Fishery independent surveys, fish and invertebrates.
#'
#' @param X is probably a dataframe with certain columns.
#' @param group is where you select which groups to include
#' @param metric is where you choose if you want to calculate using biomass or
#'   abundance
#' @family biodiversity indicators
#' @references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   Greenstreet SP, Rogers SI (2006) Indicators of the health of the North Sea
#'   fish community: identifying reference levels for an ecosystem approach to
#'   management. ICES J Mar Sci J du Cons 63:573–593
#'
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export


hillN1 <- function(X, group=c('FINFISH','ALL'), metric=c('BIOMASS','ABUNDANCE')) {
	out <- shannon(X = X, group = group, metric = metric) # calculate Shannon's index of diversity
	out[,2] <- exp(out[,2])                               # Exponential of Shannon's index
	out                                                   # Return Hill's index of diversity
#	return(out)
	}


#devtools::document()