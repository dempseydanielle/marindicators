#' @title Calculates Hill's Species Dominance
#' @description This function takes a dataframe with columns **** and calculates
#'   Hill's Species Dominance (HillN2)
#' @details  Hill's Species Dominance (HillN2): \deqn{HillN2 = 1/\Sigma p_i^2}
#'   \eqn{p_i} is the proportion of the total sample contributed by the i(th)
#'   species. HillN2 is the inverse of the Simpson's index. This index is
#'   sensitive to the evenness of the distribution of individuals between
#'   species.
#'
#'   Recommended data: Fishery independent surveys, fish and invertebrates

#' @param X add text here
#' @param group add text here
#' @param metric add text here
#' @family biodiversity indicators
#' @references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   Greenstreet SP, Rogers SI (2006) Indicators of the health of the North Sea
#'   fish community: identifying reference levels for an ecosystem approach to
#'   management. ICES J Mar Sci J du Cons 63:573–593

#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#' @export
 

# loop over all years?
hillN2 <- function(X, group = c('FINFISH','ALL'), metric = c('BIOMASS','ABUNDANCE'))  {

  # subset to group and metric?
	if(group=='FINFISH') {
		X <- X[X$SPECIES<1000,]
		}
  
  Y <- Y[order(Y[metric]), metric]   # why order?
	p <- Y/sum(Y)
	ind <- 1/sum(p^2)
	
	ind
	}