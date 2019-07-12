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
 

hillN2 <- function(X, group = c('FINFISH','ALL'), metric = c('BIOMASS','ABUNDANCE'),
                   years = c(start.year:end.year))  {

  # subset to group and metric?
	if(group=='FINFISH')	X <- X[X$SPECIES<1000,]
	uI = unique(X$ID)                   # extract the spatial scale ID's
	ind <- NULL                         # initialize dataframe for storing indicator values
	
	for (j in 1:length(uI)){            # loop over all spatal scales
	  
	  X.j = X[X$ID == uI[j], ]          # subset data to spatial scale j
	  
	  for (i in 1:length(years)){                     # loop over each year
	    
	    year.i = years[i]                             # set years.i to current year  
	    X.ij = X.j[X.j$YEAR == year.i, metric]             # subset data to include only current year
	    
	    X.ij = X.ij[order(X.ij)]                         # order from smallest to largest (not required)
	    p <- X.ij/sum(X.ij)                             # calculate proportion of each species by metric
	    ind.i <- 1/sum(p^2)                          # calculate Hill's species dominance
	    
	    ind.i = data.frame(uI[j], year.i, ind.i)     # create a dataframe with spatial scale ID, year, and indicator value
	    ind = rbind(ind, ind.i)                      # bind ind.i to ind dataframe
	  }
	}
	  names(ind) = c("ID", "YEAR", "HillDominance")    # name the ind dataframe
	  ind                                              # return Hill's species dominance
}



