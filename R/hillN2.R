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
hillN2 <- function(X, group = c('FINFISH','ALL'), metric = c('BIOMASS','ABUNDANCE'),
                   years = c(start.year:end.year))  {

  # subset to group and metric?
	if(group=='FINFISH')	X <- X[X$SPECIES<1000,]
	
	zero_index = which(X[,metric] == 0)             # index of where metric observations are zero
	if(length(zero_index) > 0){                     # message showing number of observations removed
	  X = X[-zero_index, ]                            # remove the rows where metric is zero 
	  print(paste(length(zero_index), "observations of zero removed from metric"))
	}
	#X <- X[X[metric]>0,]                           # another way to remove the zeros (but doesn't count how many)
	
	ind = vector(length = length(years))            # inititalize vector to store indicator values
	
	for (i in 1:length(years)){                     # loop over each year
	  
	  year.i = years[i]                             # set years.i to current year  
	  X.i = X[X$YEAR == year.i, metric]             # subset data to include only current year
	  
	  X.i = X.i[order(X.i)]                         # order from smallest to largest (not required)
	  p <- X.i/sum(X.i)                             # calculate proportion of each species by metric
	  ind[i] <- 1/sum(p^2)                          # calculate Hill's species dominance
	}
	
	ind                                             # return Hill's species dominance
}



