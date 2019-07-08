#' @title Calculates Heip's Evenness Index
#' @description This function takes a dataframe with columns **** and calculates
#'   Heip's Evenness Index (HE) of the community
#' @details Heip's Evenness Index (HE): \deqn{HE = (exp^{H'} - 1)/(S - 1)} where
#'   \eqn{H'} is the Shannon Index of Diversity and \eqn{S} is species richness.
#'   This index ranges from 0 to 1 and measures how equally the species richness
#'   contributes to the total abundance or biomass of the community.
#'
#'   **Recommended data: Fishery independent surveys, fish and invertebrates
#'   (NOT in tech report).
#' @param X add text here
#' @param group add text here
#' @param metric add text here
#' @family biodiversity indicators
#' @references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   Kenchington T, Kenchington E (2013) Biodiversity Metrics for Use in the
#'   Ecosystem Approach to Oceans Management. Can. Tech. Rep. Fish. Aquat. Sci.
#'   3059: vi+188p.

#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#' @export


# would it be better to just use the Shannon and Species functions??
heips <- function(X, group = c('FINFISH','ALL'), metric = c('BIOMASS','ABUNDANCE'))  {
  
  # subset to group and metric?
  if(group=='FINFISH') {
    X <- X[X$SPECIES<1000,]
  }
  
  H = shannon(X, group = group, metric = metric)
  S = speciesrichness(X,  group = group, grps) # not sure what grps should be
  
  ind = (exp(H)-1)/(S-1)

  ind
}
  # could put a check in here to make sure that it is the same as below
  
	# uI <- unique(X$ID)
	# heip.est <- numeric()
	#  for(i in 1:length(uI)) {
	#  	Y <- X[X$ID==uI[i],]
	#  	Y <- Y[order(Y[metric]),metric]   
	#  	Y <- Y/sum(Y)
	#  	heip.est[i] <- (exp(-sum(Y*log(Y)))-1)/(length(Y)-1)
	# 	}
	# out <- as.data.frame(cbind(uI,heip.est))
	# names(out)[1] <-'ID'
	# out[,2] <- as.numeric(out[,2])
	# return(out)
	
