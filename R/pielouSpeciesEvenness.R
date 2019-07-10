#' @title Calculates Pielou's Species Evenness
#' @description This function takes a dataframe with columns **** and calculates
#'   Pielou's Species Evenness.
#' @details Pielou's Species Evenness: \deqn{J' = -\Sigma p_i ln( p_i )/ln(S)}
#'   \eqn{p_i} is the proportion of the total sample contributed by the i(th)
#'   species and \eqn{S} is the number of species recorded in the sample.
#'   Pielou's index is the Shannon-Weiner index computed for the sample \eqn{S}
#'   and represents a measure of evenness of the community.
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
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export

# add a warning for if there is 0 biomass
pielouSpeciesEvenness <- function(X, group = c('FINFISH','ALL'), metric = c('BIOMASS','ABUNDANCE'),
                                  years = c(start.year:end.year))  {

  #source("R/speciesrichness.R")
  #S <- speciesrichness(X, group = group, years = years) # See note nelw
  
  # Could possibly also use the shannon() function here
  
	if(group=='FINFISH') X <- X[X$SPECIES<1000,]
  
  # Remove observations of "0" or will return NaN in -sum(p*log(p))
  zero_index = which(X[,metric] == 0)             # index of where metric observations are zero
  X = X[-zero_index, ]                            # remove the rows where metric is zero 
  if(length(zero_index) > 0){                     # message showing number of observations removed
    print(paste(length(zero_index), "observations of zero removed from metric")) 
  }
  
	ind = vector(length = length(years))            # inititalize vector to store indicator values
	
	for (i in 1:length(years)){                     # loop over all years
  
    year.i = years[i]                             # set years.i to current year  
    
    X.i = X[X$YEAR == year.i, metric]             # subset data to include only current year
    p <- X.i/sum(X.i)                             # the proportion of each species by metric
    ind[i] <- -sum(p*log(p))/log(length(X.i))     # Pielou's species evenness

    #ind[i] <- -sum(p*log(p))/log(S[i])           # S[i] should be the same as length(X.i)
    # BUT the speciesrichness() function doesn't remove zeros, so they are a bit different
     }
  
  ind
}
