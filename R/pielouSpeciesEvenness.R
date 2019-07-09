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



pielouSpeciesEvenness <- function(X, group = c('FINFISH','ALL'), metric = c('BIOMASS','ABUNDANCE'),
                                  start.year, end.year)  {

  # not sure how we are expecting them to label their data
  # maybe put a note in that they they have to label their species this way?
  # but not really helpful for merging trophic level, etc
  # maybe they will have to  put in a dataframe that already includes TL
  # make that an option!
	if(group=='FINFISH') {
		X <- X[X$SPECIES<1000,]
	}
  
  years = c(start_year:end_year)
  ind = data.frame(NULL)
  
  for (i in 1:length(years)){
  
    year.i = years[i]
    
    X.i = X[X$years == year.i, ]
   
    X.i <- X.i[order(X.i[metric]), metric]   # WHY ORDER??????
    X.i <- X.i/sum(X.i) # does this make sense? How is X.i different than the sum(X.i)?
    ind[i] <- -sum(X.i*log(X.i))/log(length(X.i)) # could also use speciesrichness instead of length(X.i)
  }
  
  ind

}
  
  
  
  
  
  
  
