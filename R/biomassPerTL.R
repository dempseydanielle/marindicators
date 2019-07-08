#' @title Calculates the biomass or abundance per discrete trophic level
#' @description This function takes a dataframe with columns **** and calculates
#'   the biomass or abundance per discrete trophic level.
#' @details **Recommended data: Fishery independent surveys, fish and
#'   invertebrates.
#' @param X add text here
#' @param table.of.trophic.level.data add text here
#' @param metric add text here
#' @param TL.grouping add text here
#' @param path add text here
#' @importFrom stats aggregate
#' @family stability and resistance indicators
#' @references Coll M, Shannon LJ, Moloney CL, Palomera I, Tudela S,
#'   2006. Comparing trophic flows and fishing impacts of a NW Mediterranean
#'   ecosystem with coastal upwellings by means of standardized ecological
#'   models and indicators. Ecol. Model. 198, 53-70. (not in references!)
#'
#'   Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the selection and
#'   evaluation of ecological indicators. Can. Tech. Rep. Fish. Aquat. Sci.
#'   3232: xii + 212 p.
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#' @export


# DD - July 8, 2019
# Could have start.year and end.year OR just the number of years
# i.e., need to decide if user will subset to the years they want, or if we will
# could also just look up the number of uniquw years inside the function: n.years = length(unique(X$years))
# and then loop from i = 1 to n.years

## hmmm but maybe better to loop from i = start.year to end.year
## this way I can subset to the current year over each iteration
## I think this will depend on what the input data looks like

## or can i do some sort of plyr thing

biomassPerTL <- function(X, table.of.trophic.level.data='INDISEAS_WSS_TL',
                         metric=c('BIOMASS','ABUNDANCE'), TL.grouping = 1, start.year, end.year) {
 
  breaks <- seq(1, 10, by = TL.grouping) # makes a vector from 1 to 10 by 1
  TLS['TL'] <- breaks[findInterval(TLS[,'TL'],breaks)]  # not sure about this one; Actually 
  # maybe it creates a column "Trophic Level" in the TLS dataframe to use with the aggregate function
  
  X <- merge(X, TLS, by='SPECIES') # think this merges the species data with the TL data		
  ind = data.frame() # initialize dataframe to store annual values
  
  # OPTION 1
  # Calculate the [metric] per trophic level for each year
  for (i in start.year:end.year){
  	
    X.year = X[X$year = i,]
    ind[i] <- stats::aggregate(X.year[metric], by= X.year['TL'], FUN=sum) # still not sure how 
    # aggregate is different than ddply
  }
  
  # OPTION 2
  # OR can I just write: (or ddply)
  ind <- stats::aggregate(X.year[metric], by= X.year[c('year', 'TL')], FUN=sum)
		   
		#   out <- as.data.frame(do.call(rbind,mmL)) # what does this do? Important??
    # maybe it reattaches it to the years
    ind		
}



