#' @title Calculates species richness (\eqn{S_y})
#' @description This function takes a dataframe with columns **** and calculates
#'   species richness.
#' @details Species richness (\eqn{S_y}) is the count of the number of species
#'   recorded in all trawl catches collected year \eqn{y}.
#'
#'   Recommended data: Fishery independent surveys, fish and invertebrates
#'
#' @param X is probably a dataframe with certain columns.
#' @param group is where you select which groups to include
#' @param grps not sure what this is because it is NOT called inside the
#'   function
#' @family biodiversity indicators
#' @references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   Greenstreet SP, Fraser HM, Rogers SI, Trenkel VM, Simpson SD, Pinnegar JK
#'   (2012) Redundancy in metrics describing the composition, structure, and
#'   functioning of the North Sea demersal fish community. ICES J Mar Sci
#'   69:8–22
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export

speciesrichness <- function(X, group = c('FINFISH','ALL'), years = c(start.year:end.year))  {

  # this could change depending on how we ask for the data
	if(group=='FINFISH') X <- X[X$SPECIES < 1000,]  # subset data to include only the species of interest
		
  ind = vector(length = length(years))            # inititalize vector to store indicator values
  
  for (i in 1:length(years)){                     # loop over each year
    
    year.i = years[i]                             # set years.i to current year  
    X.i = X[X$YEAR == year.i, ]                   # subset data to include only current year
	 	
	 	ind[i] <- length(unique(X.i$SPECIES))         # count the number of species recorded and store value
		}
	
  ind                                             # return vector of indicator values for years c(start.year:end.year) 
  
	}
