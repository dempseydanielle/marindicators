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

speciesrichness <- function(X, group=c('FINFISH','ALL'), grps, start.year, end.year)  {

  # this could change depending on how we ask for the data
	if(group=='FINFISH') {
		X <- X[X$SPECIES<1000,]
		}
	
  years = c(start_year:end_year)
  ind = data.frame(NULL)
  
  for (i in 1:length(years)){
    
    year.i = years[i]
    X.i = X[X$years == year.i, ]
	 	
	 	ind[i] <- length(unique(X.i$SPECIES))
		}
	
  ind
  
	}
