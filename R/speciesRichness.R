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

# right now this depends on metric, but seems like it shouldn't!
speciesrichness <- function(X, group = c('FINFISH','ALL'), metric = c('BIOMASS','ABUNDANCE', "CATCH"),
                            years = c(start.year:end.year))  {

  # this could change depending on how we ask for the data
  # can I use resource potential for this>?
	if((metric == "BIOMASS" || metric == "ABUNDANCE") & group =='FINFISH') X <- X[X$SPECIES < 1000,]  # subset data to include only the species of interest
	if(metric == "CATCH" & group !='ALL') land <- land[which(land[group] == 1),]      # subset to species in "GROUP"
	
	uI = unique(X$ID)                   # extract the spatial scale ID's
	ind <- NULL                         # initialize dataframe for storing indicator values
	
	for (j in 1:length(uI)){            # loop over all spatal scales
	  
	  X.j = X[X$ID == uI[j], ]          # subset data to spatial scale j
	  
	  for (i in 1:length(years)){                     # loop over each year
	    
	    year.i = years[i]                             # set years.i to current year  
	    X.ij = X.j[X.j$YEAR == year.i, ]              # subset data to include only current year
	 	
	    ind.i <- length(unique(X.ij$SPECIES))         # count the number of species recorded and store value
	    ind.i = data.frame(uI[j], year.i, ind.i)     # create a dataframe with spatial scale ID, year, and indicator value
	    ind = rbind(ind, ind.i)                      # bind ind.i to ind dataframe
	 	}
	}
	
	if(metric == "BIOMASS" || metric == "ABUNDANCE") names(ind) = c("ID", "YEAR", "SpeciesRichness")    # name the ind dataframe
	if(metric == "CATCH") names(ind) = c("ID", "YEAR", "DiversityTargetSpp")    # name the ind dataframe
	ind                                             # return vector of indicator values for years c(start.year:end.year) 
	
}
