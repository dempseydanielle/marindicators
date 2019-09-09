#'@title Calculates Species Richness of the community or the Diversity of Target
#'  Species
#'@description This function counts the number of species recorded in fishery
#'  independent survey data or commercial landings data for \eqn{i} years and
#'  \eqn{j} areas.
#'@details Two useful species richness indicators are: "Species Richness" (S) of
#'  the surveyed community in an area and "Diversity of the Target Species" (TS)
#'  in the commercial fishery, which is a measure of the distribution of fishing
#'  pressure.
#'
#'  Species richness (\eqn{S_y}) is the count of the number of species recorded
#'  in all research vessel trawl surveys collected in year \eqn{y} for a given
#'  area (Hurlbert, 1971).
#'
#'  The diversity of the target species for year y (\eqn{TS_y}) is the count of
#'  the number of target species recorded in all trawl catches collected in that
#'  year for a given area.
#'@inheritParams shannon
#'@param X A dataframe of fishery independent survey data or model output OR
#'  commercial landings data. Fishery independent suvey data has columns
#'  \code{YEAR}, \code{ID}, \code{SPECIES}, and \code{ABUNDANCE} and/or
#'  \code{BIOMASS}. \code{YEAR} indicates the year the observation was recorded,
#'  \code{ID} is an area code indicating where the observation was recorded,
#'  \code{SPECIES} is a numeric code indicating the species sampled, and
#'  \code{ABUNDANCE}/\code{BIOMASS} is the corresponding abundance/biomass
#'  (stratified and corrected for catchability as required).
#'
#'  Similarly, commercial landings data should have columns \code{YEAR},
#'  \code{ID}, \code{SPECIES} are as above, and \code{CATCH} is the
#'  corresponding landed weight.
#'@return Returns a dataframe with 3 columns. If \code{metric = "BIOMASS"} or
#'  \code{metric = "ABUNDANCE"}, columns will be named \code{ID}, \code{YEAR},
#'  \code{SpeciesRichness}. If \code{metric = "CATCH"}, columns will be named
#'  \code{ID}, \code{YEAR}, \code{DiversityTargetSpp}.
#'
#'  If there is no data for spatial scale \eqn{j} in year \eqn{i}, indicator
#'  values is assigned \code{NA}.
#'@family biodiversity indicators
#'@family fishing pressure indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Hurlbert SH. 1971. The non-concept of species diversity: a critique and
#'  alternative parameters. Ecology, 52, 577-86.
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@importFrom stats na.omit
#'@export


speciesRichness <- function(X, group, species.table = NULL, metric, years)  {

  X <- speciesGroups(X = X, group = group, species.table = species.table) # subset X to the species of interest
	
	uI = unique(X$ID)                   # extract the spatial scale ID's
	ind <- NULL                         # initialize dataframe for storing indicator values
	
	for (j in 1:length(uI)){            # loop over all spatal scales
	  
	  X.j = X[X$ID == uI[j], ]          # subset data to spatial scale j
	  
	  for (i in 1:length(years)){                     # loop over each year
	    
	    year.i = years[i]                             # set years.i to current year  
	    X.ij = X.j[X.j$YEAR == year.i, ]              # subset data to include only current year
	    
	   if(nrow(X.ij) > 0){ 
	     inx <- which(X.ij[metric] == 0)
	     X.ij[inx, metric] <- NA                   # remove entries with BIOMASS/ABUNDANCE or CATCH = 0
	     X.ij <- na.omit(X.ij)
	     ind.i <- length(unique(X.ij$SPECIES))         # count the number of species recorded and store value
	   } else ind.i <- NA
	    
	    ind.i = data.frame(uI[j], year.i, ind.i)      # create a dataframe with spatial scale ID, year, and indicator value
	    ind = rbind(ind, ind.i)                       # bind ind.i to ind dataframe
	 	}
	}
	
	if(metric == "BIOMASS" || metric == "ABUNDANCE") names(ind) = c("ID", "YEAR", "SpeciesRichness")    # name the ind dataframe
	if(metric == "CATCH") names(ind) = c("ID", "YEAR", "DiversityTargetSpp")    # name the ind dataframe
	ind <- ind[order(ind$ID), ] 
	ind                                                                         # return vector of indicator values for years c(start.year:end.year) 
	
}
