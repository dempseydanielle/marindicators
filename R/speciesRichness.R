#'@title Calculates species richness of the community or the diversity of target
#'  species
#'@description This function counts the number of species recorded in fishery
#'  independent survey data or commercial landings data for \eqn{i} years and
#'  \eqn{j} areas.
#'@details Species richness (\eqn{S_y}) is the count of the number of species
#'  recorded in all research vessel trawl surveys collected year \eqn{y}.
#'
#'  Recommended data: Fishery independent surveys or model output; fish and
#'  invertebrates
#'
#'  The diversity of the target species for year y (\eqn{TS_y}) is the count of
#'  the number of target species recorded in all trawl catches collected in that
#'  year.
#'
#'  Recommended data: commercial fisheries landings; fish and invertebrates.
#'@inheritParams shannon
#'@param X A dataframe of fishery independent survey data OR commercial landings
#'  data. Fishery independent suvey data has columns "YEAR", "ID", "SPECIES",
#'  and "ABUNDANCE" and/or "BIOMASS". "YEAR" indicates the year the observation
#'  was recorded, "ID" is an area code indicating where the observation was
#'  recorded, "SPECIES" is a numeric code indicating the species sampled, and
#'  "ABUNDANCE"/"BIOMASS" is the corresponding abundance/biomass (stratified and
#'  corrected for catchability as required).
#'
#'  Similarly,  commercial landings data will have columns "YEAR", "ID",
#'  "SPECIES" and "CATCH". "YEAR" indicates the year the landing was recorded,
#'  "ID" is an area code indicating where the landing was recorded, "SPECIES" is
#'  a numeric code indicating the species landed, and "CATCH" is the
#'  corresponding landed weight.
#'@return Returns a dataframe with 3 columns. If metric = "BIOMASS" or metric =
#'  "ABUNDANCE", columns will be named "ID", "YEAR", "SpeciesRichness". If metrc
#'  = "CATCH", columns will be named  "ID", "YEAR", "DiversityTargetSpp".
#'
#'  If there is no data for spatial scale \eqn{j} in year \eqn{i}, indicator
#'  values is assigned NA.
#'@family biodiversity indicators
#'@family fishing pressure indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
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
