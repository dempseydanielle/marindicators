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
#'@param X A dataframe of fishery independent data derived from research vessel
#'  survey data or model output, OR commercial landings data. Fishery
#'  independent survey data has columns \code{YEAR}, \code{ID}, \code{SPECIES},
#'  and \code{ABUNDANCE} and/or \code{BIOMASS}. \code{YEAR} indicates the year
#'  the observation was recorded, \code{ID} is an area code indicating where the
#'  observation was recorded, \code{SPECIES} is a numeric code indicating the
#'  species sampled, and \code{ABUNDANCE}/\code{BIOMASS} is the corresponding
#'  abundance/biomass (stratified and corrected for catchability as required).
#'
#'  Similarly, commercial landings data should have columns \code{YEAR},
#'  \code{ID}, \code{SPECIES} are as above, and \code{CATCH} is the
#'  corresponding landed weight.
#'@return Returns a dataframe with columns \code{ID} and \code{YEAR}, and if
#'  \code{metric = "ABUNDANCE"}, a column \code{SpeciesRichness_group} for each
#'  entry in \code{groups} OR if \code{metric = "CATCH"}, a column
#'  \code{DiversityTargetSpp_group} for each entry in \code{groups}.
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
#'@author  Danielle Dempsey \email{Danielle.Dempsey@@dfo-mpo.gc.ca}, Adam Cook,
#'  Catalina Gomez, Alida Bundy
#'@importFrom stats na.omit
#'@examples
#'# Calculate species richness (community)
#'data(X)
#'speciesRichness(X, groups = "ALL", metric = "BIOMASS", years = c(2014:2019))
#'
#'# Calculate diversity of target species
#'data(land)
#'speciesRichness(land, groups = "ALL", metric = "CATCH",  years = c(2014:2019))
#'@export


speciesRichness <- function(X, groups, species.table = NULL, metric, years)  {

  for(k in 1:length(groups)){          # loop over species groups
    
  X.k <- speciesGroups(X = X, group = groups[k], species.table = species.table) # subset X to the species of interest
	
	uI = unique(X$ID)                   # extract the spatial scale ID's
	ind.k <- NULL                       # initialize dataframe for storing indicator values
	
	for (j in 1:length(uI)){            # loop over all spatal scales
	  
	  X.j = X.k[X.k$ID == uI[j], ]          # subset data to spatial scale j
	  
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
	    ind.k = rbind(ind.k, ind.i)                       # bind ind.i to ind dataframe
	  }
	}

	if(metric == "BIOMASS" || metric == "ABUNDANCE") ind.name =  paste("SpeciesRichness_", groups[k], sep = "")   # name the ind 
	if(metric == "CATCH") ind.name = paste("DiversityTargetSpp_", groups[k], sep = "")                            # name the ind 
	
	names(ind.k) = c("ID", "YEAR", ind.name)                                 # name the ind dataframe
	ind.k <- ind.k[order(ind.k$ID), ] 
	
	if(k == 1) ind = ind.k
	
	ind <- merge(ind, ind.k)                                                                     # return vector of indicator values for years c(start.year:end.year) 
	
  }
  ind  
}
