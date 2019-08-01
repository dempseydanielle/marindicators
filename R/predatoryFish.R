#'@title Calculates the proportion of predatory fish in the community
#'@description This function calculates the proportion of predatory fish in the
#'  community for \eqn{j} areas and \eqn{i} years.
#'@details Proportion of predatory fish in the community is estimated by:
#'  \deqn{PropPred = Biomass Predatory Fish Surveyed/Total Biomass Surveyed}
#'  Predatory fish species are defined as all surveyed fish species that are not
#'  largely planktivorous (i.e. phytoplankton and zooplankton feeders should be
#'  excluded; Shin et al. 2010). A fish species is classified as predatory if it
#'  is piscivorous, or if it feeds on invertebrates that are larger than the
#'  macrozooplankton category (.2 cm). Detritivores should not be classified as
#'  predatory fish.
#'
#'  This indicator captures changes in the trophic structure and changes in the
#'  functional diversity of fish in the ecosystem. Data used: Fishery
#'  independent surveys, predatory fish.
#'
#'  Recommended data: Fishery independent surveys, fish and invertebrates.
#'@param X dataframe of fishery independent survey data with columns "YEAR",
#'  "ID", "SPECIES", and "BIOMASS" and/or "ABUNDANCE". "ID" is an area code
#'  designating where the observation was recorded. "SPECIES" is a numeric code
#'  indicating the species sampled.
#'@param pred.species vector of species codes for predatory fish
#'@param metric character string indicating whether to use "BIOMASS" or
#'  "ABUNDANCE" to calculate the indicator. Default is "BIOMASS".
#'@param years vector of years for which to calculate indicator
#'@return Returns a dataframe with 3 columns: "ID", YEAR", and
#'  "PropPredatoryFish".
#'
#'  If there is no data for spatial scale \eqn{j} in year \eqn{i}, indicator
#'  value is assigned NA.
#'@family ecosystem structure and function indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Shin YJ, Bundy A, Shannon LJ, Simier M, Coll M, Fulton EA, Link JS, Jouffre
#'  D, Ojaveer H, MacKinson S, Heymans JJ, Raid T (2010) Can simple be useful
#'  and reliable? Using ecological indicators to represent and compare the
#'  states of marine ecosystems. ICES J Mar Sci 67:717-731
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}
#'@export


predatoryFish <- function(X, pred.species, metric = 'BIOMASS',  years) {
  
  uI = unique(X$ID)                           # extract the spatial scale ID's
  ind <- NULL                                 # initialize dataframe for storing indicator values
		
		for (j in 1:length(uI)){                  # loop over all spatal scales
		  
		  X.j = X[X$ID == uI[j], ]                # subset data to spatial scale j
		  
		  for (i in 1:length(years)){             # loop over all years
		    
		    year.i = years[i]                     # set year.i to year i
		    X.ij = X.j[X.j$YEAR == year.i, ]      # subset data to year i
		    Xpred <- X.ij[X.ij$SPECIES %in% pred.species,]  # create a table with only predatory species
		    
		    if(nrow(X.ij) > 0 & nrow(Xpred) > 0){   # if there are no observations in X.ij or Xpred, ind.i is set is to NA
		      A.i <- sum(Xpred[metric])             # sum the metric of predatory species
		      B.i <- sum(X.ij[metric])              # sum the metric of all species
		      ind.i <- A.i/B.i                      # calculate the proportion of predatory species
		    } else ind.i <- NA
		    
		    ind.i = data.frame(uI[j], year.i, ind.i)    # create a dataframe with spatial scale ID, year, and indicator value
		    ind = rbind(ind, ind.i)                     # bind ind.i to ind dataframe
		  }
		}
		
		names(ind) = c("ID", "YEAR", "PropPredatoryFish")    # name the ind dataframe
		ind <- ind[order(ind$ID), ]                          # order by ID to be consistent with other functions
		ind                                                  # return ind
		
		}



		