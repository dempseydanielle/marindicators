#'@title Calculates the proportion of predatory fish in the community (Shin et
#'  al., 2010)
#'@description This function calculates the proportion of predatory fish in the
#'  community for \eqn{j} areas and \eqn{i} years.
#'@details Proportion of predatory fish in the community is estimated by:
#'  \deqn{PropPred = Biomass Predatory Fish Surveyed/Total Biomass Surveyed}
#'  Predatory fish species are defined as all surveyed fish species that are not
#'  largely planktivorous (i.e. phytoplankton and zooplankton feeders should be
#'  excluded; Shin et al. 2010). A fish species is classified as predatory if it
#'  is piscivorous, or if it feeds on invertebrates that are larger than the
#'  macrozooplankton category (0.2 cm). Detritivores should not be classified as
#'  predatory fish.
#'
#'  This indicator captures changes in the trophic structure and changes in the
#'  functional diversity of fish in the ecosystem.
#'
#'  Recommended data: Fishery independent survey data or model output; fish and
#'  invertebrates.
#'@inheritParams biomassPerTL
#'@param pred.species A vector of species codes for predatory fish
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
#'  Shin, YJ, Shannon LJ, Bundy A, Coll M, Aydin K, Bez N, Blanchard JL, Borges,
#'  MF, Diallo I, Diaz E, Heymans JJ, Hill L, Johannesen E, Jouffre D, Kifani S,
#'  Labrosse P, Link JS, Mackinson S, Masski H, Möllmann C, Neira S, Ojaveer H,
#'  Ould Mohammed Abdallahi ., Perry I, Thiao D, Yemane D, and Cury PM. 2010.
#'  Using indicators for evaluating, comparing and communicating the ecological
#'  status of exploited marine ecosystems. Part 2: Setting the scene. ICES
#'  Journal of Marine Science, 67: 692-716
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@export


predatoryFish <- function(X, pred.species, metric = "BIOMASS",  years) {
  
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



		