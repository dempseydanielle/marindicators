#'@title Calculates the mean maximum age of fish in the community (Shin et al.,
#'  2010)
#'@description This function calculates the mean maximum age (MMA) of fish in
#'  the community for \eqn{j} areas and \eqn{i} years.
#'@details Mean Maximum Age (MMA): \deqn{MMA = \Sigma (age_{max,i}*B_i)/\Sigma
#'  B_i} where the sum is over all species \eqn{i}, and \eqn{B_i} is biomass of
#'  species \eqn{i}. The mean lifespan or longevity is considered to be a fixed
#'  parameter per species. Lifespan may vary under fishing pressure, so Shin et
#'  al. (2010) adopted the maximum longevity observed for each species
#'  (\eqn{age_{max,i}}). The variation of this indicator captures changes in
#'  species composition.
#'
#'  Recommended data: Fishery independent survey data or model output, finfish
#'  and squid.
#'@inheritParams biomassPerTL
#'@param  age.table  A table with 2 columns: "SPECIES" and "MAXAGE", the maximum
#'  recorded age of the corresponding species.
#'@return Returns a dataframe with 3 columns. "ID", "YEAR", and "MeanLifespan".
#'
#'  If there is no data for spatial scale \eqn{j} in year \eqn{i}, indicator
#'  value is assigned NA.
#'@family stability and resistance indicators
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

	meanMaxAge <- function(X, age.table, metric = "BIOMASS", years) {
		
	  age.table <- na.omit(age.table[, c("SPECIES", "MAXAGE")])
		X <- merge(X, age.table, by = "SPECIES")
		uI = unique(X$ID)                   # extract the spatial scale ID's
		ind <- NULL                         # initialize dataframe for storing indicator values
		
		for (j in 1:length(uI)){            # loop over all spatal scales
		  
		  X.j = X[X$ID == uI[j], ]          # subset data to spatial scale j
		  
		  for (i in 1:length(years)){                     # loop over each year
		    
		    year.i = years[i]                             # set years.i to current year  
		    X.ij = X.j[X.j$YEAR == year.i, ]              # subset data to include only current year
		    
		   if(nrow(X.ij) > 0){                            # set ind.i to NA if there are no observations in X.ij 
		      ind.i <- sum(X.ij[metric]*X.ij['MAXAGE'])/sum(X.ij[metric])	 # make sure this does what it should!
		   } else ind.i <- NA
		    
		    ind.i = data.frame(uI[j], year.i, ind.i)     # create a dataframe with spatial scale ID, year, and indicator value
		    ind = rbind(ind, ind.i)                      # bind ind.i to ind dataframe
		  }
		}
		names(ind) = c("ID", "YEAR", "MeanLifespan")    # name the ind dataframe
		ind <- ind[order(ind$ID), ]                          # order by ID to be consistent with other functions
		ind                                             # return vector of indicator values for years c(start.year:end.year) 
		
	}
	
	
	