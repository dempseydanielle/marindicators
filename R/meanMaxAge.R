#'@title Calculates the mean maximum age of fish in the community
#'@description This function takes a dataframe with columns **** and calculates
#'  the mean maximum age (MMA) of fish in the community
#'@details Mean Maximum Age (MMA): \deqn{MMA = \Sigma (age_{max,i}*B_i)/\Sigma
#'  B_i} where the sum is over all species \eqn{i}, and \eqn{B_i} is biomass of
#'  species \eqn{i}. The mean lifespan or longevity is considered to be a fixed
#'  parameter per species. Lifespan may vary under fishing pressure, so IndiSeas
#'  adopted the maximum longevity observed for each species (\eqn{age_{max,i}}).
#'  The variation of this indicator captures changes in species composition.
#'
#'  Recommended data: Fishery independent surveys, finfish and squid.
#'@param X add text here
#'@param table.of.age.data add text here --or delete
#'@param metric add text here
#'@family stability and resistance indicators
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


	meanMaxAge <- function(X, age.table = "scotianshelf", metric=c('BIOMASS','ABUNDANCE'),
	                       years = c(start.year:end.year)) {
		
	  if (age.table == "scotianshelf"){
	    load("R/sysdata.rda/indiseas_MaxAge.rda")
	    age.table = indiseas_MaxAge
	    rm(indiseas_MaxAge)
	  }
	  
		X <- merge(X, age.table, by ='SPECIES')
		uI = unique(X$ID)                   # extract the spatial scale ID's
		ind <- NULL                         # initialize dataframe for storing indicator values
		
		for (j in 1:length(uI)){            # loop over all spatal scales
		  
		  X.j = X[X$ID == uI[j], ]          # subset data to spatial scale j
		  
		  for (i in 1:length(years)){                     # loop over each year
		    
		    year.i = years[i]                             # set years.i to current year  
		    X.ij = X.j[X.j$YEAR == year.i, ]              # subset data to include only current year
		  
		    ind.i <- sum(X.ij[metric]*X.ij['MAXAGE'])/sum(X.ij[metric])	 # make sure this does what it should!
	
		    ind.i = data.frame(uI[j], year.i, ind.i)     # create a dataframe with spatial scale ID, year, and indicator value
		    ind = rbind(ind, ind.i)                      # bind ind.i to ind dataframe
		  }
		}
		names(ind) = c("ID", "YEAR", "MeanLifespan")    # name the ind dataframe
		ind                                             # return vector of indicator values for years c(start.year:end.year) 
		
	}
	
	
	