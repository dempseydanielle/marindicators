#'@title Calculates the Coefficient of Variation of the Biomass
#'@description This function calculates the Coefficient of Variation of the
#'  Biomass for \eqn{j} areas and \eqn{i} years.
#'@details The Coefficient of Variation of the Biomass (CVBiomass) was estimated
#'  as an n-year moving average (Blanchard and Boucher, 2001):
#'
#'  CVBiomass = sd(total biomass for the past n years)/mean(total biomass for
#'  the past n years)
#'@inheritParams biomassPerTL
#'@param X A dataframe of fishery independent survey data or model output with
#'  columns \code{YEAR}, \code{ID}, and \code{BIOMASS}.  \code{YEAR} indicates
#'  the year the observation was recorded, \code{ID} is an area code indicating
#'  where the observation was recorded, and \code{BIOMASS} is the total recorded
#'  biomass (stratified and corrected for catchability as required). (Note: if
#'  \code{X} has an additional \code{SPECIES} column, the function will
#'  automatically calculate the total biomass).
#'@param wind Window for the moving average. The first and last
#'  \code{floor(wind/2)} values of the indicator are assigned \code{NA} to
#'  account for the moving average. Default is \code{wind = 5} years.
#'@param negative If \code{negative = TRUE}, the indicator will be multiplied by
#'  -1 so that the expected response is to decrease with increasing fishing
#'  pressure. Default is \code{negative = FALSE}.
#'@return Returns a dataframe with three columns: \code{ID}, \code{YEAR}, and
#'  \code{CVBiomass}.
#'
#'  The first and last \code{floor(wind/2)} values of the indicator are assigned
#'  \code{NA} to account for the moving average. If there is no data for spatial
#'  scale \eqn{j} in year \eqn{i}, indicator value is assigned NA.
#'@family stability and resistance indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Blanchard F, Boucher J (2001) Temporal variability of total biomass in
#'  harvested communities of demersal fishes. Fisheries Research. 49. 283–293.
#'  10.1016/S0165-7836(00)00203-4. (not in Tech refs)
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
#'@examples
#'data(X)
#'CVBiomass(X, wind = 5, negative = TRUE, years = c(2014:2019))
#'@export


	CVBiomass <- function(X, wind = 5, negative = FALSE, years){
	  
	  inx <- floor(wind/2)                       # determine how many NAs at the beginning and end, based on size of wind
	  inx.start <- years[c(1:inx)]                 # first inx years
	  inx <- inx - 1
	  inx.end <- years[(length(years) - c(0:inx))] # last inx years
	  inx.NA <- c(inx.start, inx.end)              # all years that will be set to NA
	  
	  uI = unique(X$ID)                      # extract the spatial scale ID's
	  ind <- NULL                            # initialize dataframe for storing indicator values
	  years <- data.frame(years)
	  names(years) = "YEAR"

	  for (j in 1:length(uI)){               # loop over all spatal scales

	    X.j = X[X$ID == uI[j], ]             # subset data to spatial scale j

	    if(nrow(X) > wind) {               # if nrow(X) < wind, movingStatistics will not work 
	      
	      bio <- aggregate(BIOMASS ~ YEAR + ID, data = X.j, FUN = sum)  # sum biomass for each year
	      ind.j <- data.frame(YEAR = bio['YEAR'], 
	                       BIOMASS = movingStatistics(bio[,'BIOMASS'], n = wind, stat='cv')) # moving average of coefficient of variation of the biomass

	      ind.j[ind.j$YEAR %in% inx.NA, "BIOMASS"] <- NA   # set the first and last floor(wind/2) years to NA 
	      
	      ind.j <- merge(years, ind.j, all.x = T)
	   
	      ind.j= data.frame(rep(uI[j], nrow(ind.j)), ind.j)     # create a dataframe with spatial scale ID, year, and indicator value
	      names(ind.j) <- c("ID", "YEAR", "CVBiomass")
	      
	      ind = rbind(ind, ind.j)  
	    }
	    
	  }
	  
	  if(negative == TRUE) ind$CVBiomass <- ind$CVBiomass * (-1)
	  ind <- ind[order(ind$ID), ] 
	  ind
	 
	  }
