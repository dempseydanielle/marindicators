#' @title Calculates the the coefficient of variation of the biomass (Blanchard
#'   and Boucher, 2001)
#' @description This function calculates the coefficient of variation of the
#'   biomass for \eqn{j} areas and \eqn{i} years.
#' @details The coefficient of variation of the biomass (CVBiomass) was
#'   estimated as a five year moving average: \deqn{CVBiomass =
#'   (1/n)\Sigma(SD_i/Xbar_i)} The sum is from year \eqn{i} to year \eqn{i+n-1},
#'   where \eqn{n=5} for the 5-year moving average. \eqn{SD_i} is the standard
#'   deviation of the mean biomass and \eqn{X_i} is the mean biomass for year
#'   \eqn{i}.
#'
#'   Data used: Fishery independent survey data or model output; fish and
#'   invertebrates
#' @inheritParams biomassPerTL
#' @param X A dataframe of fishery independent survey data with columns "YEAR",
#'   "ID", and "BIOMASS". "YEAR" indicates the year the observation was
#'   recorded, "ID" is an area code indicating where the observation was
#'   recorded, and "BIOMASS" is the total recorded biomass (stratified and
#'   corrected for catchability as required). (Note: the function will aggregate
#'   biomass split over different "SPECIES" for each year, i.e., X can be the
#'   same dataframe used in functions biomassPerTL(), largeSpeciesIndicator(),
#'   meanMaxAge(), meanTLCommunity(), predatoryFish(), and resourcePotential()).
#' @param window Window for the moving averge. Default is 5 years.
#' @param negative If negative = TRUE, the indicator will be multipled by -1 so
#'   that the expected response is to decrease with increasing fishing pressure.
#'   Default is negative = FALSE.
#' @return returns a dataframe with three columns: "ID", "YEAR", and
#'   "CVBiomass".
#'
#'   If there is no data for spatial scale \eqn{j} in year \eqn{i}, indicator
#'   value is assigned NA.
#' @family stability and resistance indicators
#' @references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   Blanchard F, Boucher J (2001) Temporal variability of total biomass in
#'   harvested communities of demersal fishes. Fisheries Research. 49. 283–293.
#'   10.1016/S0165-7836(00)00203-4. (not in Tech refs)
#'
#'   Shin, YJ, Shannon LJ, Bundy A, Coll M, Aydin K, Bez N, Blanchard JL,
#'   Borges, MF, Diallo I, Diaz E, Heymans JJ, Hill L, Johannesen E, Jouffre D,
#'   Kifani S, Labrosse P, Link JS, Mackinson S, Masski H, Möllmann C, Neira S,
#'   Ojaveer H, Ould Mohammed Abdallahi ., Perry I, Thiao D, Yemane D, and Cury
#'   PM. 2010. Using indicators for evaluating, comparing and communicating the
#'   ecological status of exploited marine ecosystems. Part 2: Setting the
#'   scene. ICES Journal of Marine Science, 67: 692-716
#' @author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'   Catalina Gomez, Alida Bundy
#' @export


	CVBiomass <- function(X, window = 5, negative = FALSE, years){
	  
	  uI = unique(X$ID)                      # extract the spatial scale ID's
	  ind <- NULL                            # initialize dataframe for storing indicator values
	  years <- data.frame(years)
	  names(years) = "YEAR"

	  for (j in 1:length(uI)){               # loop over all spatal scales

	    X.j = X[X$ID == uI[j], ]             # subset data to spatial scale j

	    if(nrow(X) > window) {               # if nrow(X) < window, movingStatistics will not work 
	      
	      bio <- aggregate(BIOMASS ~ YEAR + ID, data = X.j, FUN = sum)  # sum biomass for each year
	      ind.j <- data.frame(YEAR = bio['YEAR'], 
	                       BIOMASS = movingStatistics(bio[,'BIOMASS'], n = window, stat='cv')) # 1/moving average of coefficient of variation of the biomass
	      min_year <- min(ind.j$YEAR)        # minimum year
	      max_year <- max(ind.j$YEAR)        # maximum year
	      
	      ind.j[ind.j$YEAR %in% c(min_year, min_year + 1, max_year - 1, max_year), "BIOMASS"] <- NA   # set the first and last two years to NA 
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
