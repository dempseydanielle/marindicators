#' @title Calculates the inverse of the coefficient of variation of the biomass
#' @description This calculates the inverse of the coefficient of variation of
#'   the biomass (InverseCVBiomass) for \eqn{j} areas and \eqn{i} years.
#' @details  The inverse of the coefficient of variation of the biomass
#'   (InverseCVBiomass) was estimated as a five year moving average:
#'   \deqn{InverseCVBiomass = 1/(1/n)\Sigma(SD_i/Xbar_i)} The sum is from year
#'   \eqn{i} to year \eqn{i+n-1}, where \eqn{n=5} for the 5-year moving average.
#'   \eqn{SD_i} is the standard deviation of the mean biomass and \eqn{X_i} is
#'   the mean biomass for year i.
#'
#'   Data used: Fishery independent surveys, fish and invertebrates
#' @param X dataframe of fishery independent survey data with columns "YEAR",
#'   "ID", "SPECIES", and "BIOMASS". "ID" is an area code designating where the
#'   observation was recorded. "SPECIES" is a numeric code indicating the
#'   species sampled.
#' @param window window for the moving averge. Default is 5 years.
#' @param years vector of years for which to calculate indicator
#' @return returns a dataframe with three columns: "ID", "YEAR", and
#'   "invCVbiomass".
#'
#'   If there is no data for spatial scale \eqn{j} in year \eqn{i}, indicator
#'   value is assigned NA.
#' @family stability and resistance indicators
#' @references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   Blanchard, F, Boucher, J (2001) Temporal variability of total biomass in
#'   harvested communities of demersal fishes. Fisheries Research. 49.
#'   283–293. 10.1016/S0165-7836(00)00203-4. (not in Tech refs)
#'
#'   Shin YJ, Shannon LJ (2010) Using indicators for evaluating, comparing, and
#'   communicating the ecological status of exploited marine ecosystems. 1. the
#'   indiSeas project. ICES J Mar Sci 67:686–691
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#' @export


	invCVBiomass <- function(X, window = 5, years){
	  
	  uI = unique(X$ID)                      # extract the spatial scale ID's
	  ind <- NULL                            # initialize dataframe for storing indicator values
	  years <- data.frame(years)
	  names(years) = "YEAR"

	  for (j in 1:length(uI)){               # loop over all spatal scales

	    X.j = X[X$ID == uI[j], ]             # subset data to spatial scale j

	    if(nrow(X) > window) {               # if nrow(X) < window, movingStatistics will not work 
	      
	      bio <- aggregate(BIOMASS ~ YEAR + ID, data = X.j, FUN = sum)  # sum biomass for each year
	      ind.j <- data.frame(YEAR = bio['YEAR'], 
	                       BIOMASS = 1/ movingStatistics(bio[,'BIOMASS'], n = window, stat='cv')) # 1/moving average of coefficient of variation of the biomass
	      min_year <- min(ind.j$YEAR)        # minimum year
	      max_year <- max(ind.j$YEAR)        # maximum year
	      
	      ind.j[ind.j$YEAR %in% c(min_year, min_year + 1, max_year - 1, max_year), "BIOMASS"] <- NA   # set the first and last two years to NA 
	      ind.j <- merge(years, ind.j, all.x = T)
	   
	      ind.j= data.frame(rep(uI[j], nrow(ind.j)), ind.j)     # create a dataframe with spatial scale ID, year, and indicator value
	      names(ind.j) <- c("ID", "YEAR", "invCVbiomass")
	      
	      ind = rbind(ind, ind.j)  
	    }
	    
	    }

	  ind <- ind[order(ind$ID), ] 
	  ind
	 
	  }
