#' @title Calculates the inverse of the coefficient of variation of the biomass
#' @description This function takes a dataframe with columns **** and calculates
#'   the inverse of the coefficient of variation of the biomass
#'   (InverseCVBiomass)
#' @details  The inverse of the coefficient of variation of the biomass
#'   (InverseCVBiomass) was estimated as a five year moving average:
#'   \deqn{InverseCVBiomass = 1/(1/n)\Sigma(SD_i/Xbar_i)} The sum is from year
#'   \eqn{i} to year \eqn{i+n-1}, where \eqn{n=5} for the 5-year moving average.
#'   \eqn{SD_i} is the standard deviation of the mean biomass and \eqn{X_i} is
#'   the mean biomass for year i.
#'
#'   Data used: Fishery independent surveys, fish and invertebrates
#' @param X add text here
#' @param window Say that default is 5 for 5-year moving average. add text here
#' @family stability and resistance indicators
#' @references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   Blanchard, F, Boucher, J (2001) Temporal variability of total biomass in
#'   harvested communities of demersal fishes. Fisheries Research. 49. 283–293.
#'   10.1016/S0165-7836(00)00203-4. (not in Tech refs)
#'
#'   Shin YJ, Shannon LJ (2010) Using indicators for evaluating, comparing, and
#'   communicating the ecological status of exploited marine ecosystems. 1. the
#'   indiSeas project. ICES J Mar Sci 67:686–691
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#' @export


	invCVBiomass <- function(X, window = 5){
	  
	  if(nrow(X)>window) {
	    Yp <- aggregate(BIOMASS ~ YEAR, data=X, FUN=sum)
	    aw <- data.frame(YEAR=Yp['YEAR'], BIOMASS=1/ movingStatistics(Yp[,'BIOMASS'], n = window, stat='cv'))
	    my <- min(aw$YEAR)
	    xy <- max(aw$YEAR)
	    out[[i]] <- aw[!aw$YEAR %in% c(my,my+1,xy-1,xy),] # ????
	    }
	  
	  return(do.call(rbind,out)) # not sure what do.call does. Probbaly replace this line with ind
	  }
