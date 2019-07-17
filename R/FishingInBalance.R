#' @title Calculates the Fishing-in-Balance Index
#' @description This function takes a dataframe with columns **** and calculates
#'   the Fishing-in-Balance (FiB) Index
#' @details Fishing-in-Balance (FiB) Index: \deqn{FiB = log(Y_k*(1/TE)^{TL_k}) -
#'   log(Y_0 * (1/TE)^{TL_0})} where \eqn{Y} is the catch, \eqn{TL} is the mean
#'   trophic level in the catch, \eqn{TE} is the transfer efficiency, \eqn{k} is
#'   any year, and 0 refers to any year used as a baseline. By default, \eqn{TE}
#'   is set to 0.10 (Pauly and Christensen 1995).
#'
#'   This indicator captures changes in fishing strategies and their impact on
#'   system productivity: a positive FiB index indicates that the fishery has
#'   expanded and/or bottom-up effects are occurring, and there is more catch
#'   than expected, while a negative FiB index indicates it is likely that the
#'   fishing impact is so high that the ecosystem function is impaired and the
#'   ecosystem is less productive owing to excessive fishery removals
#'   (Christensen 2000, Fu et al. 2012).
#'
#'   **Recommended data: Commercial fisheries landings, fish and invertebrates.
#' @param X add text here
#' @param metric add text here
#' @param gp add text here
#' @param yr add text here
#' @param user.defined add text here
#' @param group add text here
#' @param path add text here
#' @importFrom stats aggregate
#' @family resource potential indicators
#' @references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   Christensen 2000 (not in Tech Report refs)
#'
#'   Fu C, Gaichas S, Link JS, Bundy A, Boldt JL, Cook AM, Gamble R, Utne KR,
#'   Liu H, Friedland KD (2012) Relative importance of fisheries, trophodynamic
#'   and environmental drivers in a series of marine ecosystems. Mar Ecol Prog
#'   Ser 459:169–184
#'
#'   Pauly D, Christensen V, Walters C (2000) Ecopath, Ecosim, and Ecospace as
#'   tools for evaluating ecosystem impact of fisheries. ICES J Mar Sci
#'   57:697–706
#'
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#' @export

# give option to CHOOSE years for the baseline --> base_start; base_end
FishingInBalance<- function (land, TE = 0.1,  base.start, base.end, 
                             years = c(start.year:end.year),
                             TL.table.land = "scotianshelf", prop.land.table = "scotianshelf",
                             cutoff = 0) {

  mTL <- MeanTLLandings(land = land, TL.table.land = TL.table.land, 
                        prop.land.table = prop.land.table, cutoff = 0) 
  land.total <- stats::aggregate(CATCH ~ YEAR + ID, data = land, FUN=sum) #  I think this is just total landings for each year
	
  mTL.0 <- aggregate(MeanTL.Landings ~ ID,
                     data = mTL[mTL$YEAR %in% base.start:base.end,], FUN = mean)         # BASELINE Mean trophic level
	land.0 <- aggregate(CATCH ~ ID, 
	                    data= land.total[land.total$YEAR %in% base.start:base.end,], FUN = mean) # BASELINE catch

	uI = unique(land$ID)                   # extract the spatial scale ID's
	ind <- NULL                         # initialize dataframe for storing indicator values
	
	for (j in 1:length(uI)){            # loop over all spatal scales
	  
	  mTL.j = mTL[mTL$ID == uI[j], ]                   # subset mean trophic level data to spatial scale j
	  land.total.j = land.total[land.total$ID == uI[j], ]      # subset total landings data to spatial scale j
	  
	  mTL.0.j = mTL.0[mTL.0$ID == uI[j], ]                 # subset baseline mean trophic level to spatial scale j
	  land.0.j = land.0[land.0$ID == uI[j], ]              # subset baselinelandings data to spatial scale j

	  for (i in 1:length(years)){                     # loop over each year
	    
	    year.i = years[i]                             # set years.i to current year  
	    mTL.ij = mTL.j[mTL.j$YEAR == year.i, ]               # subset mean trophic level data to year i
	    land.total.ij = land.total.j[land.total.j$YEAR == year.i, ]      # subset total landings data to year i
	    
	    ind.i <- (log(land.total.ij$CATCH*(1/TE)^mTL.ij$MeanTL.Landings) 
	              - log(land.0.j$CATCH*(1/TE)^mTL.0.j$MeanTL.Landings)) # calculate fishing in balance
	  
	    ind.i = data.frame(uI[j], year.i, ind.i)     # create a dataframe with spatial scale ID, year, and indicator value
	    ind = rbind(ind, ind.i)                      # bind ind.i to ind dataframe
	  }
	}
	names(ind) = c("ID", "YEAR", "FishinginBalance")    # name the ind dataframe
	ind                                                 # return vector of indicator values for years c(start.year:end.year) 
	
}

