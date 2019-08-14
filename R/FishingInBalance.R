#'@title Calculates the Fishing-in-Balance Index (Pauly et al., 2000)
#'@description This function calculates the Fishing-in-Balance (FiB) Index of
#'  fisheries landings for \eqn{j} areas and \eqn{i} years.
#'@details Fishing-in-Balance (FiB) Index: \deqn{FiB = log(Y_k*(1/TE)^{TL_k}) -
#'  log(Y_0 * (1/TE)^{TL_0})} where \eqn{Y} is the catch, \eqn{TL} is the mean
#'  trophic level in the catch, \eqn{TE} is the transfer efficiency, \eqn{k} is
#'  any year, and 0 refers to any year used as a baseline. By default, \eqn{TE}
#'  is set to 0.10 (Pauly and Christensen 1995).
#'
#'  This indicator captures changes in fishing strategies and their impact on
#'  system productivity: a positive FiB index indicates that the fishery has
#'  expanded and/or bottom-up effects are occurring, and there is more catch
#'  than expected, while a negative FiB index indicates it is likely that the
#'  fishing impact is so high that the ecosystem function is impaired and the
#'  ecosystem is less productive owing to excessive fishery removals.
#'
#'  Recommended data: Commercial fisheries landings; fish and invertebrates.
#'@inheritParams meanTLLandings
#'@param cutoff The minimum trophic level of species to include.
#'@param TE Trophic efficiency. Default is TE = 0.1.
#'@param base.start The beginning of the baseline period.
#'@param base.end The end of the baseline period.
#'@return returns a dataframe with three columns: "ID", "YEAR", and
#'  "FishinginBalance".
#'
#'  If there are no observations in land for spatial scale \eqn{j} and year
#'  \eqn{i}, indicator value is set to NA.
#'@importFrom stats aggregate
#'@family resource potential indicators
#'@references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Pauly D, Christensen V, Walters C. 2000. Ecopath, Ecosim, and Ecospace as
#'  tools for evaluating ecosystem impact of fisheries. ICES J Mar Sci 57:697
#'  706
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@export

fishingInBalance<- function (land, TL.table, cutoff = 0, TE = 0.1,   
                             base.start, base.end, years) {
  
  mTL <- meanTLLandings(land = land, TL.table = TL.table,                  # calculate mean trophic level of landings
                        cutoff = 0, years = c(base.start:years[length(years)])) 
  land.total <- stats::aggregate(CATCH ~ YEAR + ID, data = land, FUN=sum)  #  calculate total landings for each spatial scale and year
	
  mTL.0 <- aggregate(MeanTL.Landings ~ ID,                                 # calculate BASELINE Mean trophic level
                     data = mTL[mTL$YEAR %in% base.start:base.end,], 
                     FUN = mean)       
	land.0 <- aggregate(CATCH ~ ID,                                          # calculate BASELINE landings
	                    data= land.total[land.total$YEAR %in% base.start:base.end,], 
	                    FUN = mean) 

	uI = unique(land$ID)                   # extract the spatial scale ID's
	ind <- NULL                            # initialize dataframe for storing indicator values
	
	for (j in 1:length(uI)){               # loop over all spatal scales
	  
	  mTL.j = mTL[mTL$ID == uI[j], ]                           # subset mean trophic level data to spatial scale j
	  land.total.j = land.total[land.total$ID == uI[j], ]      # subset total landings data to spatial scale j
	  
	  mTL.0.j = mTL.0[mTL.0$ID == uI[j], ]                 # subset baseline mean trophic level to spatial scale j
	  land.0.j = land.0[land.0$ID == uI[j], ]              # subset baselinelandings data to spatial scale j

	  for (i in 1:length(years)){                          # loop over each year
	    
	    year.i = years[i]                                                # set years.i to current year  
	    mTL.ij = mTL.j[mTL.j$YEAR == year.i, ]                           # subset mean trophic level data to year i
	    land.total.ij = land.total.j[land.total.j$YEAR == year.i, ]      # subset total landings data to year i
	    
	    if(nrow(mTL.ij) > 0 & nrow(land.total.ij) > 0){   # if there are no observations in mTL.ij or land.total.ij, ind.i is set is to NA
	      ind.i <- (log(land.total.ij$CATCH*(1/TE)^mTL.ij$MeanTL.Landings) 
	              - log(land.0.j$CATCH*(1/TE)^mTL.0.j$MeanTL.Landings))         # calculate fishing in balance
	    }else ind.i <- NA
	  
	    ind.i = data.frame(uI[j], year.i, ind.i)     # create a dataframe with spatial scale ID, year, and indicator value
	    ind = rbind(ind, ind.i)                      # bind ind.i to ind dataframe
	  }
	}
	names(ind) = c("ID", "YEAR", "FishinginBalance")    # name the ind dataframe
	ind <- ind[order(ind$ID), ] 
	ind                                                 # return vector of indicator values for years c(start.year:end.year) 
	
}

