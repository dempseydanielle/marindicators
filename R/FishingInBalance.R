#'@title Calculates the Fishing-in-Balance Index
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
#'  ecosystem is less productive owing to excessive fishery removals
#'  (Christensen 2000, Fu et al. 2012).
#'
#'  **Recommended data: Commercial fisheries landings, fish and invertebrates.
#'@param land dataframe of commercial landings data with columns "YEAR", "ID",
#'  "ALLCODES" and "CATCH". "ID" is an area code designating where the
#'  observation was recorded. "ALLCODES" is a numeric commercial species code
#'  indicating the species landed, and "CATCH" is the corresponding landed
#'  weight.
#'@param TL.table dataframe with four columns: "SPECIES", "GROUP_NAME", "TL",
#'  and "LANDED". "SPECIES" is the fishery-independent species code, and
#'  "GROUP_NAME" and "TL" are the corresponding size-based functional group and
#'  trophic level, respectively. Note that one SPECIES code may be assigned more
#'  than one GROUP_NAME if ontogentic shifts occur. For example, on the Scotian
#'  Shelf small (< 33 cm) and large (>33 cm) haddock both have SPECIES code 11.
#'  Small haddock are assigend to GROUP_NAME "Haddock<33" with trophic level
#'  3.38. Large haddock are assigned to GROUP_NAME "Haddock33+" with trophic
#'  level 3.43. "LANDED" estimates the proportion of the total landings of a
#'  given species that is represented by GROUP_NAME. Landings of small haddock
#'  are negligible in this example, and so Haddock<33 is assigned LANDED = 0,
#'  while Haddock33+ is assigned LANDED = 1.
#'@param propland.table dataframe with three columns: "SPECIES", "ALLCODE", and
#'  "PROPORTION_OF_LANDINGS". "SPECIES" is is the fisheries-independent numeric
#'  species code (as in TL.table), and "ALLCODES" is the corresponding numeric
#'  commercial species code (as in land). "PROPORTION_OF_LANDINGS" is relevant
#'  to species that have different SPECIES codes, but the same ALLCODES code.
#'  For example, on the Scotian Shelf, longhorn sculpins are assigned a SPECIES
#'  code of 300, while sea ravens are assigned a species code of 320; however
#'  they are grouped together in the commercial landings data and are both
#'  assigned ALLCODE 174. The "PROPORTION_OF_LANDINGS" column estimates the
#'  proportion of each species that makes up the commercial landings. In this
#'  example, longhorn sculpins consist of about 40% of the total sculpin
#'  landings and are assigned a "PROPORTION_OF_LANDINGS" value of 0.4. Sea
#'  ravens consist of about 60% of the total sculpin landings and are assigned a
#'  "PROPORTION_OF_LANDINGS" value of 0.6.
#'@param cutoff the minimum trophic level of species to include. Set cutoff = 0
#'  to calculate the mean trophic level of the landings; Set cutoff = 3.25 to
#'  calculate the marine trophic index.
#'@param TE trophic efficiency. Default is TE = 0.1.
#'@param base.start beginning of baseline period
#'@param base.end end of baseline period
#'@param years vector of years for which to calculate indicator
#'@return returns a dataframe with three columns: "ID", "YEAR", and
#'  "FishinginBalance"
#'@importFrom stats aggregate
#'@family resource potential indicators
#'@references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  (not in Tech Report refs) Christensen V. 2000. Indicators for marine
#'  ecosystems affected by fisheries. Mar Freshwater Res. 51(5) 447-450
#'
#'  Fu C, Gaichas S, Link JS, Bundy A, Boldt JL, Cook AM, Gamble R, Utne KR, Liu
#'  H, Friedland KD. 2012. Relative importance of fisheries, trophodynamic and
#'  environmental drivers in a series of marine ecosystems. Mar Ecol Prog Ser
#'  459:169–184
#'
#'  Pauly D, Christensen V, Walters C. 2000. Ecopath, Ecosim, and Ecospace as
#'  tools for evaluating ecosystem impact of fisheries. ICES J Mar Sci
#'  57:697–706
#'
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#'@export

FishingInBalance<- function (land, TL.table, propland.table,
                             cutoff = 0, TE = 0.1,   
                             base.start, base.end, years) {

  mTL <- MeanTLLandings(land = land, TL.table = TL.table, 
                        propland.table = propland.table, cutoff = 0) 
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

