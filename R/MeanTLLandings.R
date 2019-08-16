#'@title Calculates the mean trophic level or marine trophic index of fisheries
#'  landings (Pauly et al., 1998)
#'@description This function calculates the mean trophic level or marine trophic
#'  index of fisheries landings for \eqn{j} areas and \eqn{i} years.
#'@details Mean trophic level of fisheries landings \eqn{TL_{Land}}:
#'  \deqn{TL_{Land} = \Sigma (TL_i*Y_i)/Y} where \eqn{TL_i} is the trophic level
#'  of species \eqn{i}, \eqn{Y_i} is the landings of species \eqn{i}, and
#'  \eqn{Y} is the total landings of all species. Trophic Level of individual
#'  species is estimated either through an Ecopath model or dietary analysis, or
#'  taken from a global database such as Fishbase.
#'
#'  This indicator captures the average trophic level of the species exploited
#'  in the fishery. In general, it reflects a transition from long-lived, high
#'  trophic level, demersal fish toward short-lived, low trophic level pelagic
#'  fish and invertebrates.
#'
#'  The marine trophic index is calculated similarly to \eqn{TL_{Land}}, but
#'  only includes species with trophic level greater than or equal to an
#'  explicitly stated trophic level cutoff. For instance, Pauly and Watson 2005
#'  adopted a trophic level cutoff of 3.25 to emphasize changes in the relative
#'  abundance of higher trophic level fishes. If used is this way, this
#'  indicator highlights changes in the relative abundance of the more
#'  threatened high-trophic level fishes.
#'
#'  Recommended data: Commercial fisheries landings; fish and invertebrates.
#'@inheritParams landings
#'@param TL.table A dataframe with columns "SPECIES" and the corresponding
#'  "TL_LAND" (trophic level). Other columns in TL.table are ignored.
#'@param cutoff The minimum trophic level of species to include. Set cutoff = 0
#'  to calculate the mean trophic level of the landings; Set cutoff = 3.25 to
#'  calculate the marine trophic index. Default is cutoff = 0.
#'@return returns a dataframe with three columns: "ID", "YEAR", and if cutoff =
#'  0: "MeanTL.Landings" or if cutoff > 0: "MarineTophicIndex.Landings".
#'
#'  If there are no observations in land for spatial scale \eqn{j} in year
#'  \eqn{i}, indicator value is set to NA.
#'@family fishing pressure indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Pauly D, Christensen V, Dalsgaard J, Froese R, Torres F. 1998. Fishing Down
#'  Marine Food Webs. Science 279:860-863
#'
#'  Pauly D, Watson R. 2005. Background and interpretation of the Marine Trophic
#'  Index as a measure of biodiversity. Philos Trans R Soc B Biol Sci 360:415
#'  423
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@export

meanTLLandings <- function (land, TL.table, cutoff = 0, years) {
  
  uI <- unique(land$ID)                                     # extract the spatial scale ID's
  DF <- createDataframe(uI = uI, years = years)             # create a dataframe that matches each area ID to each year
  
  TL.table <- na.omit(TL.table[, c("SPECIES", "TL_LAND")])   
  land.TL <- merge(land, TL.table, by = "SPECIES")

  land.TL$id <- land.TL$TL_LAND >= cutoff                                                         # returns TRUE when TL is >= cutoff
  land.TL$pp <- land.TL$CATCH * land.TL$id                                                                                        # accounts for the proportion of the same species at different TL
                                                                                             # and species with two+ RV codes but one commercial code
  land.TL$LL <- land.TL$pp * land.TL$TL_LAND * land.TL$id                                         # calculates landings_species.i * TL_species.i

  ind <- merge(aggregate(LL ~ ID + YEAR, data = land.TL, FUN = sum),   # sum of landings of species i * TL of species i
               aggregate(pp ~ ID + YEAR, data = land.TL, FUN = sum))   # total landings
	ind$ind <- ind$LL/ind$pp                                             # calculate mean trophic level of landings weighted by species landed                                     
	ind$LL <- NULL                                                       # rm col LL
	ind$pp <- NULL                                                       # rm col pp
	 
	ind <- merge(DF, ind, by = c("ID", "YEAR"), all.x = T)               # merge ind with DF so that years without data are set to NA 
	
	if(cutoff == 0)	names(ind) <- c("ID", "YEAR", "MeanTL.Landings")
	if(cutoff >0) names(ind) <- c("ID", "YEAR", "MarineTophicIndex.Landings")
	
	ind <- ind[order(ind$ID), ]
	ind

}
