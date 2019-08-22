#'@title Calculates the biomass per discrete trophic level
#'@description This function calculates the biomass per discrete trophic level
#'  for \eqn{j} areas and \eqn{i} years.
#'@details Recommended data: Fishery independent survey data or model output;
#'  fish and invertebrates.
#'@param X A dataframe of fishery independent survey data with columns "YEAR",
#'  "ID", "SPECIES", and "BIOMASS". "YEAR" indicates the year the observation
#'  was recorded, "ID" is an area code indicating where the observation was
#'  recorded, "SPECIES" is a numeric code indicating the species sampled, and
#'  "BIOMASS" is the corresponding biomass (stratified and corrected for
#'  catchability as required).
#'@param TL.table A dataframe with columns "SPECIES" and the corresponding "TL"
#'  (trophic level). Entries in the "SPECIES" column should be the unique values
#'  of species codes in X (or a subset thereof). Other columns in TL.table will
#'  be ignored.
#'@param metric A character string indicating which column in X to use to
#'  calculate indicator. Default is "BIOMASS".
#'@param TL.grouping Size of the trophic level bin for which to aggregrate
#'  biomass. For example, if TL.grouping = 1, trophic levels are binned from
#'  1.00 - 1.99, 2.00 - 2.99, etc. If TL.grouping = 0.5, trophic levels are
#'  binned from 1.00 - 1.49, 1.50 - 1.99, 2.00 - 2.49, 2.50 - 2.99, etc. Default
#'  is TL.grouping = 1 so that biomass is aggregated over discrete trophic
#'  levels.
#'@param years A vector of years for which to calculate indicator.
#'@return Returns a dataframe with columns "ID", "YEAR", and the corresponding
#'  biomass for each trophic level grouping. For example: for TL.grouping = 1,
#'  function will return columns "BIOMASS_TL2", "BIOMASS_TL3", "BIOMASS_TL4".
#'  For TL.grouping = 0.5, function will return columns "BIOMASS_TL2",
#'  "BIOMASS_TL2.5", "BIOMASS_TL3", "BIOMASS_TL3.5", "BIOMASS_TL4",
#'  "BIOMASS_TL4.5".
#'
#'  If there is no data for spatial scale \eqn{j} in year \eqn{i}, indicator
#'  value is assigned NA.
#'@importFrom stats aggregate
#'@family stability and resistance indicators
#'@references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Bundy, A. 2005. Structure and functioning of the eastern Scotian Shelf
#'  ecosystem before and after the collapse of groundfish stocks in the early
#'  1990s. Canadian Journal of Fisheries and Aquatic Sciences, 62(7),
#'  pp.1453-1473.
#'
#'  Coll M, Shannon LJ, Moloney CL, Palomera I, Tudela S, 2006. Comparing
#'  trophic flows and fishing impacts of a NW Mediterranean ecosystem with
#'  coastal upwellings by means of standardized ecological models and
#'  indicators. Ecol. Model. 198, 53-70.
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@export


biomassPerTL <- function(X, TL.table, metric = "BIOMASS", TL.grouping = 1, years) {
  
  breaks <- seq(1, 10, by = TL.grouping)                          # create a vector from 1 to 10 (increasing by TL.grouping)
  TL.table['TL'] <- breaks[findInterval(TL.table[,'TL'], breaks)] # truncates "TL" (removes values after the decimal)
  
  TL.table <- na.omit(TL.table[, c("SPECIES", "TL")])  
  X <- merge(X, TL.table, by = 'SPECIES')                         # merge X and the trophic level data		
  
  ind <- stats::aggregate(X[metric], by= X[c('ID', 'YEAR', 'TL')], FUN = sum) # calculate total biomass per TL
  
# Extract biomass at each TL ----------------------------------------------
  
  uI <- unique(X$ID)                            # extract the spatial scale ID's
  DF <- createDataframe(uI = uI, years = years) # create a dataframe that matches each area ID to each year

  uTL <- unique(ind$TL)     # extract unique trophic levels
  for(i in 1:length(uTL)){
    TL.i <- uTL[i]                                  # set TL.i to the current trophic level
    biomass.i <- ind[ind$TL == TL.i, ]              # subset ind to include only TL.i
    name.i <- paste(metric, "_TL", TL.i, sep = "")  # name indicator metric_TL.i
    names(biomass.i)[4] <- name.i
    biomass.i$TL <- NULL                            # remove TL column 
    
    DF <- merge(DF, biomass.i, all.x = T)           # merge biomass.i with DF so that years without data are set to NA 
  }
  
  DF <- DF[order(DF$ID), ]                          # order by ID to match other functions
  DF	

}

