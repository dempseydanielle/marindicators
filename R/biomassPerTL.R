#' @title Calculates the biomass or abundance per discrete trophic level
#' @description This function calculates the biomass or abundance per discrete
#'   trophic level for \eqn{j} areas and \eqn{i} years.
#' @details **Recommended data: Fishery independent surveys, fish and
#'   invertebrates.
#' @param X dataframe of fishery independent survey data with columns "YEAR",
#'   "ID", "SPECIES", and "BIOMASS" and/or "ABUNDANCE". "ID" is an area code
#'   designating where the observation was recorded. "SPECIES" is a numeric code
#'   indicating the species sampled.
#' @param TL.table dataframe with columns "SPECIES" and the corresponding "TL"
#'   (trophic level). **Note: for this draft TL.table = "scotianshelf" but will
#'   take this out for next draft
#' @param metric character string indicating whether to use "BIOMASS" or
#'   "ABUNDANCE" to calculate indicator.
#' @param TL.grouping size of the trophic level bin for which to aggregrate
#'   biomass (or abundance). Default is "TL.grouping = 1" so that biomass (or
#'   abundance) is aggregated over discrete trophic levels.
#' @param years vector of years for which to calculate indicator.
#' @return Returns a dataframe with columns "ID", "YEAR", and the corresponding
#'   biomass for each trophic level grouping, e.g., "BIOMASS_TL2",
#'   "BIOMASS_TL3", "BIOMASS_TL4"
#' @importFrom stats aggregate
#' @family stability and resistance indicators
#' @references Coll M, Shannon LJ, Moloney CL, Palomera I, Tudela S, 2006.
#'   Comparing trophic flows and fishing impacts of a NW Mediterranean ecosystem
#'   with coastal upwellings by means of standardized ecological models and
#'   indicators. Ecol. Model. 198, 53-70. (not in references!)
#'
#'   Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the selection and
#'   evaluation of ecological indicators. Can. Tech. Rep. Fish. Aquat. Sci.
#'   3232: xii + 212 p.
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#' @export


biomassPerTL <- function(X, TL.table,
                         metric=c('BIOMASS','ABUNDANCE'), TL.grouping = 1, 
                         years) {
  
  breaks <- seq(1, 10, by = TL.grouping)                          # create a vector from 1 to 10 (increasing by TL.grouping)
  TL.table['TL'] <- breaks[findInterval(TL.table[,'TL'], breaks)] # truncates "TL" (removes values after the decimal)
  
  X <- merge(X, TL.table, by = 'SPECIES')                                # merge X and the trophic level data		
  
  ind <- stats::aggregate(X[metric], by= X[c('ID', 'YEAR', 'TL')], FUN = sum)
  
  TL_biomass <- data.frame(years)
  names(TL_biomass) <- "YEAR"

  uTL <- unique(ind$TL) # unique trophic level
  for(i in 1:length(uTL)){
    
    TL.i <- uTL[i]
    biomass.i <- ind[ind$TL == TL.i, ]
    
    name.i <- paste(metric, "_TL", TL.i, sep = "")
    names(biomass.i)[4] <- name.i
    biomass.i$TL <- NULL
    
    TL_biomass <- merge(TL_biomass, biomass.i, all.y = T)
    
  }
  TL_biomass <- TL_biomass[order(TL_biomass$ID), ]
  TL_biomass	
}

