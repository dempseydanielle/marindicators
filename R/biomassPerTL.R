#' @title Calculates the biomass or abundance per discrete trophic level
#' @description This function takes a dataframe with columns **** and calculates
#'   the biomass or abundance per discrete trophic level.
#' @details **Recommended data: Fishery independent surveys, fish and
#'   invertebrates.
#' @param X add text here
#' @param table.of.trophic.level.data add text here
#' @param metric add text here
#' @param TL.grouping add text here
#' @param path add text here
#' @importFrom stats aggregate
#' @family stability and resistance indicators
#' @references Coll M, Shannon LJ, Moloney CL, Palomera I, Tudela S,
#'   2006. Comparing trophic flows and fishing impacts of a NW Mediterranean
#'   ecosystem with coastal upwellings by means of standardized ecological
#'   models and indicators. Ecol. Model. 198, 53-70. (not in references!)
#'
#'   Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the selection and
#'   evaluation of ecological indicators. Can. Tech. Rep. Fish. Aquat. Sci.
#'   3232: xii + 212 p.
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#' @export


biomassPerTL <- function(X, TL.table = "scotianshelf",
                         metric=c('BIOMASS','ABUNDANCE'), TL.grouping = 1, 
                         years = c(start.year:end.year)) {
 
  if (TL.table == "scotianshelf") {               # for Scotian Shelf ecosystem, import stored IndiSeas data
    load("R/sysdata.rda/indiseas_TL.rda")
    TL.table <- indiseas_wss_tl
    rm(indiseas_wss_tl)
  }
  
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

