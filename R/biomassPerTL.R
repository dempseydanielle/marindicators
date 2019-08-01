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
#'   (trophic level).
#' @param metric character string indicating whether to use "BIOMASS" or
#'   "ABUNDANCE" to calculate indicator.
#' @param TL.grouping size of the trophic level bin for which to aggregrate
#'   biomass (or abundance). Default is "TL.grouping = 1" so that biomass (or
#'   abundance) is aggregated over discrete trophic levels.
#' @param years vector of years for which to calculate indicator.
#' @return Returns a dataframe with columns "ID", "YEAR", and the corresponding
#'   biomass for each trophic level grouping, e.g., "BIOMASS_TL2",
#'   "BIOMASS_TL3", "BIOMASS_TL4".
#'
#'   If there is no data for spatial scale \eqn{j} in year \eqn{i}, indicator
#'   value is assigned NA.
#' @importFrom stats aggregate
#' @family stability and resistance indicators
#' @references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   (Alida: this was not in Tech Report references!) Coll M, Shannon LJ,
#'   Moloney CL, Palomera I, Tudela S, 2006. Comparing trophic flows and fishing
#'   impacts of a NW Mediterranean ecosystem with coastal upwellings by means of
#'   standardized ecological models and indicators. Ecol. Model. 198, 53-70.
#'   

#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#' @export


biomassPerTL <- function(X, TL.table,
                         metric=c('BIOMASS','ABUNDANCE'), TL.grouping = 1, 
                         years) {
  
  breaks <- seq(1, 10, by = TL.grouping)                          # create a vector from 1 to 10 (increasing by TL.grouping)
  TL.table['TL'] <- breaks[findInterval(TL.table[,'TL'], breaks)] # truncates "TL" (removes values after the decimal)
  
  X <- merge(X, TL.table, by = 'SPECIES')                         # merge X and the trophic level data		
  
  ind <- stats::aggregate(X[metric], by= X[c('ID', 'YEAR', 'TL')], FUN = sum) # calculate total biomass per TL
  
# Extract biomass at each TL ----------------------------------------------

  years <- data.frame(years) # dataframe of all years
  names(years) <- "YEAR"     
  
  uI <- unique(X$ID)         # extract the spatial scale ID's
  df <- NULL                 # create empty dataframe 
  for(j in 1:length(uI)){    # create a dataframe with all years for each ID
    ID.j <- rep(uI[j], nrow(years))
    df.j <- cbind(ID.j, years)
    names(df.j) <- c("ID", "YEAR")
    df <- rbind(df, df.j)   # a dataframe with two columns ("YEAR" and "ID")
  }

  uTL <- unique(ind$TL)     # extract unique trophic levels
  for(i in 1:length(uTL)){
    biomass <- NULL         # create an empty dataframe to store biomass
    
    TL.i <- uTL[i]                                  # set TL.i to the current trophic level
    biomass.i <- ind[ind$TL == TL.i, ]              # subset ind to include only TL.i
    name.i <- paste(metric, "_TL", TL.i, sep = "")  # name indicator metric_TL.i
    names(biomass.i)[4] <- name.i
    biomass.i$TL <- NULL                            # remove TL column 
    
    for(j in 1:length(uI)){                         # for years without data: add ID to ID column and NA to Biomass column
      
      biomass.ij <- biomass.i[biomass.i$ID == uI[j], ]  # subset data to include only spatial scale j
      biomass.ij <- merge(years, biomass.ij, all.x = T) # merge biomass with years (for years without data, this adds NA to ID and BIOMASS colums)
      na.inx <- which(is.na(biomass.ij$ID))             # index of where ID is NA
      biomass.ij$ID[na.inx] <- uI[j]                    # replace NAs in col ID with the ID for spatial scale j
      
      biomass <- rbind(biomass, biomass.ij)             # collect biomass at TL.i for all spatial scales
    }
    
    df <- merge(df, biomass)        # merge with years
  }
  
  df <- df[order(df$ID), ]          # order by ID to match other functions
  df	
}

