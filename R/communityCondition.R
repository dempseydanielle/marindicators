#' @title Calculates Fulton's Condition Index for the community (Ricker, 1975)
#' @description This function calculates Fulton's condition index for \eqn{j}
#'   areas and \eqn{i} years.
#' @details Fulton's Condition Index (\eqn{K}): \deqn{K = \Sigma(K_j *
#'   A_j)/\Sigma A_j} where the sum is over all species, \eqn{j}, \eqn{A_j} is
#'   the abundance of species \eqn{j}, and \deqn{K_j = 100*W_j/L_j^3} where
#'   \eqn{W_j} is the mean weight at length \eqn{L} for species \eqn{j}.
#'
#'   Recommended data: Fishery independent survey data or model output; fish.
#' @inheritParams resourcePotential
#' @param X A dataframe of fishery independent survey data with columns "YEAR",
#'   "ID", "SPECIES", "LENGTH", and "ABUNDANCE". "YEAR" indicates the year the
#'   observation was recorded, "ID" is an area code indicating where the
#'   observation was recorded, and "SPECIES" is a numeric code indicating the
#'   species sampled. "LENGTH" is the length class (cm) and "ABUNDANCE" is the
#'   corresponding abundance at length (stratified and corrected for
#'   catchability as required). Species for which there are no length data
#'   should be assigned LENGTH = -99. These observations are removed by the
#'   function.
#' @param LenWt.table A table of annual length at weight data with 5 columns.
#'   "YEAR", "ID", "SPECIES" correspond with those columns in X. "LENGTH" is
#'   fish length at the corresponding "WEIGHT" (fish weight).
#' @return Returns a dataframe with 3 columns. "ID", "YEAR", and
#'   "CommunityCondition_group".
#'
#'   If there is no data for spatial scale \eqn{j} in year \eqn{i}, indicator
#'   value is assigned NA.
#' @family ecosystem structure and function indicators
#' @references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   DFO. 2003. State of the Eastern Scotian Shelf ecosystem. Dartmouth, Nova
#'   Scotia
#'
#'   Choi JS, Frank KT, Petrie BD, Leggett WC. 2005. Integrated Assessment of a
#'   Large Marine Ecosystem: a case study of the devolution of the Eastern
#'   Scotian Shelf, Canada. Oceanogr Mar Biol An Annu Rev 43:47–67
#'
#'   Ricker, W. E. 1975. Computation and interpretation of biological statistics
#'   of fish populations. Bulletin of the Fisheries Research Board of Canada
#'   191:1-382.
#' @author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'   Catalina Gomez, Alida Bundy
#' @export


communityCondition <- function(X, group, species.table = NULL, LenWt.table, years) {
  
  X <- speciesGroups(X = X, group = group, species.table = species.table) # subset X to the species of interest
  
  inx99 <- which(X$LENGTH == -99)                          # index of rows that do not contain length data               
  if(length(inx99 > 0)) X <- X[-which(X$LENGTH == -99), ]  # remove rows that do not contain length data
  
  uI = unique(X$ID)                                      # extract the spatial scale ID's
  ind <- NULL                                            # initialize dataframe for storing indicator values
  
  for (j in 1:length(uI)){                               # loop over all spatal scales
    
    X.j = X[X$ID == uI[j], ]                             # subset biomass and abundance data to spatial scale j
    len_wgt.j = LenWt.table[LenWt.table$ID == uI[j], ]   # subset length-weight data to spatial scale j
    
    for (i in 1:length(years)){                          # loop over each year
      
      year.i = years[i]                                  # set years.i to current year  
      X.ij = X.j[X.j$YEAR == year.i, ]                   # subset biomass and abundance data to include only current year
      len_wgt.ij = len_wgt.j[len_wgt.j$YEAR == year.i, ] # subset length-weight data to include only current year
      
      if(nrow(X.ij) > 0 & nrow(len_wgt.ij) > 0){         # if there are no observations in X.ij or len_wgt.ij, ind.i is set is to NA
        
        W <- aggregate(WEIGHT ~ LENGTH + SPECIES + ID, data = len_wgt.ij, FUN = mean) # fish weights by length and species
        
        if(any(unique(X.ij$SPECIES) %in% unique(W$SPECIES)))  {                  # if there are the same species in Y (biomass) and W (weight). . . 
          Z <- merge(X.ij, W, by = c('ID', 'SPECIES','LENGTH'), all.y = T)         # merge them
          
          inx.na <- which(is.na(Z$ABUNDANCE))  # index of where Z$metric is zero
          
         if(nrow(Z) > length(inx.na)){        # if all of the rows of Z$metric are NA, then set ind.i to NA
           
             Z <- merge(Z, aggregate(ABUNDANCE~ID, data = Z, FUN = sum), by = 'ID') # add a column of total abundance (same for each row)
            Z$K <- Z$WEIGHT / Z$LENGTH^3*100                                               # calculate K for each species
            ind.i <- aggregate(Z$K*Z$ABUNDANCE.x/Z$ABUNDANCE.y ~ ID, data = Z, FUN = sum)   # calculate Fulton's condition index
            ind.i  <- ind.i[,2]
            
            }   else ind.i <- NA
          }   else ind.i <- NA
        } else ind.i <- NA
          
          ind.i = data.frame(uI[j], year.i, ind.i)          # create a dataframe with spatial scale ID, year, and indicator value
          ind = rbind(ind, ind.i)                           # bind ind.i to ind dataframe
      }
    }
    
  name.ind <- paste("CommunityCondition_", group, sep = "")  # name indicator metric_TL.i
  names(ind) = c("ID", "YEAR", name.ind)    # name the ind dataframe
  ind <- ind[order(ind$ID), ]                          # order by ID to be consistent with other functions
  ind                                                   # return ind 
}
