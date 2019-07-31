#' @title Calculates Fulton's Condition Index for the community
#' @description This function takes a dataframe of length-based fisheries
#'   independent survey data and calculates Fulton's condition index for \eqn{j}
#'   areas and \eqn{i} years.
#' @details Fulton's Condition Index (\eqn{K}): \deqn{K = \Sigma(K_j *
#'   A_j)/\Sigma A_j} where the sum is over all species, \eqn{j}, \eqn{A_j} is
#'   the abundance of species \eqn{j}, and \deqn{K_j = 100*W_j/L_j^3} where
#'   \eqn{W_j} is the mean weight at length \eqn{L} for species \eqn{j}.
#'
#'   **Recommended data: Fishery independent surveys, fish.
#' @param X dataframe of fishery independent survey data with columns "YEAR",
#'   "ID", "SPECIES", "FLEN", and "BIOMASS" and/or "ABUNDANCE". "ID" is an area
#'   code designating where the observation was recorded (a string). "SPECIES"
#'   is a numeric code indicating the species sampled. "FLEN" is the length
#'   class (cm) and "BIOMASS" and "ABUNDANCE" are the corresponding biomass and
#'   abundance at length. Species for which there are no length data should be
#'   assigned FLEN = -99. These observations are removed by the function.
#' @param group character string indicating which group of species to include.
#'   Note that this subsetting is based on the Fisheries and Oceans Canada
#'   species codes for the Scotian Shelf. For other regions it may be prudent to
#'   subsetdata to species groups of interest prior to using the function and
#'   then choose group = "ALL". Type ?speciesgroups for more information.
#' @param metric character string indicating which metric to use to calculate
#'   indicator. Default is set to "ABUNDANCE".
#' @param LenWt.table table of annual length at weight data with 5 columns.
#'   "YEAR", "ID", "SPECIES" correspond with those columns in X. "FLEN" is fish
#'   length at the corresponding "FWT" (fish weight). 
#' @param years vector of years for which to calculate indicator.
#' @return Returns a dataframe with 3 columns. "ID", "YEAR", and
#'   "CommunityCondition"
#' @family ecosystem structure and function indicators
#' @references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   DFO (2003) State of the Eastern Scotian Shelf ecosystem. Dartmouth, Nova
#'   Scotia
#'
#'   Choi JS, Frank KT, Petrie BD, Leggett WC (2005) Integrated Assessment of a
#'   Large Marine Ecosystem: a case study of the devolution of the Eastern
#'   Scotian Shelf, Canada. Oceanogr Mar Biol An Annu Rev 43:47–67
#'
#'   Rochet M, Rice JC (2005) Do explicit criteria help in selecting indicators
#'   for ecosystem-based fisheries management? ICES J Mar Sci 62:528–539
#'
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#' @export


communityFultonK <- function(X, group=c('ALL', 'FINFISH'),
                             metric ='ABUNDANCE', LenWt.table,
                             years) {
  
  if(group != "ALL") X <- speciesgroups(X = X, group = group) # subset X to the species of interest
  
  X <- X[-which(X$FLEN == -99), ]                        # remove rows that do not contain length data
  uI = unique(X$ID)                                      # extract the spatial scale ID's
  ind <- NULL                                            # initialize dataframe for storing indicator values
  
  for (j in 1:length(uI)){                               # loop over all spatal scales
    
    X.j = X[X$ID == uI[j], ]                             # subset biomass and abundance data to spatial scale j
    len_wgt.j = LenWt.table[LenWt.table$ID == uI[j], ]           # subset length-weight data to spatial scale j
    
    for (i in 1:length(years)){                          # loop over each year
      
      year.i = years[i]                                  # set years.i to current year  
      X.ij = X.j[X.j$YEAR == year.i, ]                   # subset biomass and abundance data to include only current year
      len_wgt.ij = len_wgt.j[len_wgt.j$YEAR == year.i, ] # subset length-weight data to include only current year
      
      if(nrow(X.ij) > 0 & nrow(len_wgt.ij) > 0){         # if there are no observations in X.ij or len_wgt.ij, ind.i is set is to NA
        
        W <- aggregate(FWT ~ FLEN + SPECIES + ID, data = len_wgt.ij, FUN = mean) # fish weights by length and species
        
        if(any(unique(X.ij$SPECIES) %in% unique(W$SPECIES)))  {                  # if there are the same species in Y (biomass) and W (weight). . . 
          Z <- merge(X.ij, W, by = c('ID', 'SPECIES','FLEN'), all.y = T)         # merge them
          Z <- merge(Z, aggregate(ABUNDANCE~ID, data = Z, FUN = sum), by = 'ID') # add a column of total abundance (same for each row)
          
          Z$K <- Z$FWT / Z$FLEN^3*100                                               # calculate K for each species
          ind.i <- aggregate(Z$K*Z$ABUNDANCE.x/Z$ABUNDANCE.y ~ ID, data = Z, FUN = sum)   # calculate Fulton's condition index
          ind.i  <- ind.i[,2]
          }   
        } else ind.i <- NA
          
          ind.i = data.frame(uI[j], year.i, ind.i)          # create a dataframe with spatial scale ID, year, and indicator value
          ind = rbind(ind, ind.i)                           # bind ind.i to ind dataframe
      }
    }
    
  names(ind) = c("ID", "YEAR", "CommunityCondition")    # name the ind dataframe
  ind <- ind[order(ind$ID), ]                          # order by ID to be consistent with other functions
  ind                                                   # return ind 
}
