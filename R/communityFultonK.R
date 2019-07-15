#' @title Calculates Fulton's Condition Index for the community
#' @description This function takes a dataframe with columns **** and calculates
#'   Fulton's Condition Index (\deqn{K}) for the community (weighted by
#'   abundance)
#' @details Fulton's Condition Index (\eqn{K}): \deqn{K = \Sigma(K_j *
#'   A_j)/\Sigma A_j} where the sum is over all species, \eqn{j}, \eqn{A_j} is
#'   the abundance of species \eqn{j}, and \deqn{K_j = 100*W_j/L_j^3} where
#'   \eqn{W_j} is the mean weight at length \eqn{L} for species \eqn{j}.
#'
#'   **Recommended data: Fishery independent surveys, fish.
#' @param X add text here
#' @param metric add text here
#' @param gp add text here
#' @param yr add text here
#' @param user.defined add text here
#' @param group add text here
#' @param path add text here
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


communityFultonK <- function(X, metric='ABUNDANCE', LenWt.table = "scotianshelf",
                             user.defined = F, 
                             group=c('FINFISH','SKATES','CLUPEIDS','GROUNDFISH','FLATFISH','GADOIDS','FORAGE',
                                     'LBENTHIVORE','MBENTHIVORE','PISCIVORE','PLANKTIVORE','ZOOPISCIVORE'),
                             years = c(start.year:end.year)) {
  
  #could possibly even remove this. Or change to "FINFISH" or "ALL" lie the other inds
  if(user.defined) {                                    
    X <- X[X$SPECIES %in% group,]                        # subset X to the species of interest
    } else { X <- speciesgroups(X = X, group = group)}

  X <- X[-which(X$FLEN == -99), ]                        # remove rows that do not contain length data
  
  if (LenWt.table == "scotianshelf") {                   # for Scotian Shelf ecosystem, import stored IndiSeas data
    load("R/sysdata.rda/scotianshelf_lenwt.rda")
  }
    
  uI = unique(X$ID)                                      # extract the spatial scale ID's
  ind <- NULL                                            # initialize dataframe for storing indicator values
  
  for (j in 1:length(uI)){                               # loop over all spatal scales
    
    X.j = X[X$ID == uI[j], ]                             # subset biomass and abundance data to spatial scale j
    len_wgt.j = len_wgt[len_wgt$ID == uI[j], ]           # subset length-weight data to spatial scale j
    
    for (i in 1:length(years)){                          # loop over each year
      
      year.i = years[i]                                  # set years.i to current year  
      X.ij = X.j[X.j$YEAR == year.i, ]                   # subset biomass and abundance data to include only current year
      len_wgt.ij = len_wgt.j[len_wgt.j$YEAR == year.i, ] # subset length-weight data to include only current year
    
      W <- aggregate(FWT ~ FLEN + SPECIES + ID, data = len_wgt.ij, FUN = mean) # fish weights by length and species
      
      if(any(unique(X.ij$SPECIES) %in% unique(W$SPECIES)))  {                  # if there are the same species in Y (biomass) and W (weight). . . 
        Z <- merge(X.ij, W, by = c('ID', 'SPECIES','FLEN'), all.y = T)         # merge them
        Z <- merge(Z, aggregate(ABUNDANCE~ID, data = Z, FUN = sum), by = 'ID') # add a column of total abundance (same for each row)
        
        Z$K <- Z$FWT / Z$FLEN^3*100                                               # calculate K for each species
        ind.i <- aggregate(Z$K*Z$ABUNDANCE.x/Z$ABUNDANCE.y ~ ID,data=Z,FUN=sum)   # calculate Fulton's condition index
    
        ind.i = data.frame(uI[j], year.i, ind.i[,2])          # create a dataframe with spatial scale ID, year, and indicator value
        ind = rbind(ind, ind.i)                               # bind ind.i to ind dataframe
      }
    }
    }
  names(ind) = c("ID", "YEAR", "CommunityCondition")    # name the ind dataframe
  ind                                                   # return ind 
}
