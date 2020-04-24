#'@title Calculates the Large Species Indicator
#'@description This function calculates the Large Species Indicator (LSI) for
#'  \eqn{j} areas and \eqn{i} years.
#'@details Large Species Indicator (LSI): \deqn{LSI = \Sigma B_i(L_{max} > 85
#'  cm)/\Sigma B_i} where \eqn{B_i} is biomass of individual species, \eqn{i},
#'  and \eqn{L_{max}} is the maximum asymptotic length in cm (threshold default
#'  here is 85 cm; Shin et al., 2010).
#'@inheritParams biomassPerTL
#'@inheritParams shannon
#'@param group  character string indicating which species to include in the
#'  indicator calculation. If \code{group = "ALL"}, all species will be
#'  included; otherwise, \code{group} should match a column name in
#'  \code{species.table}.
#'@param maxlength.table A dataframe with columns \code{SPECIES} and
#'  \code{MAXLENGTH}, the maximum recorded length of the corresponding species.
#'  Entries in the \code{SPECIES} column should be the unique values of species
#'  codes in \code{X} (or a subset thereof). Other columns in
#'  \code{maxlength.table} are ignored.
#'@param max.length The threshold for large species (cm). Default is
#'  \code{max.length = 85} (i.e., large species are those with \code{MAXLENGTH}
#'  >= 85 cm).
#'@return Returns a dataframe with 3 columns. \code{ID}, \code{YEAR}, and
#'  \code{LargeSpeciesIndicator}.
#'
#'  If there are no observations of large species or no observations in \code{X}
#'  for spatial scale \eqn{j} in year \eqn{i}, indicator value is assigned
#'  \code{NA}.
#'@family ecosystem structure and function indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Shephard S, Fung T, Houle JE, Farnsworth KD, Reid DG, Rossberg AG. 2012.
#'  Size-selective fishing drives species composition in the Celtic Sea. ICES J
#'  Mar Sci 69:223-234
#'
#'  Shin, YJ, Shannon LJ, Bundy A, Coll M, Aydin K, Bez N, Blanchard JL, Borges,
#'  MF, Diallo I, Diaz E, Heymans JJ, Hill L, Johannesen E, Jouffre D, Kifani S,
#'  Labrosse P, Link JS, Mackinson S, Masski H, Möllmann C, Neira S, Ojaveer H,
#'  Abdallahi KM, Perry I, Thiao D, Yemane D, and Cury PM. 2010. Using
#'  indicators for evaluating, comparing and communicating the ecological status
#'  of exploited marine ecosystems. Part 2: Setting the scene. ICES Journal of
#'  Marine Science, 67: 692-716
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@examples
#'data(X)
#'data(species.table)
#'largeSpeciesIndicator(X, group = "ALL", species.table = species.table,
#'    maxlength.table = species.info, max.length=85,  metric = "BIOMASS", years = c(2014:2019))
#'@export


largeSpeciesIndicator <- function(X, group, species.table = NULL,
                                  maxlength.table, max.length = 85,  metric = "BIOMASS", years) {
  
  X <- speciesGroups(X = X, group = group, species.table = species.table) # subset X to the species of interest
  largespecies <- maxlength.table$SPECIES[maxlength.table$MAXLENGTH > max.length]
 
  uI = unique(X$ID)                   # extract the spatial scale ID's
  ind <- NULL                         # initialize dataframe for storing indicator values
  
  for (j in 1:length(uI)){            # loop over all spatal scales
    
    X.j = X[X$ID == uI[j], ]          # subset data to spatial scale j
    
    for (i in 1:length(years)){       # loop over all years
      
      year.i = years[i]                  # set year.i to year i
      X.ij = X.j[X.j$YEAR == year.i, ]   # subset data to year i
      
      if(nrow(X.ij) > 0){
        A.i <- sum(X.ij[X.ij$SPECIES %in% largespecies, metric])  # sum of biomass of all large species
        B.i <- sum(X.ij[, metric])                                # sum of biomass of all species
        ind.i = A.i/B.i                                           # fraction of large species in community (by biomass)
      } else ind.i <- NA
      
      ind.i = data.frame(uI[j], year.i, ind.i)                  # create a dataframe with spatial scale ID, year, and indicator value
      ind = rbind(ind, ind.i)                                   # bind ind.i to ind dataframe
    }
  }
  
  names(ind) = c("ID", "YEAR", "LargeSpeciesIndicator")    # name the ind dataframe
  ind <- ind[order(ind$ID), ]                           # order by ID to be consistent with other functions
  inx <- which(ind$LargeSpeciesIndicator == 0)             # index of where LargeSpeciesIndicator is 0       
  ind$LargeSpeciesIndicator[inx] <- NA                     # set values of 0 to NA
  
  
  ind                                                      # return ind
}
  

