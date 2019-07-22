#'@title Calculates the Large Species Indicator
#'@description This function takes a dataframe of length-based fisheries
#'  independent survey data and calculates the Large Species Indicator (LSI) for
#'  \eqn{j} areas and \eqn{i} years.
#'@details Large Species Indicator (LSI): \deqn{LSI = \Sigma B_i(L_{max} >85
#'  cm)/\Sigma B_i} where \eqn{B_i} is biomass of individual species, \eqn{i},
#'  and \eqn{L_{max}} is the maximum asymptotic length (cm; here the default is
#'  85 cm).
#'
#'  Recommended data: Fishery independent surveys, fish.
#'@param X dataframe of fishery independent survey data with columns "YEAR",
#'  "ID", "SPECIES", and "BIOMASS" and/or "ABUNDANCE". "ID" is an area code
#'  designating where the observation was recorded. "SPECIES" is a numeric code
#'  indicating the species sampled.
#'@param lmax.table table with 2 columns: "SPECIES" and "MAXLEN99", the maximum
#'  recorded length of the corresponding species.
#'@param lmax threshold for large fish (cm). Default is 85 cm (i.e., large
#'  species are those with MAXLEN99 >= 85 cm)
#'@param metric character string indicating whether to use "BIOMASS" or
#'  "ABUNDANCE" to calculate indicator.
#'@param years vector of years for which to calculate indicator.
#'@return Returns a dataframe with 3 columns. "ID", "YEAR", and
#'  "LargeFishIndicator"
#'@family ecosystem structure and function indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Shephard S, Fung T, Houle JE, Farnsworth KD, Reid DG, Rossberg AG (2012)
#'  Size-selective fishing drives species composition in the Celtic Sea. ICES J
#'  Mar Sci 69:223-234
#'
#'  Houle JE, Farnsworth KD, Rossberg AG, Reid DG (2012) Assessing the
#'  sensitivity and specificity of fish community indicators to management
#'  action. Can J Fish Aquat Sci 69:1065-1079
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#'@export


largeSpeciesIndicator <- function(X, lmax.table, lmax = 85,  metric = 'BIOMASS',
                                  years) {
  
  largespecies <- lmax.table$SPECIES[lmax.table$MAXLEN99 > lmax]
 
  uI = unique(X$ID)                   # extract the spatial scale ID's
  ind <- NULL                         # initialize dataframe for storing indicator values
  
  for (j in 1:length(uI)){            # loop over all spatal scales
    
    X.j = X[X$ID == uI[j], ]          # subset data to spatial scale j
    
    for (i in 1:length(years)){       # loop over all years
      
      year.i = years[i]                  # set year.i to year i
      X.ij = X.j[X.j$YEAR == year.i, ]   # subset data to year i
      
      A.i <- sum(X.ij[X.ij$SPECIES %in% largespecies, metric])  # sum of biomass of all large species
      B.i <- sum(X.ij[, metric])                                # sum of biomass of all species
      ind.i = A.i/B.i                                           # fraction of large species in community (by biomass)
      
      ind.i = data.frame(uI[j], year.i, ind.i)                  # create a dataframe with spatial scale ID, year, and indicator value
      ind = rbind(ind, ind.i)                                   # bind ind.i to ind dataframe
    }
  }
  
  names(ind) = c("ID", "YEAR", "LargeSpeciesIndicator")    # name the ind dataframe
  ind                                                      # return ind
}
  

