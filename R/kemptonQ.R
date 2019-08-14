#'@title Calculates Kempton's Biodiversity Index (Kempton and Taylor, 1976)
#'@description This function calculates Kempton's Biodiversity index (Q) for
#'  \eqn{j} areas and \eqn{i} years.
#'@details Kempton's Biodiversity Index \eqn{(Q)} is a relative index of biomass
#'  diversity calculated from the Kempton's Q75 index developed for expressing
#'  species diversity. This index includes those species or functional groups
#'  with a trophic level of three or higher. \deqn{Q = S*(p_2 -
#'  p_1)*log(R_2/R_1)} \eqn{S} is the total number of species or functional
#'  groups, \eqn{p_1} and  \eqn{p_2} are the lower and upper percentiles of
#'  interest and \eqn{R_1} and \eqn{R_2} are the corresponding lower and upper
#'  quartiles of the species abundance distribution. \eqn{p_1} and \eqn{p_2} are
#'  defaulted to 0.25 and 0.75, respectively.
#'
#'  Recommended data: Fishery independent surveys or model output; fish and
#'  invertebrates
#' @inheritParams shannon
#'@param TL.table table with columns "SPECIES" and "TL", where the "SPECIES"
#'  codes match those in X, and "TL" is the corresponding trophic level.
#'@param percentiles percentiles used to determine R1 and R2. Default here is
#'  percentiles = c(0.25, 0.75).
#'@param minTL minimum trophic level for species included in the calculation.
#'  Default is minTL = 3.
#'@return Returns a dataframe with 3 columns: "ID", YEAR", and "KemptonQ".
#'
#'  If there is no data for spatial scale \eqn{j} in year \eqn{i}, indicator
#'  values is assigned NA.
#'@family biodiversity indicators
#'@references Ainsworth, C, Pitcher, T. 2006. Modifying Kempton's species
#'  diversity index for use with ecosystem simulation models. Ecological
#'  Indicators. 6. 623-630. 10.1016/j.ecolind.2005.08.024.
#'
#'  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the selection and
#'  evaluation of ecological indicators. Can. Tech. Rep. Fish. Aquat. Sci. 3232:
#'  xii + 212 p.
#'
#'  Kempton R, Taylor L. 1976. Models and statistics for species diversity.
#'  Nature 262:818-820
#'@author  Danielle Dempsey, Alida Bundy, Adam Cook, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#'@export

kemptonQ<- function(X, TL.table, percentiles = c(.25, 0.75), minTL = 3, 
                    group, species.table = NULL, metric = "ABUNDANCE", years) {
  
  X <- speciesGroups(X = X, group = group, species.table = species.table) # subset X to the species of interest
  X <- merge(X, TL.table, by = 'SPECIES')     # Add trophic level data to RV survey data
  X <- X[X$TL > minTL, ]
  
  uI = unique(X$ID)                   # extract the spatial scale ID's
  ind <- NULL                         # initialize dataframe for storing indicator values
  
  for (j in 1:length(uI)){            # loop over all spatal scales
    
    X.j = X[X$ID == uI[j], ]          # subset data to spatial scale j
    
    for (i in 1:length(years)){                     # loop over each year
      
      year.i = years[i]                             # set years.i to current year  
      X.ij = X.j[X.j$YEAR == year.i, ]              # subset data to include only current year
  
      Y <- X.ij[order(X.ij[metric]),metric]        # set Y to metric ORDERED FROM SMALLEST TO LARGEST
      S <- length(Y)                               # number of species recorded (simpler than speciesRichness function)
      
      if(S>2) {
        w <- c(round(S*percentiles[1], 0), round(S*percentiles[2], 0)) # index of where percentile value is in Y
                                                                       # this works becuase S = length(Y)
        if(w[1]==0) w[1]<-1                                            # can't have index of 0, so if small number, set to 1
        ind.i <- S*(percentiles[2]-percentiles[1])/log(Y[w[2]]/Y[w[1]]) # calculate Kemptons' Q
      } else {
          ind.i <- NA
        } 
      
      ind.i = data.frame(uI[j], year.i, ind.i)     # create a dataframe with spatial scale ID, year, and indicator value
      ind = rbind(ind, ind.i)                      # bind ind.i to ind dataframe
      
    }
  }
  
  names(ind) = c("ID", "YEAR", "KemptonQ")    # name the ind dataframe
  ind <- ind[order(ind$ID), ]                 # order by ID to be consistent with other functions
  ind                                         # return vector of indicator values for years c(start.year:end.year) 
  
}
