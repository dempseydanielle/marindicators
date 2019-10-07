#'@title Calculates Kempton's Biodiversity Index
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
#'  defaulted to 0.25 and 0.75, respectively (Kempton and Taylor, 1976).
#'@inheritParams shannon
#'@inheritParams meanTLCommunity
#'@param percentiles The percentiles used to determine R1 and R2. Default is
#'  \code{percentiles = c(0.25, 0.75)}.
#'@param minTL Minimum trophic level for species included in the calculation.
#'  Default is \code{minTL = 0}.
#'@return Returns a dataframe with 3 columns: \code{ID}, \code{YEAR}, and
#'  \code{KemptonQ_minTL}.
#'
#'  If there is no data for spatial scale \eqn{j} in year \eqn{i}, indicator
#'  values is assigned \code{NA}.
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
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@examples
#'data(X)
#'data(species.info)
#'kemptonQ(X, TL.table = species.info, percentiles = c(.25, 0.75), minTL = 0,
#'    groups = "ALL", metric = "ABUNDANCE", years = c(2014:2019))
#'@export

kemptonQ<- function(X, TL.table, percentiles = c(.25, 0.75), minTL = 0, 
                    groups, species.table = NULL, metric = "ABUNDANCE", years) {
  
  TL.table <- na.omit(TL.table[, c("SPECIES", "TL")])
  
  for(k in 1:length(groups)){  
    X.k <- speciesGroups(X = X, group = groups[k], species.table = species.table) # subset X to the species of interest
    X.k <- merge(X.k, TL.table, by = 'SPECIES')     # Add trophic level data to RV survey data
    X.k <- X.k[X.k$TL > minTL, ]
    
    uI = unique(X$ID)                   # extract the spatial scale ID's
    ind.k <- NULL                         # initialize dataframe for storing indicator values
    
    for (j in 1:length(uI)){            # loop over all spatal scales
      
      X.j = X.k[X.k$ID == uI[j], ]          # subset data to spatial scale j
      
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
        ind.k = rbind(ind.k, ind.i)                      # bind ind.i to ind dataframe
        
      }
    }
    
    ind.name <- paste("KemptonQ_", groups[k], "_", minTL, sep ="")        # name indicator: KemptonQ_group_minTL
    names(ind.k) = c("ID", "YEAR", ind.name)                              # name the ind dataframe
    ind.k <- ind.k[order(ind.k$ID), ]                 # order by ID to be consistent with other functions
    
    if(k == 1) ind = ind.k
    
    ind <- merge(ind, ind.k)
  }
  ind                                         # return vector of indicator values for years c(start.year:end.year) 
  
}
