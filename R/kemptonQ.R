#'@title Calculates Kempton's Biodiversity Index
#'@description This function takes a dataframe of fisheries independent survey
#'  data and calculates Kempton's Biodiversity index (Q) for \eqn{j} areas and
#'  \eqn{i} years.
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
#'  **Recommended data: Fishery independent surveys, fish and invertebrates
#'@param X dataframe of fishery independent survey data with columns "YEAR",
#'  "ID", "SPECIES", and "BIOMASS" and/or "ABUNDANCE". "ID" is an area code
#'  designating where the observation was recorded. "SPECIES" is a numeric code
#'  indicating the species sampled.
#'@param TL.table table with columns "SPECIES" and "TL", where the "SPECIES"
#'  codes match those in X, and "TL" is the corresponding trophic level.
#'@param percentiles percentiles used to determine R1 and R2. Default here is
#'  percentiles = c(0.25, 0.75).
#'@param minTL minimum trophic level for species included in the calculation.
#'  Default is minTL = 3.
#'@param  group character string indicating which species to include, either
#'  "ALL" or "FINFISH". Note that this subsetting is based on the Fisheries and
#'  Oceans Canada species codes for the Scotian Shelf. For other regions it may
#'  be prudent to subsetdata to species groups of interest prior to using the
#'  function and then choose group = "ALL". Type ?speciesgroups for more
#'  information.
#'@param metric character string indicating whether to use "BIOMASS" or
#'  "ABUNDANCE" to calculate the indicator.
#'@param years vector of years for which to calculate indicator
#'@return Returns a dataframe with 3 columns: "ID", YEAR", and "KemptonQ"
#'@family biodiversity indicators
#'@references (Alida - this ref was not in tech report. Could be this one or the
#'one below) Ainsworth, C, Pitcher, T (2006) Modifying Kempton's species
#'diversity index for use with ecosystem simulation models. Ecological
#'Indicators. 6. 623-630. 10.1016/j.ecolind.2005.08.024.
#'
#'Ainsworth, C, Varkey, D, Pitcher, TJ  (2006)  Preliminary ecosystem simulation
#'models for the Bird’s Head Seascape, Papua.  Mid-term narrative technical
#'report.  Birds Head Seascape Ecosystem-Based Management Project. University of
#'British Columbia Fisheries Centre.  December, 2006, 274 pp.
#'
#'Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the selection and
#'evaluation of ecological indicators. Can. Tech. Rep. Fish. Aquat. Sci. 3232:
#'xii + 212 p.
#'
#'Kempton R, Taylor L (1976) Models and statistics for species diversity. Nature
#'262:818-820
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#'@export

kemptonQ<- function(X, TL.table, percentiles = c(.25, 0.75), 
                    minTL = 3, group = "ALL",
                    metric = c('BIOMASS','ABUNDANCE'), years) {
  
  if(group != "ALL") X <- speciesgroups(X = X, group = group) # subset X to the species of interest
  X <- merge(X, TL.table, by = 'SPECIES')     # Add trophic level data to RV survey data
  X <- X[X$TL > minTL, ]
  
  uI = unique(X$ID)                   # extract the spatial scale ID's
  ind <- NULL                         # initialize dataframe for storing indicator values
  
  for (j in 1:length(uI)){            # loop over all spatal scales
    
    X.j = X[X$ID == uI[j], ]          # subset data to spatial scale j
    
    for (i in 1:length(years)){                     # loop over each year
      
      year.i = years[i]                             # set years.i to current year  
      X.ij = X.j[X.j$YEAR == year.i, ]              # subset data to include only current year
  
      #S <- speciesrichness(X = X.ij, group = group, metric = metric, years = year.i) # calculate species richness
      #S <- S[,3]                                                                     # extract number of species for spatila scale j and area i
      
      Y <- X.ij[order(X.ij[metric]),metric]        # set Y to metric ORDERED FROM SMALLEST TO LARGEST
      S <- length(Y)                               # number of species recorded (simpler than speciesrichness function)
      
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
  ind                                                # return vector of indicator values for years c(start.year:end.year) 
  
}
