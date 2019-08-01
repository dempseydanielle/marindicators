#'@title Calculates the mean maximum length of fish in the community
#'@description This function calculates the mean maximum length (MML) of fish in
#'  the community weighted by biomass or abundance for \eqn{j} areas and \eqn{i}
#'  years.
#'@details Mean Maximum Length (MML): \deqn{MML = \Sigma (L_{max,i}*M_i)/\Sigma
#'  M_i} where \eqn{L_{max,i}} is the maximum asymptotic length (cm) of species
#'  \eqn{i}, and \eqn{M_i} is biomass or abundance of species \eqn{i} (excluding
#'  invertebrates).
#'
#'  Recommended data: Fishery independent surveys, fish.
#'@param X X dataframe of fishery independent survey data with columns "YEAR",
#'  "ID", "SPECIES", "FLEN", and "BIOMASS" and/or "ABUNDANCE". "ID" is an area
#'  code designating where the observation was recorded (a string). "SPECIES" is
#'  a numeric code indicating the species sampled. "FLEN" is the length class
#'  (cm) and "BIOMASS" and "ABUNDANCE" are the corresponding biomass and
#'  abundance at length. Species for which there are no length data should be
#'  assigned FLEN = -99. These observations are removed by the function.
#'@param lmax.table table with 2 columns: "SPECIES" and "MAXLEN99", the maximum
#'  recorded length of the corresponding species.
#'@param metric character string indicating whether to use "BIOMASS" or
#'  "ABUNDANCE" to calculate indicator.
#'@param years vector of years for which to calculate indicator.
#'@return Returns a dataframe with 3 columns. "ID", "YEAR", and
#'  "MMlength_metric".
#'
#'  If there is no data for spatial scale \eqn{j} in year \eqn{i}, indicator
#'  value is assigned NA.
#'@family stability and resistance indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Houle JE, Farnsworth KD, Rossberg AG, Reid DG (2012) Assessing the
#'  sensitivity and specificity of fish community indicators to management
#'  action. Can J Fish Aquat Sci 69:1065-1079
#'
#'  Shin YJ, Rochet MJ, Jennings S, Field JG, Gislason H (2005) Using size-based
#'  indicators to evaluate the ecosystem effects of fishing. In: ICES Journal of
#'  Marine Science. p 384-396
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}
#'@export

meanMaxL <- function(X, lmax.table, metric=c('BIOMASS','ABUNDANCE'), years) {

  X <- merge(X, lmax.table, by = 'SPECIES')
  X <- X[-which(X$FLEN == -99), ]     # remove rows that do not contain length data

  uI = unique(X$ID)                   # extract the spatial scale ID's
  ind <- NULL                         # initialize dataframe for storing indicator values
  
  for (j in 1:length(uI)){            # loop over all spatal scales
    
    X.j = X[X$ID == uI[j], ]          # subset data to spatial scale j
    
    for (i in 1:length(years)){                     # loop over each year
      
      year.i = years[i]                             # set years.i to current year  
      X.ij = X.j[X.j$YEAR == year.i, ]              # subset data to include only current year
      
      if(nrow(X.ij) > 0){                           # set ind.i to NA if there are no observations in X.ij 
      ind.i <- sum(X.ij[metric]*X.ij['MAXLEN99'])/sum(X.ij[metric])	 # make sure this does what it should!
      } else ind.i <- NA         
      
      ind.i = data.frame(uI[j], year.i, ind.i)     # create a dataframe with spatial scale ID, year, and indicator value
      ind = rbind(ind, ind.i)                      # bind ind.i to ind dataframe
    }
  }
  
  ind.name = paste("MMLength", metric, sep = "")
  names(ind) = c("ID", "YEAR", ind.name)          # name the ind dataframe
  ind <- ind[order(ind$ID), ]                          # order by ID to be consistent with other functions
  ind                                             # return vector of indicator values for years c(start.year:end.year) 
  
}



