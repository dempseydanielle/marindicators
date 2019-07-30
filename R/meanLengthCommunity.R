#'@title Calculates the mean length of the community weighted by biomass or
#'@description This function calculates the mean length of the community
#'  weighted by biomass or abundance for \eqn{j} areas and \eqn{i} years.
#'@details \deqn{MeanLength = \Sigma(Length_m * metric_i)/\Sigma metric_j} where
#'  \eqn{Length_m} is the length (cm) of an individual in size class \eqn{m},
#'  \eqn{B_i} is the biomass or abundance of species \eqn{i} and \eqn{metric_j}
#'  is the total biomass or abundance of the community.
#'
#'  **Recommended data: Fishery independent surveys, fish and invertebrates.
#'@param X dataframe of fishery independent survey data with columns "YEAR",
#'  "ID", "SPECIES", "FLEN", and "BIOMASS" and/or "ABUNDANCE". "ID" is an area
#'  code designating where the observation was recorded (a string). "SPECIES" is
#'  a numeric code indicating the species sampled. "FLEN" is the length class
#'  (cm) and "BIOMASS" and "ABUNDANCE" are the corresponding biomass and
#'  abundance at length. Species for which there are no length data should be
#'  assigned FLEN = -99. These observations are removed by the function.
#'@param metric character string indicating whether to use "BIOMASS" or
#'  "ABUNDANCE" to calculate indicator.
#'@param years vector of years for which to calculate indicator.
#'@return Returns a dataframe with 3 columns. "ID", "YEAR", and
#'  "MeanLength_metric"
#'@importFrom stats aggregate
#'@family structure and functioning indicators
#'@references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Houle JE, Farnsworth KD, Rossberg AG, Reid DG. 2012. Assessing the
#'  sensitivity and specificity of fish community indicators to management
#'  action. Can J Fish Aquat Sci 69:1065–1079
#'
#'  Shin YJ, Bundy A, Shannon LJ, Simier M, Coll M, Fulton EA, Link JS, Jouffre
#'  D, Ojaveer H, MacKinson S, Heymans JJ, Raid T. 2010. Can simple be useful
#'  and reliable? Using ecological indicators to represent and compare the
#'  states of marine ecosystems. ICES J Mar Sci 67:717–731
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#'@export


meanLengthCommunity <- function(X, metric=c('BIOMASS','ABUNDANCE'), years) {
	
  uI = unique(X$ID)                   # extract the spatial scale ID's
  X <- X[-which(X$FLEN == -99), ]     # remove rows that do not contain length data
  ind <- NULL                         # initialize dataframe for storing indicator values
  
  for (j in 1:length(uI)){            # loop over all spatal scales
    
    X.j = X[X$ID == uI[j], ]          # subset data to spatial scale j
    
    for (i in 1:length(years)) {      # loop over all years
      
      year.i = years[i]                  # set year.i to year i
      X.ij <- X.j[X.j$YEAR == year.i, ]  # subset data to year i
      
      ind.i <- sum(X.ij[, 'FLEN']* X.ij[, metric])/sum(X.ij[, metric])
      
      ind.i = data.frame(uI[j], year.i, ind.i)          # create a dataframe with spatial scale ID, year, and indicator value
      ind = rbind(ind, ind.i)                           # bind ind.i to ind dataframe
    }
    }
  
  ind.name <- paste("MeanLength", metric, sep = "")
  names(ind) <- c("ID", "YEAR", ind.name)                # name the ind dataframe
  ind <- ind[order(ind$ID), ]                          # order by ID to be consistent with other functions
  ind                                                   # return ind 
}
 
