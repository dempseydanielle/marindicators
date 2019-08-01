#'@title Calculates the Large Fish Indicator
#'@description This function takes a dataframe of length-based fisheries
#'  independent survey data and calculates the Large Fish Indicator (LFI) for
#'  \eqn{j} areas and \eqn{i} years.
#'@details Large Fish Indicator (LFI): \deqn{LFI = \Sigma B_m(L >50 cm)/\Sigma
#'  B_m} \eqn{B_m} is biomass of individuals in a body size class centred at
#'  mass m, and \eqn{L} is the length (cm) of an individual. This indicator
#'  describes the proportion (by weight) of the fish community that is larger
#'  than some length threshold (default here is 35 cm, **check this: i.e., the
#'  proportion of biomass occupying the top predator trophic level).
#'
#'  Recommended data: Fishery independent surveys, fish.
#'@param X dataframe of fishery independent survey data with columns "YEAR",
#'  "ID", "SPECIES", "FLEN", and "BIOMASS" and/or "ABUNDANCE". "ID" is an area
#'  code designating where the observation was recorded (a string). "SPECIES" is
#'  a numeric code indicating the species sampled. "FLEN" is the length class
#'  (cm) and "BIOMASS" and "ABUNDANCE" are the corresponding biomass and
#'  abundance at length. Species for which there are no length data should be
#'  assigned FLEN = -99. These observations are removed by the function.
#'@param metric character string indicating whether to use "BIOMASS" or
#'  "ABUNDANCE" to calculate indicator.
#'@param large.fish threshold for large fish (cm). Default is 35 cm (i.e., large
#'  fish are those with X$FLEN >= 35 cm)
#'@param years vector of years for which to calculate indicator.
#'@return Returns a dataframe with 3 columns. "ID", "YEAR", and
#'  "LargeFishIndicator".
#'
#'  If there are no observations of large fish or no observations in X for
#'  spatial scale \eqn{j} in year \eqn{i}, indicator value is assigned NA.
#'@family ecosystem structure and function indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Greenstreet SP, Fraser HM, Rogers SI, Trenkel VM, Simpson SD, Pinnegar JK
#'  (2012) Redundancy in metrics describing the composition, structure, and
#'  functioning of the North Sea demersal fish community. ICES J Mar Sci 69:8-22
#'
#'  Houle JE, Farnsworth KD, Rossberg AG, Reid DG (2012) Assessing the
#'  sensitivity and specificity of fish community indicators to management
#'  action. Can J Fish Aquat Sci 69:1065-1079
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#'@export

largeFishIndicator <- function(X, metric=c('BIOMASS','ABUNDANCE'), 
                               large.fish = 35, years) {
  
  uI = unique(X$ID)                   # extract the spatial scale ID's
  X <- X[-which(X$FLEN == -99), ]     # remove rows that do not contain length data
  ind <- NULL                         # initialize dataframe for storing indicator values
  
  for (j in 1:length(uI)){            # loop over all spatal scales
    
    X.j = X[X$ID == uI[j], ]          # subset data to spatial scale j
  
    for (i in 1:length(years)) {      # loop over all years
      
      year.i = years[i]                  # set year.i to year i
      X.ij <- X.j[X.j$YEAR == year.i, ]  # subset data to year i
      
      if(nrow(X.ij) > 0){
        LF <- X.ij$FLEN >= large.fish                     # returns TRUE when fish length is >= large.fish   
        ind.i <- sum(X.ij[LF, metric])/sum(X.ij[,metric]) # calculate the large fish indicator
      } else ind.i <- NA
      
      ind.i = data.frame(uI[j], year.i, ind.i)          # create a dataframe with spatial scale ID, year, and indicator value
      ind = rbind(ind, ind.i)                           # bind ind.i to ind dataframe
    }
    
  }
  
  names(ind) = c("ID", "YEAR", "LargeFishIndicator")    # name the ind dataframe
  inx <- which(ind$LargeFishIndicator == 0)             # index of where LargeFishIndicator is 0       
  ind$LargeFishIndicator[inx] <- NA                     # set values of 0 to NA
  
  ind <- ind[order(ind$ID), ]                           # order by ID to be consistent with other functions
  ind                                                   # return ind 
}

