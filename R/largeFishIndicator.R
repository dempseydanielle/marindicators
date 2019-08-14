#'@title Calculates the Large Fish Indicator (Greenstreet and Rogers, 2006)
#'@description This function calculates the Large Fish Indicator (LFI) for
#'  \eqn{j} areas and \eqn{i} years.
#'@details Large Fish Indicator (LFI): \deqn{LFI = \Sigma B_m(L >50 cm)/\Sigma
#'  B_m} \eqn{B_m} is biomass of individuals in a body size class centred at
#'  mass m, and \eqn{L} is the length (cm) of an individual. This indicator
#'  describes the proportion (by weight) of the fish community that is larger
#'  than some length threshold (default here is 35 cm, i.e., the proportion of
#'  biomass occupying the top predator trophic level).
#'
#'  Recommended data: Fishery independent survey data or model output; fish.
#'@inheritParams resourcePotential
#'@param X A dataframe of fishery independent survey data with columns "YEAR",
#'  "ID", "SPECIES", "LENGTH", and "ABUNDANCE". "YEAR" indicates the year the
#'  observation was recorded, "ID" is an area code indicating where the
#'  observation was recorded, and "SPECIES" is a numeric code indicating the
#'  species sampled. "LENGTH" is the length class (cm) and "ABUNDANCE" is the
#'  corresponding abundance at length (stratified and corrected for catchability
#'  as required). Species for which there are no length data should be assigned
#'  LENGTH = -99. These observations are removed by the function.
#'@param large.fish threshold for large fish (cm). Default is 35 cm (i.e., large
#'  fish are those with X$LENGTH >= 35 cm)
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
#'  Greenstreet SPR and Rogers SI. 2006. Indicators of the health of the fish
#'  community of the North Sea: identifying reference levels for an Ecosystem
#'  Approach to Management. ICES J. Mar. Sci., 63: 573–593.
#'
#'  ICES. 2006. Report of the Working Group on Ecosystem Effects of Fishing
#'  Activities. ICES Document CM 2006/ACE: 05. 174 pp.
#'@author Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca}, Catalina
#'  Gomez, Alida Bundy
#'@export

largeFishIndicator <- function(X, group, species.table = NULL, 
                               metric = "BIOMASS", large.fish = 35, years) {
  
  X <- speciesGroups(X = X, group = group, species.table = species.table) # subset X to the species of interest
  X <- X[-which(X$LENGTH == -99), ]     # remove rows that do not contain length data
  
  uI = unique(X$ID)                   # extract the spatial scale ID's
  ind <- NULL                         # initialize dataframe for storing indicator values
  
  for (j in 1:length(uI)){            # loop over all spatal scales
    
    X.j = X[X$ID == uI[j], ]          # subset data to spatial scale j
  
    for (i in 1:length(years)) {      # loop over all years
      
      year.i = years[i]                  # set year.i to year i
      X.ij <- X.j[X.j$YEAR == year.i, ]  # subset data to year i
      
      if(nrow(X.ij) > 0){
        LF <- X.ij$LENGTH >= large.fish                     # returns TRUE when fish length is >= large.fish   
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

