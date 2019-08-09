#'@title Calculates the mean length of the community weighted by biomass or
#'  abundance (Shin et al., 2010)
#'@description This function calculates the mean length of the community
#'  weighted by biomass or abundance for \eqn{j} areas and \eqn{i} years.
#'@details \deqn{MeanLength = \Sigma(Length_m * metric_i)/\Sigma metric_j} where
#'  \eqn{Length_m} is the length (cm) of an individual in size class \eqn{m},
#'  \eqn{B_i} is the biomass or abundance of species \eqn{i} and \eqn{metric_j}
#'  is the total biomass or abundance of the community.
#'
#'  Recommended data: Fishery independent survey data or model output; fish and
#'  invertebrates.
#'@inheritParams resourcePotential
#'@param X A dataframe of fishery independent survey data with columns "YEAR",
#'  "ID", "SPECIES", "LENGTH", "ABUNDANCE", and/or "BIOMASS". "YEAR" indicates
#'  the year the observation was recorded, "ID" is an area code indicating where
#'  the observation was recorded, and "SPECIES" is a numeric code indicating the
#'  species sampled. "LENGTH" is the length class (cm) and "ABUNDANCE" and
#'  "BIOMASS" are the corresponding abundance and biomass at length (stratified
#'  and corrected for catchability as required). Species for which there are no
#'  length data should be assigned LENGTH = -99. These observations are removed
#'  by the function.
#'@return Returns a dataframe with 3 columns. "ID", "YEAR", and
#'  "MeanLength_metric".
#'
#'  If there is no data for spatial scale \eqn{j} in year \eqn{i}, indicator
#'  value is assigned NA.
#'@importFrom stats aggregate
#'@family structure and functioning indicators
#'@references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Shin, YJ, Shannon LJ, Bundy A, Coll M, Aydin K, Bez N, Blanchard JL, Borges,
#'  MF, Diallo I, Diaz E, Heymans JJ, Hill L, Johannesen E, Jouffre D, Kifani S,
#'  Labrosse P, Link JS, Mackinson S, Masski H, Möllmann C, Neira S, Ojaveer H,
#'  Ould Mohammed Abdallahi ., Perry I, Thiao D, Yemane D, and Cury PM. 2010.
#'  Using indicators for evaluating, comparing and communicating the ecological
#'  status of exploited marine ecosystems. Part 2: Setting the scene. ICES
#'  Journal of Marine Science, 67: 692-716
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@export


meanLengthCommunity <- function(X, metric, years) {
	
  uI = unique(X$ID)                   # extract the spatial scale ID's
  X <- X[-which(X$FLEN == -99), ]     # remove rows that do not contain length data
  ind <- NULL                         # initialize dataframe for storing indicator values
  
  for (j in 1:length(uI)){            # loop over all spatal scales
    
    X.j = X[X$ID == uI[j], ]          # subset data to spatial scale j
    
    for (i in 1:length(years)) {      # loop over all years
      
      year.i = years[i]                  # set year.i to year i
      X.ij <- X.j[X.j$YEAR == year.i, ]  # subset data to year i
      
      if(nrow(X.ij) > 0){
        ind.i <- sum(X.ij[, 'FLEN']* X.ij[, metric])/sum(X.ij[, metric])
      } else ind.i <- NA
      
      ind.i = data.frame(uI[j], year.i, ind.i)          # create a dataframe with spatial scale ID, year, and indicator value
      ind = rbind(ind, ind.i)                           # bind ind.i to ind dataframe
    }
    }
  
  ind.name <- paste("MeanLength", metric, sep = "")
  names(ind) <- c("ID", "YEAR", ind.name)               # name the ind dataframe
  ind <- ind[order(ind$ID), ]                           # order by ID to be consistent with other functions
  ind                                                   # return ind 
}
 
