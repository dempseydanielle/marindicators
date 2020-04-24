#'@title Calculates the Mean Length of the Community weighted by biomass or
#'  abundance
#'@description This function calculates the Mean Length of the Community
#'  weighted by biomass or abundance for \eqn{j} areas and \eqn{i} years.
#'@details \deqn{MeanLength = \Sigma(Length_m * metric_i)/\Sigma metric_j} where
#'  \eqn{Length_m} is the length (cm) of an individual in size class \eqn{m},
#'  \eqn{metric_i} is the biomass or abundance of species \eqn{i} and
#'  \eqn{metric_j} is the total biomass or abundance of the community (Shin et
#'  al., 2010).
#'@inheritParams resourcePotential
#'@inheritParams largeFishIndicator
#'@param metric A character string indicating which column in \code{X} to use to
#'  calculate indicator.
#'@return Returns a dataframe with 3 columns. \code{ID}, \code{YEAR}, and
#'  \code{MeanLength_metric}.
#'
#'  If there is no data for spatial scale \eqn{j} in year \eqn{i}, indicator
#'  value is assigned \code{NA}.
#'@importFrom stats aggregate
#'@family structure and functioning indicators
#'@references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
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
#'data(X_length)
#'# Weighted by abundance
#'meanLengthCommunity(X_length, metric = "ABUNDANCE", years = c(2014:2019))
#'# Weighted by biomass
#'meanLengthCommunity(X_length, metric = "BIOMASS", years = c(2014:2019))
#'@export


meanLengthCommunity <- function(X_length, metric, years) {
	
  X <- X_length
  
  inx99 <- which(X$LENGTH == -99)                          # index of rows that do not contain length data               
  if(length(inx99 > 0)) X <- X[-which(X$LENGTH == -99), ]  # remove rows that do not contain length data
  
   uI = unique(X_length$ID)                            # extract the spatial scale ID's
   ind <- NULL                                         # initialize dataframe for storing indicator values
  
  for (j in 1:length(uI)){            # loop over all spatal scales
    
    X.j = X[X$ID == uI[j], ]          # subset data to spatial scale j
    
    for (i in 1:length(years)) {      # loop over all years
      
      year.i = years[i]                  # set year.i to year i
      X.ij <- X.j[X.j$YEAR == year.i, ]  # subset data to year i
      
      if(nrow(X.ij) > 0){
        ind.i <- sum(X.ij[, "LENGTH"]* X.ij[, metric])/sum(X.ij[, metric])
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
 
