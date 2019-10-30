#'@title Calculates the Mean Maximum Length of fish in the community
#'@description This function calculates the Mean Maximum Length of fish in the
#'  community weighted by biomass or abundance for \eqn{j} areas and \eqn{i}
#'  years.
#'@details Mean Maximum Length: \deqn{Mean Maximum Length = \Sigma
#'  (L_{max,i}*M_i)/\Sigma M_i} where \eqn{L_{max,i}} is the maximum asymptotic
#'  length (cm) of species \eqn{i}, and \eqn{M_i} is biomass or abundance of
#'  species \eqn{i} (excluding invertebrates; Shin et al., 2005).
#'@inheritParams largeSpeciesIndicator
#'@return Returns a dataframe with 3 columns. \code{ID}, \code{YEAR}, and
#'  \code{MMLength_metric}.
#'
#'  If there is no data for spatial scale \eqn{j} in year \eqn{i}, indicator
#'  value is assigned \code{NA}.
#'@family stability and resistance indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Shin YJ, Rochet MJ, Jennings S, Field JG, Gislason H. 2005. Using size-based
#'  indicators to evaluate the ecosystem effects of fishing. In: ICES Journal of
#'  Marine Science. p 384-396
#'@author  Danielle Dempsey \email{Danielle.Dempsey@@dfo-mpo.gc.ca}, Adam Cook,
#'  Catalina Gomez, Alida Bundy
#'@examples
#'# Compile data
#'data(X)
#'data(species.info)
#'data(species.table)
#'
#'# Calculate indicators
#'meanMaxLength(X, group = "FINFISH", species.table = species.table, 
#'    maxlength.table = species.info, metric = "BIOMASS", years = c(2014:2019))
#'meanMaxLength(X, group = "FINFISH", species.table = species.table, 
#'    maxlength.table = species.info, metric = "ABUNDANCE", years = c(2014:2019))
#'@export

meanMaxLength <- function(X, group, species.table = NULL, maxlength.table, metric, years) {

  maxlength.table <- data.frame(na.omit(maxlength.table[, c("SPECIES", "MAXLENGTH")]))
  X <- speciesGroups(X = X, group = group, species.table = species.table) # subset X to the species of interest
  
  X <- merge(X, maxlength.table, by = "SPECIES")

  uI = unique(X$ID)                   # extract the spatial scale ID's
  ind <- NULL                         # initialize dataframe for storing indicator values
  
  for (j in 1:length(uI)){            # loop over all spatal scales
    
    X.j = X[X$ID == uI[j], ]          # subset data to spatial scale j
    
    for (i in 1:length(years)){                     # loop over each year
      
      year.i = years[i]                             # set years.i to current year  
      X.ij = X.j[X.j$YEAR == year.i, ]              # subset data to include only current year
      
      if(nrow(X.ij) > 0){                           # set ind.i to NA if there are no observations in X.ij 
        ind.i <- sum(X.ij[metric]*X.ij["MAXLENGTH"])/sum(X.ij[metric])	 # make sure this does what it should!
      } else ind.i <- NA         
      
      ind.i = data.frame(uI[j], year.i, ind.i)     # create a dataframe with spatial scale ID, year, and indicator value
      ind = rbind(ind, ind.i)                      # bind ind.i to ind dataframe
    }
  }
  
  ind.name = paste("MMLength_", metric, sep = "")
  names(ind) = c("ID", "YEAR", ind.name)          # name the ind dataframe
  ind <- ind[order(ind$ID), ]                     # order by ID to be consistent with other functions
  ind                                             # return vector of indicator values for years c(start.year:end.year) 
  
}



