#'@title Calculates Margalef's Species Richness
#'@description This function calculates Margalef's Species Richness for \eqn{j}
#'  areas and \eqn{i} years.
#'@details Margalef's Species Richness: \deqn{S_{Marg} = (S_y - 1)/log(F_y)}
#'  \eqn{S_y} is the count of the number of species recorded in all trawl
#'  catches collected in year \eqn{y}. \eqn{F} is the total count of all
#'  individuals caught in year \eqn{y} (Margalef, 1958).
#'@inheritParams shannon
#'@inheritParams resourcePotential
#'@return Returns a dataframe with columns \code{ID} and \code{YEAR}, and a
#'  column \code{MargalefRichness_group} for each entry in \code{groups}.
#'
#'  If there is no data for spatial scale \eqn{j} in year \eqn{i}, indicator
#'  values is assigned \code{NA}.
#'@family biodiversity indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Margalef R. 1958. Information theory in ecology. General Systems 3, 36–71.
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@examples
#'data(X)
#'margalef(X, groups = c("ALL", "GROUNDFISH"), metric= "ABUNDANCE", years = c(2014:2019))
#'@export

margalef <- function(X, groups, species.table = NULL, metric = "ABUNDANCE", years)  {
  
  for(k in 1:length(groups)){                                                   # loop over species groups
    
    X.k <- speciesGroups(X = X, group = groups[k], species.table = species.table) # subset X to the species of interest
    
    S <- speciesRichness(X = X, groups = groups[k], species.table = species.table, 
                         metric = metric, years = years)                        # calculate species richness for each year
    
    uI = unique(X$ID)                   # extract the spatial scale ID's
    ind.k <- NULL                         # initialize dataframe for storing indicator values
    
    for (j in 1:length(uI)){            # loop over all spatal scales
      
      X.j = X.k[X.k$ID == uI[j], ]          # subset data to spatial scale j
      S.j = S[S$ID == uI[j], ]
      
      for(i in 1:length(years)){        # loop over all years
        
        year.i = years[i]                          # set years.i to current year
        X.ij = X.j[X.j$YEAR == year.i, metric]     # subset data to include only current year
        
        logF.i = log(sum(X.ij))                       # calculate the log of the sum of metric over all species
        ind.i = (S.j$SpeciesRichness[i] - 1)/logF.i   # calculate Margalef species richness
        
        ind.i = data.frame(uI[j], year.i, ind.i)          # create a dataframe with spatial scale ID, year, and indicator value
        ind.k = rbind(ind.k, ind.i)                           # bind ind.i to ind dataframe
      }
    }  
    
    ind.name <- paste("MargalefRichness_", groups[k], sep = "")            # name indicator: MargalefRichness_group
    names(ind.k) = c("ID", "YEAR", ind.name)                               # name the ind dataframe
    ind.k <- ind.k[order(ind.k$ID), ] 
    
    if(k == 1) ind = ind.k
    
    ind <- merge(ind, ind.k)
  }
  
  ind
}


