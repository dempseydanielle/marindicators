#'@title Calculates Shannon's Index of Diversity
#'@description This function calculates Shannon's Index of Diversity for \eqn{j}
#'  areas and \eqn{i} years.
#'@details Shannon's index of diversity (H'): \deqn{H' = -\Sigma p_i ln(p_i)}
#'  \eqn{p_i} is the proportion of the total sample contributed by the i(th)
#'  species and \eqn{S} is the number of species recorded in the sample. This
#'  index is sensitive to the number of species recorded in the sample
#'  (Magurran, 1988).
#'@param X A dataframe of fishery independent survey data or model output with
#'  columns \code{YEAR}, \code{ID}, \code{SPECIES}, and \code{ABUNDANCE}.
#'  \code{YEAR} indicates the year the observation was recorded, \code{ID} is an
#'  area code indicating where the observation was recorded, \code{SPECIES} is a
#'  numeric code indicating the species sampled, and \code{ABUNDANCE} is the
#'  corresponding abundance (stratified and corrected for catchability as
#'  required).
#'@param group A character string indicating which species to include in the
#'  indicator calculation. If \code{group = "ALL"}, all species will be
#'  included; otherwise, \code{group} should match a column name in
#'  \code{species.table}.
#'@param species.table A table with at least one column, where the column name
#'  is the string \code{group}, and the column entries are species codes from
#'  \code{X} indicating which species are included that group.
#'  \code{species.table} may also include columns for other species groups;
#'  these will be ignored. If \code{group = "ALL"}, this table is not required.
#'  Default is \code{species.table = NULL}.
#'@param metric A character string indicating which column in \code{X} to use to
#'  calculate the indicator. Default is \code{metric = "ABUNDANCE"}.
#'@param years A vector of years for which to calculate indicator.
#'@return Returns a dataframe with 3 columns: \code{ID}, \code{YEAR}, and
#'  \code{ShannonDiversity}.
#'
#'  If there is no data for spatial scale \eqn{j} in year \eqn{i}, indicator
#'  values is assigned NA.
#'@family biodiversity indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Greenstreet SP, Rogers SI. 2006. Indicators of the health of the North Sea
#'  fish community: identifying reference levels for an ecosystem approach to
#'  management. ICES J Mar Sci J du Cons 63:573-593
#'
#'  Magurran, AE. 1988. Ecological Diversity and its Measurement. Chapman and
#'  Hall, London. 179 pp.
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@examples
#'data(X)
#'shannon(X, group = "ALL", metric = "ABUNDANCE", years = c(2015:2019))
#'@export


shannon <- function(X, group, species.table = NULL, metric = "ABUNDANCE", years) {
  
  X <- speciesGroups(X = X, group = group, species.table = species.table) # subset X to the species of interest

  uI = unique(X$ID)                   # extract the spatial scale ID's
  ind <- NULL                         # initialize dataframe for storing indicator values
  
  for (j in 1:length(uI)){            # loop over all spatal scales
    
    X.j = X[X$ID == uI[j], ]          # subset data to spatial scale j
  
    for (i in 1:length(years)){                   # loop over all years
      
      year.i = years[i]                           # set years.i to current year  
      X.ij = X.j[X.j$YEAR == year.i, metric]      # subset data to include only current year
      
      if(length(X.ij) > 0){                          # if there are no obs in X.ij,  set ind.i to NA
        p <- X.ij/sum(X.ij)                          # calculate the proportion of each species by metric
        ind.i <- -sum(p*log(p))                      # calculate Shannon's metric of diversity
      } else ind.i <- NA
      
      ind.i = data.frame(uI[j], year.i, ind.i)       # create a dataframe with spatial scale ID, year, and indicator value
      ind = rbind(ind, ind.i)                        # bind ind.i to ind dataframe
    }
  }
  
  names(ind) = c("ID", "YEAR", "ShannonDiversity")    # name the ind dataframe
  ind <- ind[order(ind$ID), ] 
  ind                                                  # return ind 
}
  
