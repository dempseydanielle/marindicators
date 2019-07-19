#'@title Calculates Shannon's index of diversity (H')
#'@description This function takes a dataframe of fisheries independent survey
#'  data and calculates Shannon's index of diversity for \eqn{j} areas and
#'  \eqn{i} years.
#'@details Shannon diversity index (H'): \deqn{H' = -\Sigma p_i ln(p_i)}
#'  \eqn{p_i} is the proportion of the total sample contributed by the i(th)
#'  species and \eqn{S} is the number of species recorded in the sample. This
#'  index is sensitive to the number of species recorded in the sample.
#'
#'  Recommended data: Fishery independent surveys, fish and invertebrates.
#'@param X dataframe of fishery independent survey data with columns "YEAR",
#'  "ID", "SPECIES", and "BIOMASS" and/or "ABUNDANCE". "ID" is an area code
#'  designating where the observation was recorded. "SPECIES" is a numeric code
#'  indicating the species sampled.
#'@param group character string indicating which species to include, either
#'  "ALL" or "FINFISH". Note that this subsetting is based on the Fisheries and
#'  Oceans Canada species codes for the Scotian Shelf. For other regions it may
#'  be prudent to subsetdata to species groups of interest prior to using the
#'  function and then choose grou = "ALL". Type ?speciesgroups for more
#'  information.
#'@param metric character string indicating whether to use "BIOMASS" or
#'  "ABUNDANCE" to calculate the indicator.
#'@param years vector of years for which to calculate indicator
#'@return Returns a dataframe with 3 columns: "ID", YEAR", and
#'  "ShannonDiversity"
#'@family biodiversity indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Greenstreet SP, Rogers SI (2006) Indicators of the health of the North Sea
#'  fish community: identifying reference levels for an ecosystem approach to
#'  management. ICES J Mar Sci J du Cons 63:573-593
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#'@export


shannon <- function(X, group = c('FINFISH','ALL'), metric = c('BIOMASS','ABUNDANCE'),
                    years = c(start.year:end.year)) {
  
  if(group != "ALL") X <- speciesgroups(X = X, group = group) # subset X to the species of interest

  uI = unique(X$ID)                   # extract the spatial scale ID's
  ind <- NULL                         # initialize dataframe for storing indicator values
  
  for (j in 1:length(uI)){            # loop over all spatal scales
    
    X.j = X[X$ID == uI[j], ]          # subset data to spatial scale j
  
    for (i in 1:length(years)){                   # loop over all years
      
      year.i = years[i]                           # set years.i to current year  
      X.ij = X.j[X.j$YEAR == year.i, metric]      # subset data to include only current year
      
      p <- X.ij/sum(X.ij)                          # calculate the proportion of each species by metric
      ind.i <- -sum(p*log(p))                      # calculate Shannon's metric of diversity
      
      ind.i = data.frame(uI[j], year.i, ind.i)          # create a dataframe with spatial scale ID, year, and indicator value
      ind = rbind(ind, ind.i)                           # bind ind.i to ind dataframe
    }
  }
  
  names(ind) = c("ID", "YEAR", "ShannonDiversity")    # name the ind dataframe
  ind                                                  # return ind 
}
  
