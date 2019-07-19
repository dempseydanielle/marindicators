#' @title Calculates Margalef's species richness
#' @description This function takes a dataframe of fisheries independent survey
#'   data and calculates Margalef's species richness for \eqn{j} areas and
#'   \eqn{i} years.
#' @details Margalef's species richness: \deqn{S_{Marg} = (S_y - 1)/log(F_y)}
#'   \eqn{S_y} is the count of the number of species recorded in all trawl
#'   catches collected in year \eqn{y}. \eqn{F} is the total count of all
#'   individuals caught in year \eqn{y}.
#'
#'   **Recommended data: Fishery independent surveys, fish and invertebrates.
#'
#' @param X dataframe of fishery independent survey data with columns "YEAR",
#'   "ID", "SPECIES", and "BIOMASS" and/or "ABUNDANCE". "ID" is an area code
#'   designating where the observation was recorded. "SPECIES" is a numeric code
#'   indicating the species sampled.
#' @param group character string indicating which species to include, either
#'   "ALL", "FINFISH" or "GROUNDFISH". Note that this subsetting is based on the
#'   Fisheries and Oceans Canada species codes for the Scotian Shelf. For other
#'   regions it may be prudent to subsetdata to species groups of interest prior
#'   to using the function and then choose group = "ALL". Type ?speciesgroups
#'   for more information.
#' @param metric character string indicating whether to use "BIOMASS" or
#'   "ABUNDANCE" to calculate the indicator.
#' @param years vector of years for which to calculate indicator
#' @return Returns a dataframe with 3 columns: "ID", YEAR", and
#'   "MargalefRichness"
#' @family biodiversity indicators
#' @references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   Greenstreet SP, Fraser HM, Rogers SI, Trenkel VM, Simpson SD, Pinnegar JK
#'   (2012) Redundancy in metrics describing the composition, structure, and
#'   functioning of the North Sea demersal fish community. ICES J Mar Sci
#'   69:8-22
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#' @export

margalef <- function(X, group=c('ALL', 'FINFISH', 'GROUNDFISH'), 
                     metric=c('BIOMASS','ABUNDANCE'),
                     years = c(start.year:end.year))  {

  if(group != "ALL") X <- speciesgroups(X = X, group = group) # subset X to the species of interest

  S <- speciesrichness(X = X, group = group, metric = metric, years = years) # calculate species richness for each year
  
  uI = unique(X$ID)                   # extract the spatial scale ID's
  ind <- NULL                         # initialize dataframe for storing indicator values
  
  for (j in 1:length(uI)){            # loop over all spatal scales
    
    X.j = X[X$ID == uI[j], ]          # subset data to spatial scale j
    S.j = S[S$ID == uI[j], ]
    
    for(i in 1:length(years)){        # loop over all years
     
      year.i = years[i]                          # set years.i to current year
      X.ij = X.j[X.j$YEAR == year.i, metric]     # subset data to include only current year
      
      logF.i = log(sum(X.ij))                       # calculate the log of the sum of metric over all species
      ind.i = (S.j$SpeciesRichness[i] - 1)/logF.i   # calculate Margalef species richness
      
      ind.i = data.frame(uI[j], year.i, ind.i)          # create a dataframe with spatial scale ID, year, and indicator value
      ind = rbind(ind, ind.i)                           # bind ind.i to ind dataframe
      }
    }  
  names(ind) = c("ID", "YEAR", "MargalefRichness")    # name the ind dataframe
  ind
}
