#'@title Calculates Pielou's Species Evenness
#'@description This function takes a dataframe of fisheries independent survey
#'  data and calculates Pielou's Species Evenness for \eqn{j} areas and \eqn{i}
#'  years.
#'@details Pielou's Species Evenness: \deqn{J' = -\Sigma p_i ln( p_i )/ln(S)}
#'  \eqn{p_i} is the proportion of the total sample contributed by the i(th)
#'  species and \eqn{S} is the number of species recorded in the sample.
#'  Pielou's index is the Shannon-Weiner index computed for the sample \eqn{S}
#'  and represents a measure of evenness of the community.
#'
#'  **Recommended data: Fishery independent surveys, fish and invertebrates.
#'
#'@param X dataframe of fishery independent survey data with columns "YEAR",
#'  "ID", "SPECIES", and "BIOMASS" and/or "ABUNDANCE". "ID" is an area code
#'  designating where the observation was recorded (a string). "SPECIES" is a
#'  numeric code indicating the species sampled.
#'@param group character string indicating which species to include, either
#'  "ALL" or "FINFISH". Note that this subsetting is based on the Fisheries and
#'  Oceans Canada species codes for the Scotian Shelf. For other regions it may
#'  be prudent to subsetdata to species groups of interest prior to using the
#'  function and then choose group = "ALL". Type ?speciesgroups for more
#'  information.
#'@param metric character string indicating whether to use "BIOMASS" or
#'  "ABUNDANCE" to calculate the indicator.
#'@param years vector of years for which to calculate indicator.
#'@return Returns a dataframe with 3 columns: "ID", YEAR", and "PielouEvenness"
#'@family biodiversity indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Greenstreet SP, Rogers SI (2006) Indicators of the health of the North Sea
#'  fish community: identifying reference levels for an ecosystem approach to
#'  management. ICES J Mar Sci J du Cons 63:573–593
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}
#'@export

# add a warning for if there is 0 biomass
pielouSpeciesEvenness <- function(X, group = c('FINFISH','ALL'), metric = c('BIOMASS','ABUNDANCE'),
                                  years)  {


  H <-shannon(X, group = group, metric = metric, years = years) # calculate Shannon's diversity for each year
  S <- speciesrichness(X, group = group, metric = metric, years = years) # calculate species richness for each year

  H$Pielous = H$ShannonDiversity/log(S$SpeciesRichness) # calculate Pielou's species evenness
  
  ind <- H
  ind$ShannonDiversity <- NULL                      # remove Shannon's diversity from ind
  names(ind) = c("ID", "YEAR", "PielouEvenness")    # name the ind dataframe
  ind
}
