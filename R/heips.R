#' @title Calculates Heip's Evenness Index
#' @description This function takes a dataframe of fisheries independent survey
#'   data and calculates Heip's Evenness Index (HE) of the community for \eqn{j}
#'   areas and \eqn{i} years.
#' @details Heip's Evenness Index (HE): \deqn{HE = (exp^{H'} - 1)/(S - 1)} where
#'   \eqn{H'} is the Shannon Index of Diversity and \eqn{S} is species richness.
#'   This index ranges from 0 to 1 and measures how equally the species richness
#'   contributes to the total abundance or biomass of the community.
#'
#'   **Recommended data: Fishery independent surveys, fish and invertebrates
#' @param X dataframe of fishery independent survey data with columns "YEAR",
#'   "ID", "SPECIES", and "BIOMASS" and/or "ABUNDANCE". "ID" is an area code
#'   designating where the observation was recorded. "SPECIES" is a numeric code
#'   indicating the species sampled.
#' @param group character string indicating which species to include, either
#'   "ALL" or "FINFISH". Note that this subsetting is based on the Fisheries and
#'   Oceans Canada species codes for the Scotian Shelf. For other regions it may
#'   be prudent to subsetdata to species groups of interest prior to using the
#'   function and then choose group = "ALL". Type ?speciesgroups for more
#'   information.
#' @param metric character string indicating whether to use "BIOMASS" or
#'   "ABUNDANCE" to calculate the indicator.
#' @param years vector of years for which to calculate indicator
#' @return Returns a dataframe with 3 columns: "ID", YEAR", and "Heips"
#' @family biodiversity indicators
#' @references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   Kenchington T, Kenchington E (2013) Biodiversity Metrics for Use in the
#'   Ecosystem Approach to Oceans Management. Can. Tech. Rep. Fish. Aquat. Sci.
#'   3059: vi+188p.

#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#' @export

heips <- function(X, group = c('FINFISH','ALL'), metric = c('BIOMASS','ABUNDANCE'),
                  years = c(start.year:end.year))  {
  
  H <-shannon(X, group = group, metric = metric, years = years) # calculate Shannon's diversity for each year
  S <- speciesrichness(X, group = group, metric = metric, years = years) # calculate species richness for each year
  
  H$heips = (exp(H$ShannonDiversity)-1)/(S$SpeciesRichness-1)
  ind <- H
  
  ind$ShannonDiversity <- NULL                     # remove Shannon's diversity from ind
  names(ind) = c("ID", "YEAR", "Heips")            # name the ind dataframe
  ind      
}
