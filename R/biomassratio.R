#'@title Calculates the biomass ratio between two species groups
#'@description This function calculates the biomass ratio between two
#'  pre-defined species groups for \eqn{j} areas and \eqn{i} years.
#'@details One useful biomass ratio is the proportion of predatory fish in the
#'  community, which is estimated by: \deqn{PropPred = Biomass Predatory Fish
#'  Surveyed/Total Biomass Surveyed} Predatory fish species are defined as all
#'  surveyed fish species that are not largely planktivorous, ie., fish that are
#'  piscivorous, or that feed on invertebrates larger than the macrozooplankton
#'  category (0.2 cm; Shin et al. 2010). Phytoplankton, zooplankton and detritus
#'  feeders should be excluded. This indicator captures changes in the trophic
#'  structure and changes in the functional diversity of fish in the ecosystem.
#'
#'  Other useful biomass \eqn{(B)} ratios indicators include:
#'  \deqn{B_{invertebrates}/B_{demersal}} and \deqn{B_{pelagic}/B_{demersal}}
#'
#'  Recommended data: Fishery independent survey data or model output; fish and
#'  invertebrates.
#'@inheritParams resourcePotential
#'@param X A dataframe of fishery independent survey data with columns "YEAR",
#'  "ID", "SPECIES", and "BIOMASS". "YEAR" indicates the year the observation
#'  was recorded, "ID" is an area code indicating where the observation was
#'  recorded, "SPECIES" is a numeric code indicating the species sampled, and
#'  "BIOMASS" is the corresponding biomass (stratified and corrected for
#'  catchability as required).
#'@param group1  A character string indicating which species to include in the
#'  numerator. Must match the name of a column in species.table.
#'@param group2  A character string indicating which species to include in the
#'  denominator. Must be "ALL" or match the name of a column in species.table.
#'@param species.table A table with at least one column, named after the string
#'  group1. The entries in column group1 are the species codes for the species
#'  included in this group. If group2 is not "ALL", an additional column in
#'  species.table is required. This column is named after the string group2, and
#'  entries are the species codes for the species included in this group.
#'  Species codes should be a subset of those in the "SPECIES" column of X.
#'  species.table may also include columns for other species groups; these will
#'  be ignored.
#'@return Returns a dataframe with 3 columns. "ID", "YEAR", and "group1_group2".
#'
#'  If there is no data for spatial scale \eqn{j} in year \eqn{i}, indicator
#'  values is assigned NA.
#'@family ecosystem structure and function indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Bundy A, Heymans JJ, Morissette L, Savenkoff C (2009) Seals, cod and forage
#'  fish: A comparative exploration of variations in the theme of stock collapse
#'  and ecosystem change in four Northwest Atlantic ecosystems. Prog Oceanogr
#'  81:188 206
#'
#'  Shin YJ, Shannon LJ, Bundy A, Coll M, Aydin K, Bez N, Blanchard JL, Borges,
#'  MF, Diallo I, Diaz E, Heymans JJ, Hill L, Johannesen E, Jouffre D, Kifani S,
#'  Labrosse P, Link JS, Mackinson S, Masski H, Möllmann C, Neira S, Ojaveer H,
#'  Abdallahi KM, Perry I, Thiao D, Yemane D, and Cury PM. 2010.
#'  Using indicators for evaluating, comparing and communicating the ecological
#'  status of exploited marine ecosystems. Part 2: Setting the scene. ICES
#'  Journal of Marine Science, 67: 692-716
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@export

biomassRatio <- function(X, group1, group2, species.table,
                    metric = 'BIOMASS', years) {
	
  num <- resourcePotential(X = X, group = group1, metric = metric,
                           species.table = species.table, years = years)  # calculate biomass of invertebrates
  names(num) <- c("ID", "YEAR", metric)
  denom <- resourcePotential(X = X, group= group2, metric = metric, 
                             species.table = species.table, years = years)     # calculate biomass of demersal fish
  names(denom) <- c("ID", "YEAR", metric)
    
  num$ind = num[, metric] / denom[, metric]       # calculate invertebrate to demersal ratio
  num[metric] <- NULL
  
  name.ind <- paste(group1, "_", group2, sep = "")
  names(num) = c("ID", "YEAR", name.ind)             # name the ind dataframe
  num <- num[order(num$ID), ]
  num                                                # return dataframe of indicator values for years c(start.year:end.year) 

}

