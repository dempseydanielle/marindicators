#'@title Calculates the biomass ratio between two species groups
#'@description This function calculates the biomass ratio between two
#'  pre-defined species groups for \eqn{j} areas and \eqn{i} years.
#'@details Useful biomass \eqn{(B)} ratio indicators include:
#'  \deqn{B_{invertebrates}/B_{demersal}} and \deqn{B_{pelagic}/B_{demersal}}
#'
#'  Recommended data: Fishery independent survey data or model output, fish and
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
#'  denominator. Must match the name of a column in species.table.
#'@param species.table A table with at least two columns, named after the
#'  strings in group1 and group2. The entries in column group1 are the species
#'  codes for the species included in this group. The entries in column group2
#'  are the species codes for the species included in this group. species.table
#'  may also include columns for different species groups; these will be
#'  ignored.
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
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#'@export

biomassratio <- function(X, group1, group2, species.table,
                    metric = 'BIOMASS', years) {
	
  num <- resourcePotential(X = X, group = group1, metric = metric, years = years)  # calculate biomass of invertebrates
  names(num) <- c("ID", "YEAR", metric)
  denom <- resourcePotential(X = X, group= group2, metric = metric, years = years)     # calculate biomass of demersal fish
  names(denom) <- c("ID", "YEAR", metric)
    
  num$ind = num[, metric] / denom[, metric]       # calculate invertebrate to demersal ratio
  num[metric] <- NULL
  
  name.ind <- paste(group1, "_", group2, sep = "")
  names(num) = c("ID", "YEAR", name.ind)             # name the ind dataframe
  num <- num[order(num$ID), ]
  num                                                # return dataframe of indicator values for years c(start.year:end.year) 

}

