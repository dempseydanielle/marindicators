#' @title Calculates the biomass (or abundance) ratio between two species groups
#'
#' @description This function calculates the biomass ration between two
#'   pre-defined species groups for \eqn{j} areas and \eqn{i} years.
#' @details Useful biomass ratio indicators include:
#'   \deqn{B_{invertebrates}/B_{demersal}} and \deqn{B_{pelagic}/B_{demersal}}
#'
#'   **Recommended data: Fishery independent surveys, fish and invertebrates.
#' @param X dataframe of fishery independent survey data with columns "YEAR",
#'   "ID", "SPECIES", and "BIOMASS" and/or "ABUNDANCE". "ID" is an area code
#'   designating where the observation was recorded. "SPECIES" is a numeric code
#'   indicating the species sampled.
#' @param group1 species group for the numerator (string), as defined in
#'   functions "resourcePotential" and "speciesgroups"
#' @param group2 species group for the denominator (string), as defined in
#'   functions "resourcePotential" and "speciesgroups"
#' @param metric character string indicating which metric to use to calculate
#'   indicator.
#' @param years vector of years for which to calculate indicator
#' @return Returns a dataframe with 3 columns. "ID", "YEAR", and
#'   "group12group2".
#'
#'   If there is no data for spatial scale \eqn{j} in year \eqn{i}, indicator
#'   values is assigned NA.
#' @family ecosystem structure and function indicators
#' @references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   Bundy A, Heymans JJ, Morissette L, Savenkoff C (2009) Seals, cod and forage
#'   fish: A comparative exploration of variations in the theme of stock
#'   collapse and ecosystem change in four Northwest Atlantic ecosystems. Prog
#'   Oceanogr 81:188 206
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#' @export

biomassratio <- function(X, group1 = c('INVERTEBRATES', 'PELAGIC'), group2 = c('GROUNDFISH'),
                    metric = 'BIOMASS', years) {
	
  num <- resourcePotential(X = X, group = group1, metric = metric, years = years)  # calculate biomass of invertebrates
  names(num) <- c("ID", "YEAR", metric)
  denom <- resourcePotential(X = X, group= group2, metric = metric, years = years)     # calculate biomass of demersal fish
  names(denom) <- c("ID", "YEAR", metric)
    
  num$ind = num[, metric] / denom[, metric]       # calculate invertebrate to demersal ratio
  num[metric] <- NULL
  
  name.ind <- paste(group1, "2", group2, sep = "")
  names(num) = c("ID", "YEAR", name.ind)             # name the ind dataframe
  num <- num[order(num$ID), ]
  num                                                # return dataframe of indicator values for years c(start.year:end.year) 

}

