#'@title Calculates the biomass ratio between two species groups
#'@description This function calculates the biomass ratio between two
#'  pre-defined species groups for \eqn{j} areas and \eqn{i} years.
#'@details One useful biomass ratio is the proportion of predatory fish in the
#'  community, which is estimated by:
#'
#'  PropPred = Biomass of Predatory Fish Surveyed/Total Biomass Surveyed
#'
#'  Predatory fish species are defined as all surveyed fish species that are not
#'  largely planktivorous, ie., fish that are piscivorous, or that feed on
#'  invertebrates larger than the macrozooplankton category (0.2 cm; Shin et al.
#'  2010). Phytoplankton, zooplankton and detritus feeders should be excluded.
#'  This indicator captures changes in the trophic structure and changes in the
#'  functional diversity of fish in the ecosystem.
#'
#'  Other useful biomass \eqn{(B)} ratios indicators include:
#'  \deqn{B_{invertebrates}/B_{demersal}} and \deqn{B_{pelagic}/B_{demersal}}
#'@inheritParams resourcePotential
#'@param X A dataframe of fishery independent survey data or model output with
#'  columns \code{YEAR}, \code{ID}, \code{SPECIES}, and \code{BIOMASS}.
#'  \code{YEAR} indicates the year the observation was recorded, \code{ID} is an
#'  area code indicating where the observation was recorded, \code{SPECIES} is a
#'  numeric code indicating the species sampled, and \code{BIOMASS} is the
#'  corresponding biomass (stratified and corrected for catchability as
#'  required).
#'@param ratio.groups A dataframe with two columns, which must be named
#'  \code{group1} and \code{group2}. Each row holds the group names for one
#'  biomass ratio, with the numerator in column \code{group1} and the
#'  denominator in column \code{group2}. Each entry must be a character string
#'  matching the name of a column in \code{species.table} or \code{"ALL"}.
#'@param species.table A table with column names that match the entries of
#'  \code{ratio.groups}. The entries in each column are the species codes from
#'  \code{X} indicating which species are included that group.
#'  \code{species.table} may also include columns for other species groups;
#'  these will be ignored. Note that an entry in \code{ratio.groups} could be
#'  \code{"ALL"}. In this case, a column in \code{species.table} named
#'  \code{"ALL"} is not required; the function will automatically include all
#'  species in \code{X}.
#'@return Returns a dataframe with columns \code{ID} and \code{YEAR}, and a
#'  column for each biomass ratio, named after the entries in ratio.groups, e.g.
#'  \code{group1_group2}.
#'
#'  If there is no data for spatial scale \eqn{j} in year \eqn{i}, indicator
#'  values is assigned \code{NA}.
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
#'  Abdallahi KM, Perry I, Thiao D, Yemane D, and Cury PM. 2010. Using
#'  indicators for evaluating, comparing and communicating the ecological status
#'  of exploited marine ecosystems. Part 2: Setting the scene. ICES Journal of
#'  Marine Science, 67: 692-716
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@examples
#'ratio.groups <- data.frame(rbind(c("PELAGIC", "GROUNDFISH"), c("PREDATORS", "ALL")))
#'names(ratio.groups) <- c("group1", "group2")
#'biomassRatio(X, ratio.groups = ratio.groups, species.table = species.groups, 
#'    metric = "BIOMASS", years = c(2014:2019))
#'@export

biomassRatio <- function(X, ratio.groups, species.table,
                    metric = 'BIOMASS', years) {
	
  for (k in 1:nrow(ratio.groups)){
    
    num.k <- resourcePotential(X = X, groups = ratio.groups[k, "group1"], metric = metric,
                           species.table = species.table, years = years)  # calculate biomass of invertebrates
    names(num.k) <- c("ID", "YEAR", metric)
  
    denom.k <- resourcePotential(X = X, groups = ratio.groups[k, "group2"], metric = metric, 
                             species.table = species.table, years = years)     # calculate biomass of demersal fish
    names(denom.k) <- c("ID", "YEAR", metric)
    
    num.k$ind = num.k[, metric] / denom.k[, metric]       # calculate invertebrate to demersal ratio
    num.k[metric] <- NULL
    
    name.ind <- paste(ratio.groups[k, "group1"], "_", ratio.groups[k, "group2"], sep = "")
    names(num.k) = c("ID", "YEAR", name.ind)             # name the ind dataframe
    num.k <- num.k[order(num.k$ID), ]
    
    if(k == 1) ind = num.k
    ind <- merge(ind, num.k)
  }
  
  ind                                                # return dataframe of indicator values for years c(start.year:end.year) 

}

