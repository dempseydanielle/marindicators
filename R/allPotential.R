#'@title Calculates all Resource Potential indicators
#'@description This function calculates all (or a subset) of the Resource
#'  Potential indicators for \eqn{j} areas and \eqn{i} years.
#'@details This function calculates the Resource Potential indicators: Abundance
#'  and Biomass of the community, Resource Potential of predefined species
#'  groups, and Fishing-in-Balance (FiB). If data are not available to calculate
#'  one or more of these indicators, a subset will be returned. See the help
#'  file for the individual functions for information on how each indicator is
#'  calculated.
#'@inheritParams allStructure
#'@inheritParams resourcePotential
#'@inheritParams landings
#'@inheritParams fishingInBalance
#'@param species.table A table where the column names match the entries in
#'  \code{resource.groups}. Column entries are the species codes indicating the
#'  species from \code{X} included in each group. \code{species.table} may also
#'  include columns for other species groups; these will be ignored.
#'@param speciesinfo.table A table with columns \code{SPECIES} and the
#'  corresponding \code{TL_LAND} (trophic level of landed species). Entries in
#'  the \code{SPECIES} column should be the unique values of species codes in
#'  \code{land} (or a subset thereof). Other columns will be ignored.
#'@param minTL The minimum trophic level of species to include to calculate FiB.
#'  If \code{minTL = NULL}, FiB wll not be calculated.
#'@param TE Trophic efficiency, used to calculate FiB. Default is \code{TE =
#'  0.1}, i.e., a trophic efficiency of 10\%. If \code{TE = NULL}, FiB wll not
#'  be calculated.
#'@param base.start Year indicating the beginning of the baseline period for
#'  calculating FiB. The average landings and average mean trophic level of the
#'  landings over the baseline period are used as baseline values to calculate
#'  FiB. \code{land} must include data for the baseline period. If
#'  \code{base.start = NULL}, FiB wll not be calculated.
#'@param base.end Year indicating the end of the baseline period for calculating
#'  FiB. The average landings and average mean trophic level of the landings
#'  over the baseline period are used as baseline values to calculate FiB.
#'  \code{land} must include data for the baseline period. If \code{base.end =
#'  NULL}, FiB wll not be calculated.
#'@return Returns a dataframe with columns \code{ID}, \code{YEAR}, and
#'  indicators corresponding to the arguments supplied to the function.
#'  Standardized indicators are noted with "_s" in the name.
#'@family ecosystem structure and function indicators
#'@references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@examples
#'data(X)
#'data(land)
#'data(species.table)
#'data(species.info)
#'
#'resource.groups <- c("ALL", "CLUPEIDS", "FINFISH", "FLATFISH",
#'    "FORAGE",  "GADOIDS", "GROUNDFISH")
#'allPotential(X = X, land = land, species.table = species.groups,
#'    speciesinfo.table = species.info, resource.groups = resource.groups,
#'    TE = 0.1, base.start = 2014, base.end = 2015, years = c(2014:2019),
#'    raw = FALSE, std = TRUE)
#'
#'@export


allPotential <- function(X, land, 
                         species.table, speciesinfo.table, resource.groups,
                         minTL = 0, TE = 0.1, base.start, base.end, 
                         years,  raw = TRUE, std = TRUE){
  
  if("BIOMASS" %in% colnames(X)) {
    inds <- createDataframe(unique(X$ID), years)
  } else inds <- createDataframe(unique(land$ID), years)

  # Abundance
  if("ABUNDANCE" %in% colnames(X)){
    abund = resourcePotential(X, metric = "ABUNDANCE", groups = "ALL",
                          species.table = species.table, years = years)
    inds <- merge(inds, abund, all.x = TRUE)
  }
  
  # Biomass 
  if("BIOMASS" %in% colnames(X) & length(resource.groups)){
      
        potential <- resourcePotential(X, metric = "BIOMASS", groups = resource.groups,
                                       species.table = species.table, years = years)
        inds <- merge(inds, potential, all.x = TRUE)
  }

  # Fishing in Balance
  if("CATCH" %in% colnames(land) & length(TE) > 0 & length(minTL) >0
     &length(base.start) > 0 & length(base.end) >0){
    FIB = fishingInBalance(land, TE = TE,  base.start = base.start, base.end = base.end, 
                          TL.table = speciesinfo.table,  minTL = minTL,  years = years)
    
    inds <- merge(inds, FIB, all.x = TRUE)
  }

  if(raw == FALSE & std == FALSE) print("error: both raw and std are FALSE")
  
  if(std == TRUE){
    inds_std <-  standardize(inds)
    
    if(raw == FALSE) inds <- inds_std
    if(raw == TRUE) inds <- merge(inds, inds_std)
  }
  
  inds
}




