#'@title Calculates all resource potential indicators
#'@description This function calculates all (or a subset) of the resource
#'  potential indicators for \eqn{j} areas and \eqn{i} years.
#'@details This function calculates the resource potential indicators: abundance
#'  and biomass of the community, resource potential of predefined species
#'  groups, and fishing-in-balance (FiB). If data are not available to calculate
#'  one or more of these indicators, a subset will be returned. See the help
#'  file for the individual functions for information on how each indicator is
#'  calculated.
#'@inheritParams allStructure
#'@inheritParams resourcePotential
#'@inheritParams landings
#'@inheritParams fishingInBalance
#'@param species.table A table where the column names match the entries in
#'  resource.groups. Column entries are the species codes indicating the species
#'  from X included in each group. species.table may also include columns for
#'  other species groups; these will be ignored.
#'@param speciesinfo.table A table with columns "SPECIES" and the corresponding
#'  "TL_LAND" (trophic level of landed species). Entries in the "SPECIES" column
#'  should be the unique values of species codes in land (or a subset thereof).
#'  Other columns will be ignored.
#'@param cutoff The minimum trophic level of species to include to calculate
#'  FiB. If cutoff = NULL, FiB wll not be calculated.
#'@param TE Trophic efficiency, used to calculate FiB. Default is TE = 0.1,
#'  i.e., a trophic efficiency of 10\%. If TE = NULL, FiB wll not be calculated.
#'@param base.start Year indicating the beginning of the baseline period for
#'  calculating FiB. The average landings and average mean trophic level of the
#'  landings over the baseline period are used as baseline values to calculate
#'  FiB (see Details). land must include data for the baseline period. If
#'  base.start = NULL, FiB wll not be calculated.
#'@param base.end Year indicating the end of the baseline period for calculating
#'  FiB. The average landings and average mean trophic level of the landings
#'  over the baseline period are used as baseline values to calculate FiB (see
#'  Details). land must include data for the baseline period. If base.end =
#'  NULL, FiB wll not be calculated.
#'@return Returns a dataframe with columns "ID", "YEAR", and indicators
#'  corresponding to the arguments supplied to the function.
#'@family ecosystem structure and function indicators
#'@references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@export


allPotential <- function(X, land, 
                         species.table, speciesinfo.table, 
                         resource.groups, cutoff = 0,
                         TE = 0.1, base.start, base.end, years){
  
  if("BIOMASS" %in% colnames(X)) {
    inds <- createDataframe(unique(X$ID), years)
  } else inds <- createDataframe(unique(land$ID), years)

  # Abundance
  if("ABUNDANCE" %in% colnames(X)){
    abund = resourcePotential(X, metric = "ABUNDANCE", groups = "ALL",
                          species.table = species.table, years = years)
    inds <- merge(inds, abund)
  }
  
  # Biomass 
  if("BIOMASS" %in% colnames(X) & length(resource.groups)){
      
        potential <- resourcePotential(X, metric = "BIOMASS", groups = resource.groups,
                                       species.table = species.table, years = years)
        inds <- merge(inds, potential)
  }

  # Fishing in Balance
  if("CATCH" %in% colnames(land) & length(TE) > 0 & length(cutoff) >0
     &length(base.start) > 0 & length(base.end) >0){
    FIB = fishingInBalance(land, TE = TE,  base.start = base.start, base.end = base.end, 
                          TL.table = speciesinfo.table,  cutoff = cutoff,  years = years)
    
    inds <- merge(inds, FIB)
  }

  inds
}




