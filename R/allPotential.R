#'@title Calculates all resource potential indicators
#'@description This function calculates all (or a subset) of the resource
#'  potential indicators for \eqn{j} areas and \eqn{i} years.
#'@details This function calculates the resource potential indicators: abundance
#'  and biomass of the community, resource potential of predefined species
#'  groups, and fishing-in-balance. If data are not available to calculate
#'  one or more of these indicators, a subset will be returned. See the help
#'  file for the individual functions for information on how each indicator is
#'  calculated.
#'@inheritParams resourcePotential
#'@inheritParams landings
#'@inheritParams fishingInBalance
#'@inheritParams allStructure
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
  if("CATCH" %in% colnames(land)){
    FIB = fishingInBalance(land, TE = TE,  base.start = base.start, base.end = base.end, 
                          TL.table = speciesinfo.table,  cutoff = cutoff,  years = years)
    
    inds <- merge(inds, FIB)
  }

  inds
}




