#'@title Calculates all stability and resistance indicators
#'@description This function calculates all (or a subset) of the structure and
#'  function indicators for \eqn{j} areas and \eqn{i} years.
#'@details This function calculates the stability and resistance indicators:
#'  biomass per trophic level, intrinsic vulnerability index, CV of biomass,
#'  mean lifespan, and mean maximum length (weighted by biomass and abundance).
#'  If data are not available to calculate one or more of these indicators, a
#'  subset will be returned. See the help file for the individual functions for
#'  information on how each indicator is calculated.
#'@inheritParams resourcePotential
#'@inheritParams landings
#'@inheritParams communityCondition
#'@inheritParams largeSpeciesIndicator
#'@inheritParams biomassPerTL
#'@inheritParams CVBiomass
#'@param speciesinfo.table A table with columns "SPECIES" and the corresponding
#'  "MAXAGE", "MAXLENGTH" and "TL" (maximum recorded age, maximum recorded
#'  length and trophic level). Entries in the "SPECIES" column should be the
#'  unique values of species codes in X (or a subset thereof). Other columns
#'  will be ignored.
#'@return Returns a dataframe with columns "ID", "YEAR", and indicators
#'  corresponding to the arguments supplied to the function.
#'@family stability and resistance indicators
#'@references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@export


allStability <- function(X, X_length, land, 
                         species.table, speciesinfo.table,  
                         TL.grouping = 1, window = 5, negative = FALSE, years){
  
  if("BIOMASS" %in% colnames(X)) {
    inds <- createDataframe(unique(X$ID), years)
  } else {if("BIOMASS" %in% colnames(X_length)) {
      inds <- createDataframe(unique(X_length$ID), years)
  } else {inds <- createDataframe(unique(land$ID), years)}
  }
  
  if("BIOMASS" %in% colnames(X)){
    
    # CV biomass
    CV_bio = CVBiomass(X, window = 5, negative = negative, years = years)
    inds <- merge(inds, CV_bio)
    
    # Max LifeSpan
    if("MAXAGE" %in% colnames(speciesinfo.table)){
      MMA = meanMaxAge(X, age.table = speciesinfo.table, "BIOMASS", years = years)
      
      inds <- merge(inds, MMA)
    }
    
    # Biomass per TL
    if("TL" %in% colnames(speciesinfo.table)){
      bio_TL = biomassPerTL(X, TL.table = speciesinfo.table, metric = "BIOMASS", 
                            TL.grouping = TL.grouping, years = years)
      inds <- merge(inds, bio_TL)
    }
  } 
  
  # Length-based indicators
  if("LENGTH" %in% colnames(X_length)){
    
    # Mean max length
    if("MAXLENGTH" %in% colnames(speciesinfo.table)){
      MML_bio = meanMaxLength(X_length, group = "FINFISH", species.table = species.table,
                        lmax.table = speciesinfo.table, metric = "BIOMASS", years = years)
      inds <- merge(inds, MML_bio)
 
      MML_abund = meanMaxLength(X_length, group = "FINFISH",  species.table = species.table,
                          lmax.table = speciesinfo.table, metric = "ABUNDANCE", years = years)
      inds <- merge(inds, MML_abund)
    }
  }
  
  # Landings-based indicators
  if("CATCH" %in% colnames(land)){
    
    # IVI Landings
    if("IVI" %in% colnames(speciesinfo.table)){
      IVI = IVILandings(land, IVI.table = speciesinfo.table, negative = negative, years = years)
      inds <- merge(inds, IVI)
    }
  }
  
  inds
}
  
  
  
  

