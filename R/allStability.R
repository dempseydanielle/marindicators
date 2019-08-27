#'@title Calculates all stability and resistance indicators
#'@description This function calculates all (or a subset) of the stability and
#'  resistance indicators for \eqn{j} areas and \eqn{i} years.
#'@details This function calculates the stability and resistance indicators:
#'  biomass per trophic level, intrinsic vulnerability index, CV of biomass,
#'  mean lifespan, and mean maximum length (weighted by biomass and abundance).
#'  If data are not available to calculate one or more of these indicators, a
#'  subset will be returned. See the help file for the individual functions for
#'  information on how each indicator is calculated.
#'@inheritParams resourcePotential
#'@inheritParams landings
#'@inheritParams communityCondition
#'@param maxlength.group A character string indicating the species group for
#'  which to calculate the mean maximum length of fish in the community. Must be
#'  set to "ALL" or match a column name in species.table. If maxlength.group =
#'  NULL, the mean maximum length will not be calculated.
#'@param species.table A table where the column names match the entries in
#'  resource.groups, ratio.groups, and/or condition.groups. Column entries are
#'  the species codes indicating the species from X (or X_length) included in
#'  each group. species.table may also include columns for other species groups;
#'  these will be ignored.
#'@param speciesinfo.table A table with columns "SPECIES" and the corresponding
#'  "MAXAGE", "MAXLENGTH" and "TL" (maximum recorded age, maximum recorded
#'  length and trophic level). Entries in the "SPECIES" column should be the
#'  unique values of species codes in X (or a subset thereof). Other columns
#'  will be ignored.
#'@param TL.grouping Size of the trophic level bin for which to aggregrate
#'  biomass when calculating biomass per trophic level. For example, if
#'  TL.grouping = 1, trophic levels are binned from 1.00 - 1.99, 2.00 - 2.99,
#'  etc. If TL.grouping = 0.5, trophic levels are binned from 1.00 - 1.49, 1.50
#'  - 1.99, 2.00 - 2.49, 2.50 - 2.99, etc. Default is TL.grouping = 1 so that
#'  biomass is aggregated over discrete trophic levels.
#'@param window Window for the moving average used to calculate CV of the
#'  biomass. The first and last floor(window/2) values of the indicator are
#'  assigned NA to account for the moving average. Default is window = 5 years.
#'@param negative If negative = TRUE, the CVBiomass will be multiplied by -1 so
#'  that the expected response is to decrease with increasing fishing pressure.
#'  Default is negative = FALSE.
#'@param years A vector of years for which to calculate indicators.
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
                         maxlength.group,
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
  
  # Mean max length
  if("LENGTH" %in% colnames(X_length) & "MAXLENGTH" %in% colnames(speciesinfo.table) & 
     length(maxlength.group) > 0){
    
    if("BIOMASS" %in% colnames(X_length)){
      MML_bio = meanMaxLength(X_length, group = maxlength.group, species.table = species.table,
                        lmax.table = speciesinfo.table, metric = "BIOMASS", years = years)
      inds <- merge(inds, MML_bio)
    }
 
    if("ABUNDANCE" %in% colnames(X_length)){
      MML_abund = meanMaxLength(X_length, group = maxlength.group,  species.table = species.table,
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
  
  
  
  

