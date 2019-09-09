#'@title Calculates all Stability and Resistance indicators
#'@description This function calculates all (or a subset) of the Stability and
#'  Resistance indicators for \eqn{j} areas and \eqn{i} years.
#'@details This function calculates the Stability and Resistance indicators:
#'  Biomass per Trophic Level, Intrinsic Vulnerability Index, Coefficient of
#'  Variation of the biomass, Mean Lifespan, and Mean Maximum Length (weighted
#'  by biomass and abundance). If data are not available to calculate one or
#'  more of these indicators, a subset will be returned. See the help file for
#'  the individual functions for information on how each indicator is
#'  calculated.
#'@inheritParams resourcePotential
#'@inheritParams landings
#'@inheritParams communityCondition
#'@param maxlength.group A character string indicating the species group for
#'  which to calculate the mean maximum length of fish in the community. Must be
#'  set to \code{"ALL"} or match a column name in \code{species.table}. If
#'  \code{maxlength.group = NULL}, the mean maximum length will not be
#'  calculated.
#'@param species.table A table where the column names match the entries in
#'  \code{resource.groups}, \code{ratio.groups}, and/or \code{condition.groups}.
#'  Column entries are the species codes indicating the species from \code{X}
#'  (or \code{X_length}) included in each group. \code{species.table} may also
#'  include columns for other species groups; these will be ignored.
#'@param speciesinfo.table A table with columns \code{SPECIES} and the
#'  corresponding \code{MAXAGE}, \code{MAXLENGTH}, and \code{TL} (maximum
#'  recorded age, maximum recorded length, and trophic level). Entries in the
#'  \code{SPECIES} column should be the unique values of species codes in
#'  \code{X} (or a subset thereof). Other columns will be ignored.
#'@param TL.grouping Size of the trophic level bin for which to aggregrate
#'  biomass when calculating Biomass per Trophic Level. For example, if
#'  \code{TL.grouping = 1}, trophic levels are binned from 1.00 - 1.99, 2.00 -
#'  2.99, etc. If TL.grouping = 0.5, trophic levels are binned from 1.00 - 1.49,
#'  1.50 - 1.99, 2.00 - 2.49, 2.50 - 2.99, etc. Default is \code{TL.grouping =
#'  1} so that biomass is aggregated over discrete trophic levels.
#'@param window Window for the moving average used to calculate the Coefficient
#'  of Variation of the Biomass. The first and last \code{floor(window/2)}
#'  values of the indicator are assigned \code{NA} to account for the moving
#'  average. Default is \code{window = 5} years.
#'@param negative If \code{negative = TRUE}, the Coefficient of Variation of the
#'  Biomasss will be multiplied by -1 so that the expected response is to
#'  decrease with increasing fishing pressure. Default is \code{negative =
#'  FALSE}.
#'@param years A vector of years for which to calculate indicators.
#'@return Returns a dataframe with columns \code{ID}, \code{YEAR}, and
#'  indicators corresponding to the arguments supplied to the function.
#'@family stability and resistance indicators
#'@references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@export


allStability <- function(X, land, 
                         maxlength.group,
                         species.table, speciesinfo.table,  
                         TL.grouping = 1, window = 5, negative = FALSE, years){
  
  if("BIOMASS" %in% colnames(X)) {
    inds <- createDataframe(unique(X$ID), years)
  } else {inds <- createDataframe(unique(land$ID), years)}
  
  
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
    
    if("MAXLENGTH" %in% colnames(speciesinfo.table) & length(maxlength.group) > 0){
      MML_bio = meanMaxLength(X, group = maxlength.group, species.table = species.table,
                        maxlength.table = speciesinfo.table, metric = "BIOMASS", years = years)
      inds <- merge(inds, MML_bio)
    }
 
    if("ABUNDANCE" %in% colnames(X)){
      MML_abund = meanMaxLength(X, group = maxlength.group,  species.table = species.table,
                          maxlength.table = speciesinfo.table, metric = "ABUNDANCE", years = years)
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
  
  
  
  

