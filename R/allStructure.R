#'@title Calculates all structure and function indicators
#'@description This function calculates all (or a subset) of the structure and
#'  function indicators for \eqn{j} areas and \eqn{i} years.
#'@details This function calculates the structure and function indicators:
#'  biomass, biomass ratio(s), large species indicator, trophic level of the
#'  community, large fish indicator, mean length (weighted by biomass and
#'  abundance), and community condition. If data are not available to calculate
#'  one or more of these indicators, a subset will be returned. See the help
#'  file for the individual functions for information on how each indicator is
#'  calculated.
#'
#'  Notes on indicator calculations: In the individual functions, the user
#'  generally has control over which metric is used to calculate indicator
#'  (e.g., \code{BIOMASS} or \code{ABUNDANCE}). Here, \code{BIOMASS} is used to
#'  calculate biomass, biomass ratio(s), large species indicator, large fish
#'  indicator, trophic level of the community, and mean length weighted by
#'  biomass. \code{ABUNDANCE} is used to calculate mean length weighted by
#'  abundance and community condition. Similarly, \code{group = "ALL"} for large
#'  species indicator and the large fish indicator. See individual functions for
#'  more flexibility.
#'@inheritParams resourcePotential
#'@inheritParams communityCondition
#'@inheritParams largeSpeciesIndicator
#'@param LSI.group A character string indicating the species group for which to
#'  calculate the Large Species Indicator. Must be set to \code{"ALL"} or match
#'  a column name in \code{species.table}. If \code{LSI.group = NULL}, the large
#'  species indicator will not be calculated.
#'@param LFI.group A character string indicating the species group for which to
#'  calculate the Large Fish Indicator. Must be set to \code{"ALL"} or match a
#'  column name in \code{species.table}. If \code{LFI.group = NULL}, the large
#'  fish indicator will not be calculated.
#'@param resource.groups A vector indicating the species groups for which to
#'  calculate the resource potential. Each entry must be a character string
#'  matching the name of a column in \code{species.groups}. If
#'  \code{resource.groups = NULL}, the resource potential indicators will not be
#'  calculated.
#'@param condition.groups  A vector indicating the species groups for which to
#'  calculate Fulton's community condition factor. Each entry must be a
#'  character string matching the name of a column in \code{species.groups}. If
#'  \code{condition.groups = NULL}, the community condition indicators will not
#'  be calculated.
#'@param ratio.groups A dataframe with two columns, which must be named
#'  \code{group1} and \code{group2}. Each row holds the group names for one
#'  biomass ratio, with the numerator in column \code{group1} and the
#'  denominator in column \code{group2}.  Each entry must be a character string
#'  matching the name of a column in \code{species.groups}. If
#'  \code{ratio.groups = NULL}, biomass ratio indicators will not be calculated.
#'@param species.table A table where the column names match the entries in
#'  \code{resource.groups}, \code{ratio.groups}, and/or \code{condition.groups}.
#'  Column entries are the species codes indicating the species from \code{X}
#'  (or \code{X_length}) included in each group. \code{species.table} may also
#'  include columns for other species groups; these will be ignored.
#'@param speciesinfo.table A table with columns \code{SPECIES} and the
#'  corresponding \code{MAXLENGTH} and \code{TL} (maximum recorded length and
#'  trophic level). Entries in the \code{SPECIES} column should be the unique
#'  values of species codes in \code{X}/\code{X_length} (or a subset thereof).
#'  Other columns will be ignored.
#'@param years A vector of years for which to calculate indicators.
#'@return Returns a dataframe with columns \code{ID}, \code{YEAR}, and
#'  indicators corresponding to the arguments supplied to the function.
#'@family ecosystem structure and function indicators
#'@references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@export

allStructure <- function(X, X_length,
                         LSI.group, LFI.group,
                         resource.groups, condition.groups, ratio.groups,
                         species.table, speciesinfo.table, LenWt.table,
                         lmax, years){

  if("BIOMASS" %in% colnames(X)) {
    inds <- createDataframe(unique(X$ID), years)
  } else inds <- createDataframe(unique(X_length$ID), years)

  if("BIOMASS" %in% colnames(X)){
    
    # Resource potential
    if(length(resource.groups) > 0){

        potential <- resourcePotential(X, metric = "BIOMASS", groups = resource.groups,
                                         species.table = species.table, years = years)
        inds <- merge(inds, potential)
      }
    
    # Biomass ratio
    if("group1" %in% colnames(ratio.groups)){
      
      ratio <- biomassRatio(X, metric = "BIOMASS", ratio.groups = ratio.groups,
                                species.table = species.table, years = years)
        
      inds <- merge(inds, ratio)
    }
    
  
    # Large species indicator
    if("MAXLENGTH" %in% colnames(speciesinfo.table) & length(LSI.group > 0)){
      LSI = largeSpeciesIndicator(X, group = LSI.group, species.table = species.table,
                              lmax.table = speciesinfo.table, lmax = lmax,  
                              metric = "BIOMASS", years = years)
      inds <- merge(inds, LSI)
      }
    
    # Trophic level of the community
    if("TL" %in% colnames(speciesinfo.table)){
      
      TL = meanTLCommunity(X, TL.table = speciesinfo.table, metric = "BIOMASS", years = years)
      
      inds <- merge(inds, TL)
      }
  } 
  
  # Length-based indicators
  if("LENGTH" %in% colnames(X_length)){
    
    # Large fish indicator
    if(length(LFI.group > 0)){
      
      LFI = largeFishIndicator(X_length, group = LFI.group, species.table = species.table,
                             metric = "BIOMASS", years = years)
      inds <- merge(inds, LFI)
    }
    
    # Mean length of community weighted by biomass
    ML_bio = meanLengthCommunity(X_length, metric = "BIOMASS", years = years)
    inds <- merge(inds, ML_bio)
    
    # Mean length of community weighted by abundance
    ML_abund = meanLengthCommunity(X_length, metric = "ABUNDANCE", years = years)
    inds <- merge(inds, ML_abund)
    
    # Commuity condition
    if("LENGTH" %in% colnames(LenWt.table) & length(condition.groups) > 0){
      
      condition = communityCondition(X_length, LenWt.table = LenWt.table, 
                                         groups = condition.groups,
                                         species.table = species.table, years = years)
      
      inds <- merge(inds, condition)
    }
    
  }
  
  inds
}
