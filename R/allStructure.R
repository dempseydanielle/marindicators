#'@title Calculates all structure and function indicators
#'@description This function calculates all (or a subset) of the Structure and
#'  Functioning indicators for \eqn{j} areas and \eqn{i} years. The user can
#'  choose whether the function returns the raw indicator values, the
#'  standardized (z-score) values, or both.
#'@details This function calculates the Structure and Functioning indicators:
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
#'  abundance and community condition. See individual functions for more
#'  flexibility.
#'@inheritParams resourcePotential
#'@inheritParams communityCondition
#'@inheritParams largeSpeciesIndicator
#'@param LSI.group A character string indicating the species group for which to
#'  calculate the Large Species Indicator. Must be set to \code{"ALL"} or match
#'  a column name in \code{species.table}. If \code{LSI.group = NULL}, the Large
#'  Species Indicator will not be calculated.
#'@param LFI.group A character string indicating the species group for which to
#'  calculate the Large Fish Indicator. Must be set to \code{"ALL"} or match a
#'  column name in \code{species.table}. If \code{LFI.group = NULL}, the Large
#'  Fish Indicator will not be calculated.
#'@param guild.groups A vector indicating the species groups for which to
#'  calculate the resource potential. Each entry must be a character string
#'  matching the name of a column in \code{species.table}. If
#'  \code{guild.groups = NULL}, these indicators will not be
#'  calculated.
#'@param condition.groups  A vector indicating the species groups for which to
#'  calculate Fulton's community condition factor. Each entry must be a
#'  character string matching the name of a column in \code{species.table}. If
#'  \code{condition.groups = NULL}, the community condition indicators will not
#'  be calculated.
#'@param ratio.groups A dataframe with two columns, which must be named
#'  \code{group1} and \code{group2}. Each row holds the group names for one
#'  biomass ratio, with the numerator in column \code{group1} and the
#'  denominator in column \code{group2}. Each entry must be a character string
#'  matching the name of a column in \code{species.table} or \code{"ALL"}. If
#'  \code{ratio.groups = NULL}, biomass ratio indicators will not be calculated.
#'@param species.table A table where the column names match the entries in
#'  \code{guild.groups}, \code{ratio.groups}, and/or \code{condition.groups}.
#'  Column entries are the species codes indicating the species from \code{X}
#'  (or \code{X_length}) included in each group. \code{species.table} may also
#'  include columns for other species groups; these will be ignored.
#'@param speciesinfo.table A table with columns \code{SPECIES} and the
#'  corresponding \code{MAXLENGTH} and \code{TL} (maximum recorded length and
#'  trophic level). Entries in the \code{SPECIES} column should be the unique
#'  values of species codes in \code{X}/\code{X_length} (or a subset thereof).
#'  Other columns will be ignored.
#'@param years A vector of years for which to calculate indicators.
#'@param raw A logical value. If \code{raw = TRUE}, the raw indicator values are
#'  returned from the function. If \code{raw = FALSE}, the raw indcator values
#'  are not returned. Default is \code{raw = TRUE}. Either \code{raw} or
#'  \code{std} must be \code{TRUE}.
#'@param std A logical value. If \code{std = TRUE}, the standardized indicator
#'  values for each area ID are returned from the function. Here, indicators are
#'  standardized using Z-scores, i.e., by subtracting the mean and dividing by
#'  the standard deviation (ignoring NA values). If \code{std = FALSE}, the
#'  standardized indcator values are not returned. Default is \code{std = TRUE}.
#'  Either \code{raw} or \code{std} must be \code{TRUE}.
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
#'data(X_length)
#'data(species.groups)
#'data(species.info)
#'data(Length_Weight)
#'
#'ratio.groups <- data.frame(rbind(c("PELAGIC", "GROUNDFISH"), c("PREDATORS", "ALL")))
#'names(ratio.groups) <- c("group1", "group2")
#'trophicguild.groups <- c("LBENTHIVORE", "MBENTHIVORE", "PISCIVORE", "PLANKTIVORE",
#'    "ZOOPISCIVORE")
#'condition.groups <- c("FINFISH", "LBENTHIVORE", "MBENTHIVORE", "PISCIVORE",
#'     "PLANKTIVORE", "ZOOPISCIVORE")
#'allStructure(X = X, X_length = X_length,
#'    LSI.group = "ALL", LFI.group = "ALL",
#'    guild.groups = trophicguild.groups, condition.groups = condition.groups,
#'    ratio.groups = ratio.groups,
#'    species.table = species.groups, speciesinfo.table = species.info,
#'    LenWt.table = Length_Weight,
#'    max.length = 85, years = c(2014:2019), raw = TRUE, std = FALSE)
#'@export

allStructure <- function(X, X_length,
                         LSI.group, LFI.group,
                         guild.groups, condition.groups, ratio.groups,
                         species.table, speciesinfo.table, LenWt.table,
                         max.length, years, raw = TRUE, std = TRUE){

  if(raw == FALSE & std == FALSE) print("error: both raw and std are FALSE")
  
  if("BIOMASS" %in% colnames(X)) {
    inds <- createDataframe(unique(X$ID), years)
  } else inds <- createDataframe(unique(X_length$ID), years)

  if("BIOMASS" %in% colnames(X)){
    
    # Resource potential
    if(length(guild.groups) > 0){

        potential <- resourcePotential(X, metric = "BIOMASS", groups = guild.groups,
                                         species.table = species.table, years = years)
        inds <- merge(inds, potential, all.x = TRUE)
      }
    
    # Biomass ratio
    if("group1" %in% colnames(ratio.groups)){
      
      ratio <- biomassRatio(X, metric = "BIOMASS", ratio.groups = ratio.groups,
                                species.table = species.table, years = years)
        
      inds <- merge(inds, ratio, all.x = TRUE)
    }
    
  
    # Large species indicator
    if("MAXLENGTH" %in% colnames(speciesinfo.table) & length(LSI.group > 0)){
      LSI = largeSpeciesIndicator(X, group = LSI.group, species.table = species.table,
                              maxlength.table = speciesinfo.table, max.length = max.length,  
                              metric = "BIOMASS", years = years)
      inds <- merge(inds, LSI, all.x = TRUE)
      }
    
    # Trophic level of the community
    if("TL" %in% colnames(speciesinfo.table)){
      
      TL = meanTLCommunity(X, TL.table = speciesinfo.table, metric = "BIOMASS", years = years)
      
      inds <- merge(inds, TL, all.x = TRUE)
      }
  } 
  
  # Length-based indicators
  if("LENGTH" %in% colnames(X_length)){
    
    # Large fish indicator
    if(length(LFI.group > 0)){
      
      LFI = largeFishIndicator(X_length, group = LFI.group, species.table = species.table,
                             metric = "BIOMASS", years = years)
      inds <- merge(inds, LFI, all.x = TRUE)
    }
    
    # Mean length of community weighted by biomass
    ML_bio = meanLengthCommunity(X_length, metric = "BIOMASS", years = years)
    inds <- merge(inds, ML_bio, all.x = TRUE)
    
    # Mean length of community weighted by abundance
    ML_abund = meanLengthCommunity(X_length, metric = "ABUNDANCE", years = years)
    inds <- merge(inds, ML_abund, all.x = TRUE)
    
    # Commuity condition
    if("LENGTH" %in% colnames(LenWt.table) & length(condition.groups) > 0){
      
      condition = communityCondition(X_length, LenWt.table = LenWt.table, 
                                         groups = condition.groups,
                                         species.table = species.table, years = years)
      
      inds <- merge(inds, condition, all.x = TRUE)
    }
    
  }
  
  if(std == TRUE){
    inds_std <-  standardize(inds)
    
    if(raw == FALSE) inds <- inds_std
    if(raw == TRUE) inds <- merge(inds, inds_std)
  }
  
  inds
}
