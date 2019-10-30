#'@title Calculates all Structure and Functioning indicators
#'@description This function calculates all (or a subset) of the Structure and
#'  Functioning indicators for \eqn{j} areas and \eqn{i} years. The user can
#'  choose whether the function returns the indicator dataframe to the global
#'  environment, exports the dataframe to a .csv file, or both. The user can
#'  also choose whether the function returns the raw indicator values, the
#'  standardized (z-score) values, or both.
#'@details This function calculates the Structure and Functioning indicators:
#'  Biomass, Biomass Ratio(s), Large Species Indicator, Trophic Level of the
#'  Community, Large Fish Indicator, Mean Length (weighted by biomass and
#'  abundance), and Community Condition. If data are not available to calculate
#'  one or more of these indicators, a subset will be returned. See the help
#'  file for the individual functions for information on how each indicator is
#'  calculated.
#'
#'  Notes on indicator calculations: In the individual functions, the user
#'  generally has control over which metric is used to calculate the indicator
#'  (e.g., \code{BIOMASS} or \code{ABUNDANCE}). Here, \code{BIOMASS} is used to
#'  calculate Biomass, Biomass Ratio(s), Large Species Indicator, Large Fish
#'  Indicator, Trophic Level of the Community, and Mean Length weighted by
#'  biomass. \code{ABUNDANCE} is used to calculate Mean Length weighted by
#'  abundance and Community Condition. See individual functions for more
#'  flexibility.
#'@inheritParams resourcePotential
#'@inheritParams communityCondition
#'@inheritParams largeSpeciesIndicator
#'@param X A dataframe of fishery independent data derived from research vessel
#'  survey data or model output, with columns \code{YEAR}, \code{ID},
#'  \code{SPECIES}, and \code{BIOMASS}. \code{YEAR} indicates the year the
#'  observation was recorded, \code{ID} is an area code indicating where the
#'  observation was recorded, \code{SPECIES} is a numeric code indicating the
#'  species sampled, and \code{BIOMASS} is the corresponding biomass (stratified
#'  and corrected for catchability as required).
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
#'  matching the name of a column in \code{species.table}. If \code{guild.groups
#'  = NULL}, these indicators will not be calculated.
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
#'  corresponding \code{TL} and \code{MAXLENGTH} (trophic level and maximum
#'  recorded length). Entries in the \code{SPECIES} column should be the unique
#'  values of species codes in \code{X}/\code{X_length} (or a subset thereof).
#'  Other columns will be ignored.
#'@param years A vector of years for which to calculate indicators.
#'@param raw A logical value. If \code{raw = TRUE}, the raw indicator values are
#'  returned by the function. If \code{raw = FALSE}, the raw indcator values are
#'  not returned. Default is \code{raw = TRUE}. Either \code{raw} or \code{std}
#'  must be \code{TRUE}.
#'@param std A logical value. If \code{std = TRUE}, the standardized indicator
#'  values for each area \code{ID} are returned by the function. Indicators are
#'  standardized using z-scores, i.e., by subtracting the mean and dividing by
#'  the standard deviation (ignoring \code{NA} values). If \code{std = FALSE},
#'  the standardized indcator values are not returned. Default is \code{std =
#'  TRUE}. Either \code{raw} or \code{std} must be \code{TRUE}.
#'@param glob.env Logical value indicating whether to return output to global
#'  environment. Default is \code{glob.env = TRUE}.
#'@param export.path File path indicating where to save a .csv file of
#'  calculated indicators (named structure_export.id.csv; see below). If
#'  \code{export.file = NULL}, the indicator dataframe will not be exported as a
#'  .csv file. Default is \code{export.path = NULL}.
#'@param export.id Character string to modify the name of the .csv file (if
#'  export.path is specified), for example an area name or date of analysis. The
#'  exported .csv file is named structure_export.id.csv. Default is
#'  \code{export.id = NULL}.
#'@return Returns a dataframe with columns \code{ID}, \code{YEAR}, and
#'  indicators corresponding to the arguments supplied to the function.
#'  Standardized indicators are noted with \code{_s} in the name.
#'@family ecosystem structure and function indicators
#'@references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'@author  Danielle Dempsey \email{Danielle.Dempsey@@dfo-mpo.gc.ca}, Adam Cook,
#'  Catalina Gomez, Alida Bundy
#'@examples
#'# Compile data
#'data(X)
#'data(X_length)
#'data(species.table)
#'data(species.info)
#'data(Length_Weight)
#'
#'# Species groups of interest
#'ratio.groups <- data.frame(rbind(c("PELAGIC", "GROUNDFISH"), c("PREDATORS", "ALL")))
#'names(ratio.groups) <- c("group1", "group2")
#'trophicguild.groups <- c("LBENTHIVORE", "MBENTHIVORE", "PISCIVORE", "PLANKTIVORE",
#'    "ZOOPISCIVORE")
#'condition.groups <- c("FINFISH", "LBENTHIVORE", "MBENTHIVORE", "PISCIVORE",
#'     "PLANKTIVORE", "ZOOPISCIVORE")
#'
#'# Calculate raw indicators
#'allStructure(X = X, X_length = X_length,
#'    LSI.group = "ALL", LFI.group = "ALL",
#'    guild.groups = trophicguild.groups, condition.groups = condition.groups,
#'    ratio.groups = ratio.groups,
#'    species.table = species.table, speciesinfo.table = species.info,
#'    LenWt.table = Length_Weight,
#'    max.length = 85, years = c(2014:2019), raw = TRUE, std = FALSE)
#'@export

allStructure <- function(X, X_length,
                         LSI.group, LFI.group,
                         guild.groups, condition.groups, ratio.groups,
                         species.table, speciesinfo.table, LenWt.table,
                         max.length, years, raw = TRUE, std = TRUE,
                         glob.env = TRUE, export.path = NULL, export.id = NULL){

  if(glob.env == FALSE & length(export.path) == 0) {
    stop("error: please specify a valid export.path or set glob.env = TRUE")
  }
  
  if(raw == FALSE & std == FALSE) stop("error: both raw and std are FALSE")
  
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
  
  if(length(export.path) > 0){
    write.csv(inds, file = paste(export.path, "/structure_", 
                                 export.id, ".csv", sep = ""), row.names = FALSE)
  } 
  if(glob.env) inds
}
