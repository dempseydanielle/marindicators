#'@title Calculates indicators from all attributes
#'@description This function calculates all of the indicators described in this
#'  package. The user can choose whether the function returns the indicator
#'  dataframe to the global environment, exports the dataframe to a .csv file,
#'  or both. The user can also choose whether the function returns the raw
#'  indicator values, the standardized (z-score) values, or both.
#'@details This function calculates the indicators for each attribute and
#'  pressure: Biodiversity, Structure and Functioning, Stability and Resistance,
#'  Resource Potential, and Fishing Pressure.
#'
#'  See the help file for the individual functions for information on how each
#'  indicator is calculated.
#'@inheritParams allStructure
#'@inheritParams allBiodiversity
#'@inheritParams allStability
#'@inheritParams allPotential
#'@inheritParams allPressure
#'@param X A dataframe of fishery independent data derived from research vessel
#'  survey data or model output, with columns \code{YEAR}, \code{ID},
#'  \code{SPECIES}, \code{BIOMASS} and \code{ABUNDANCE}. \code{YEAR} indicates
#'  the year the observation was recorded, \code{ID} is an area code indicating
#'  where the observation was recorded, \code{SPECIES} is a numeric code
#'  indicating the species sampled, and \code{ABUNDANCE} is the corresponding
#'  abundance (stratified and corrected for catchability as required).
#'@param X_length A dataframe of fishery independent data derived from research
#'  vessel survey data or model output, with columns \code{YEAR}, \code{ID},
#'  \code{SPECIES}, \code{LENGTH}, \code{BIOMASS} and \code{ABUNDANCE}.
#'  \code{YEAR} indicates the year the observation was recorded, \code{ID} is an
#'  area code indicating where the observation was recorded, and \code{SPECIES}
#'  is a numeric code indicating the species sampled. \code{LENGTH} is the
#'  length class (cm) and \code{ABUNDANCE} is the corresponding abundance at
#'  length (stratified and corrected for catchability as required). Species for
#'  which there are no length data should be assigned \code{LENGTH = -99}. These
#'  observations are removed by the function.
#'@param land A dataframe of commercial landings data with columns \code{YEAR},
#'  \code{ID}, \code{SPECIES} and \code{CATCH}. \code{YEAR} indicates the year
#'  the landing was recorded, \code{ID} is an area code indicating where the
#'  landing was recorded, \code{SPECIES} is a numeric code indicating the
#'  species landed, and \code{CATCH} is the corresponding landed weight. If
#'  \code{land = NULL}, the landings-based indicators will not be calculated.
#'@param speciesinfo.table A table with columns \code{SPECIES} and the
#'  corresponding \code{TL}, \code{MAXLENGTH}, \code{MAXAGE},  \code{IVI}, and
#'  \code{TL_LAND} (trophic level, maximum recorded age, maximum recorded
#'  length, intrinsic vulnerability index, and trophic level of the landings).
#'  Entries in the \code{SPECIES} column should be the unique values of species
#'  codes in \code{X}/\code{X_length} (or a subset thereof). If there are
#'  different species codes in \code{X} and \code{land}, the Fishing-in-Balance,
#'  Intrinsic Vulnerability Index of Landings, Mean Trophic Level of the
#'  Landings, and Marine Trophic Index should be calculated using their
#'  respective single functions (see manual or vignette).
#'@param species.table A table where the column names match the entries in
#'  \code{condition.groups}, \code{FP.groups}, \code{group.bio},
#'  \code{guild.groups}, \code{landings.group}, \code{LFI.group},
#'  \code{LSI.group}, \code{maxlength.group}, \code{ratio.groups}, and
#'  \code{resource.groups}. Column entries are the species codes
#'  indicating the species from \code{X} (or \code{X_length}) included in each
#'  group. \code{species.table} may also include columns for other species
#'  groups; these will be ignored.
#'@param metric.bio A character string indicating which column in \code{X} to
#'  use to calculate the biodiversity indicators. Default is \code{metric =
#'  "ABUNDANCE"}.
#'@param group.bio A character string indicating which species to include in the
#'  calculation of the Biodiversity indicators. If \code{group = "ALL"}, all
#'  species will be included; otherwise, \code{group.bio} should match a column
#'  name in \code{species.table}. Default is \code{group.bio = "ALL"}.
#'@param minTL.bio Minimum trophic level for species included to calculate
#'  Kempton's Q. Default is \code{minTL.bio = 0}.
#'@param negative If \code{negative = TRUE}, the Coefficient of Variation of the
#'  Biomass and the Intrinsic Vulnerability Index of the Landings will be
#'  multiplied by -1 so that their expected response is to decrease with
#'  increasing fishing pressure. Default is \code{negative = FALSE}.
#'@param minTL.FiB The minimum trophic level of species to include to calculate
#'  Fishing-in-Balance.
#'@param minTL.FP A vector containing minimum trophic level to include when
#'  calculating the mean trophic level of the landings. Default is \code{minTL =
#'  c(0, 3.25)}, which will return the mean trophic level of the landings and
#'  the marine trophic index.
#'@param export.path File path indicating where to save a .csv file of
#'  calculated indicators (named allIndicators_export.id.csv; see below). If
#'  \code{export.file = NULL}, the indicator dataframe will not be exported as a
#'  .csv file. Default is \code{export.path = NULL}.
#'@param export.id Character string to modify the name of the .csv file (if
#'  export.path is specified), for example an area name or date of analysis. The
#'  exported .csv file is named allIndicators_export.id.csv. Default is
#'  \code{export.id = NULL}.
#'@return Returns a dataframe with columns \code{ID}, \code{YEAR}, and
#'  indicators corresponding to the arguments supplied to the function.
#'  Standardized indicators are noted with "_s" in the name.
#'@importFrom stats aggregate
#'@importFrom utils write.csv
#'@author  Danielle Dempsey \email{Danielle.Dempsey@@dfo-mpo.gc.ca}, Adam Cook,
#'  Catalina Gomez, Alida Bundy
#'@export
#'@examples
#'# Compile data
#'data(X)
#'data(X_length)
#'data(land)
#'data(species.table)
#'data(species.info)
#'data(Length_Weight)
#'
#'# Species groups of interest
#'trophicguild.groups <- c("LBENTHIVORE", "MBENTHIVORE", "PISCIVORE", "PLANKTIVORE",
#'    "ZOOPISCIVORE")
#'condition.groups <- c("FINFISH", "PISCIVORE", "PLANKTIVORE", "ZOOPISCIVORE")
#'resource.groups <- c("ALL", "CLUPEIDS", "FINFISH", "FLATFISH", "FORAGE",
#'    "GADOIDS", "GROUNDFISH", "PELAGIC", "SKATES")
#'ratio.groups <- data.frame(rbind(c("PELAGIC", "GROUNDFISH"), c("PREDATORS", "ALL")))
#'names(ratio.groups) <- c("group1", "group2")
#'
#'landings.groups <- c("ALL", "CLUPEIDS.L", "FLATFISH.L", "GROUNDFISH.L")
#'FP.groups <- data.frame(rbind(c("ALL", "ALL"),
#'     c("CLUPEIDS", "CLUPEIDS.L"),
#'     c("FLATFISH", "FLATFISH.L"),
#'     c("GROUNDFISH", "GROUNDFISH.L")))
#' names(FP.groups) <- c("group.X", "group.land")
#'
#'# Calculate raw and standardized indicators
#' extractAll(X = X, X_length = X_length, land = land,
#'     speciesinfo.table = species.info, species.table = species.table, years = c(2014:2019),
#'     LSI.group = "ALL", LFI.group = "ALL", LenWt.table = Length_Weight,
#'     guild.groups = trophicguild.groups, condition.groups = condition.groups,
#'     ratio.groups = ratio.groups,
#'     maxlength.group = "FINFISH",
#'     resource.groups = resource.groups,
#'     minTL.FiB = 0, base.start = 2014, base.end = 2015,
#'     landings.groups = landings.groups, FP.groups = FP.groups,
#'     raw = TRUE, std = TRUE, export.path = NULL)

extractAll <- function(X, X_length, land, 
                       speciesinfo.table, species.table,  years,
                       # biodiversity
                       metric.bio = "ABUNDANCE", group.bio = "ALL", 
                       percentiles = c(.25, 0.75), minTL.bio = 0,
                       # structure
                       LSI.group, max.length = 85,
                       LFI.group, large.fish = 35, LenWt.table,
                       guild.groups, condition.groups, ratio.groups, 
                       # stability
                       maxlength.group, TL.grouping = 1, wind = 5, negative = FALSE,
                       # potential
                       resource.groups,
                       minTL.FiB = 0, TE = 0.1, base.start, base.end,
                       # pressure
                       landings.groups, FP.groups, minTL.FP = c(0, 3.25),
                       # return
                       raw = TRUE, std = TRUE, glob.env = TRUE,
                       export.path = NULL, export.id = NULL){
  
  if(glob.env == FALSE & length(export.path) == 0) {
    stop("error: please specify a valid export.path or set glob.env = TRUE")
  }
   
  inds <- createDataframe(unique(X$ID), years)
  
  diversity <- allBiodiversity(X, metric = metric.bio, groups = group.bio, 
                               TL.table = speciesinfo.table, species.table = species.table,
                               percentiles = c(0.25, 0.75), minTL = minTL.bio, 
                               years = years, raw = raw, std = std,
                               export.path = NULL, export.id = NULL, glob.env = TRUE)

  
  structure <- allStructure(X = X, X_length = X_length, 
                            LSI.group = LSI.group, LFI.group = LFI.group, 
                            guild.groups = guild.groups, condition.groups = condition.groups, 
                            ratio.groups = ratio.groups,
                            species.table = species.table, speciesinfo.table = speciesinfo.table, 
                            LenWt.table = LenWt.table,
                            max.length = max.length, years = years, raw = raw, std =  std,
                            export.path = NULL, export.id = NULL, glob.env = TRUE)

  stability <- allStability(X = X, land = land, 
                            maxlength.group = maxlength.group,
                            species.table = species.table, speciesinfo.table = speciesinfo.table,  
                            TL.grouping = TL.grouping, wind = wind, negative = negative, 
                            years = years, raw = raw, std = std,
                            export.path = NULL, export.id = NULL, glob.env = TRUE)
  
  potential <- allPotential(X = X, land = land, 
                            species.table = species.table, speciesinfo.table = speciesinfo.table, 
                            resource.groups = resource.groups, 
                            minTL = minTL.FiB, TE = TE, base.start = base.start, base.end = base.end, 
                            years = years, raw = raw, std = std,
                            export.path = NULL, export.id = NULL, glob.env = TRUE)
  
  if("CATCH" %in% colnames(land)) {
  pressure <- allPressure(X = X, land = land, 
                          species.table = species.table, speciesinfo.table = speciesinfo.table, 
                          landings.groups = landings.groups, FP.groups = FP.groups,
                          minTL = c(0, 3.25),  years, raw = raw, std = std,
                          export.path = NULL, export.id = NULL, glob.env = TRUE)
  }
  
  inds <- merge(inds, diversity, all.x = TRUE)
  inds <- merge(inds, structure, all.x = TRUE)
  inds <- merge(inds, stability, all.x = TRUE)
  inds <- merge(inds, potential, all.x = TRUE)
  if("CATCH" %in% colnames(land)) inds <- merge(inds, pressure, all.x = TRUE)

  if(raw == TRUE & std == TRUE){
    
    std_cols <- grep("_s", names(inds))
    inds_std <- inds[, std_cols]
    inds_raw <- inds[, -std_cols]
    
    inds <- cbind(inds_raw, inds_std)
  }
  
  if(length(export.path) > 0){
    write.csv(inds, file = paste(export.path, "/allIndicators_", 
                                 export.id, ".csv", sep = ""), row.names = FALSE)
  } 
  if(glob.env) inds
  
}

