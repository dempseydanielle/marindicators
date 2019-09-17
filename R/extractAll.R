#'@title Calculates indicators from all attributes
#'@description This function calculates all of the indicators described in this
#'  package. The user can choose whether the function returns the indicator
#'  dataframe to the global environment, or exports the dataframe to a csv file.
#'  The user can also choose whether the function returns the raw indicator
#'  values, the standaradized (z-score) values, or both.
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
#'@param metric.bio A character string indicating which column in \code{X} to
#'  use to calculate the biodiversity indicators. Default is \code{metric =
#'  "ABUNDANCE"}.
#'@param group.bio A character string indicating which species to include in the
#'  calculation of the Biodiversity indicators. If \code{group = "ALL"}, all
#'  species will be included; otherwise, \code{group.bio} should match a column
#'  name in \code{species.table}. Default is \code{group.bio = "ALL"}.
#'@param minTL.bio Minimum trophic level for species included to calculate
#'  Kempton's Q. Default is \code{minTL.bio = 0}.
#'@param minTL.FiB The minimum trophic level of species to include to calculate
#'  Fishing-in-Balance.
#'@param minTL.FP A vector containing minimum trophic level to include when
#'  calculating the mean trophic level of the landings. Default is \code{minTL =
#'  c(0, 3.25)}, which will return the mean trophic level of the landings and
#'  the marine trophic index.
#'@param export.path File path indicating where to save a .csv file of
#'  calculated indicators (named allIndicators.csv). If \code{export.file =
#'  NULL}, the indicator dataframe will be returned to the global environment,
#'  but not exported as a .csv file.
#'@return Returns a dataframe with columns \code{ID}, \code{YEAR}, and
#'  indicators corresponding to the arguments supplied to the function.
#'  Standardized indicators are noted with "_s" in the name.
#'@importFrom stats aggregate
#'@importFrom utils write.csv
#'@family biodiversity indicators
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@export
#'@examples
#'data(X)
#'data(X_length)
#'data(land)
#'data(species.groups)
#'data(species.info)
#'data(Length_Weight)
#'
#'trophicguild.groups <- c("LBENTHIVORE", "MBENTHIVORE", "PISCIVORE", "PLANKTIVORE",
#'    "ZOOPISCIVORE")
#'condition.groups <- c("FINFISH", "LBENTHIVORE", "MBENTHIVORE", "PISCIVORE",
#'    "PLANKTIVORE", "ZOOPISCIVORE")
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
#' extractAll(X = X, X_length = X_length, land = land,
#'     speciesinfo.table = species.info, species.table = species.groups,
#'     LenWt.table = Length_Weight,
#'     LSI.group = "ALL", LFI.group = "ALL",
#'     guild.groups = trophicguild.groups,
#'     resource.groups = resource.groups, condition.groups = condition.groups,
#'     ratio.groups = ratio.groups,
#'     max.length = 85, maxlength.group = "FINFISH",
#'     minTL.FiB = 0, base.start = 2014, base.end = 2015,
#'     landings.groups = landings.groups, FP.groups = FP.groups,
#'     years = c(2014:2019),
#'     raw = TRUE, std = TRUE, export.path = NULL)

extractAll <- function(X, X_length, land, 
                       speciesinfo.table, species.table, LenWt.table, years,
                       metric.bio = "ABUNDANCE", group.bio = "ALL", 
                       percentiles = c(.25, 0.75), minTL.bio = 0,
                       LSI.group, LFI.group, 
                       guild.groups, resource.groups, condition.groups, 
                       ratio.groups, max.length,
                       maxlength.group, TL.grouping = 1, wind = 5, negative = FALSE,
                       minTL.FiB, TE = 0.1, base.start, base.end,
                       landings.groups, FP.groups, minTL.FP = c(0, 3.25),
                       raw = TRUE, std = TRUE,
                       export.path = NULL){
  
  inds <- createDataframe(unique(X$ID), years)
  
  diversity <- allBiodiversity(X, metric = metric.bio, group = group.bio, 
                               TL.table = speciesinfo.table, 
                               percentiles = c(0.25, 0.75), minTL = minTL.bio, 
                               years = years, raw = FALSE, std = FALSE)
  
  structure <- allStructure(X = X, X_length = X_length, 
                            LSI.group = LSI.group, LFI.group = LFI.group, 
                            guild.groups = guild.groups, condition.groups = condition.groups, 
                            ratio.groups = ratio.groups,
                            species.table = species.table, speciesinfo.table = speciesinfo.table, 
                            LenWt.table = LenWt.table,
                            max.length = max.length, years = years, raw = raw, std = std)
  
  stability <- allStability(X = X, land = land, 
                            maxlength.group = maxlength.group,
                            species.table = species.table, speciesinfo.table = speciesinfo.table,  
                            TL.grouping = TL.grouping, wind = wind, negative = negative, 
                            years = years, raw = raw, std = std)
  
  potential <- allPotential(X = X, land = land, 
                            species.table = species.table, speciesinfo.table = speciesinfo.table, 
                            resource.groups = resource.groups, 
                            minTL = minTL.FiB, TE = 0.1, base.start = base.start, base.end = base.end, 
                            years = years, raw = raw, std = std)
  
  pressure <- allPressure(X = X, land = land, 
                          species.table = species.table, speciesinfo.table = speciesinfo.table, 
                          landings.groups = landings.groups, FP.groups = FP.groups,
                          minTL = c(0, 3.25),  years, raw = raw, std = std)
  
  
  inds <- merge(inds, diversity, all.x = TRUE)
  inds <- merge(inds, structure, all.x = TRUE)
  inds <- merge(inds, stability, all.x = TRUE)
  inds <- merge(inds, potential, all.x = TRUE)
  inds <- merge(inds, pressure, all.x = TRUE)
  
  # if(raw == FALSE && std == FALSE) print("error: both raw and std are FALSE")
  # 
  # if(std == TRUE){
  #   inds_std <-  standardize(inds)
  #   
  #   if(raw == FALSE) inds <- inds_std
  #   if(raw == TRUE) inds <- merge(inds, inds_std)
  # }

  if(length(export.path) > 0){
    write.csv(inds, file = paste(export.path, "/allIndicators.csv", sep = ""), row.names = FALSE)
  } else  inds
  
}

