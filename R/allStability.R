#'@title Calculates all Stability and Resistance indicators
#'@description This function calculates all (or a subset) of the Stability and
#'  Resistance indicators for \eqn{j} areas and \eqn{i} years. The user can
#'  choose whether the function returns the indicator dataframe to the global
#'  environment, exports the dataframe to a .csv file, or both. The user can also
#'  choose whether the function returns the raw indicator values, the
#'  standardized (z-score) values, or both.
#'@details This function calculates the Stability and Resistance indicators:
#'  Biomass per Trophic Level, Intrinsic Vulnerability Index, Coefficient of
#'  Variation of the Biomass, Mean Lifespan, and Mean Maximum Length (weighted
#'  by biomass and abundance). If data are not available to calculate one or
#'  more of these indicators, a subset will be returned. See the help file for
#'  the individual functions for information on how each indicator is
#'  calculated.
#'
#'
#'  Notes on indicator calculations: In the individual functions, the user
#'  generally has control over which metric (e.g., \code{BIOMASS} or
#'  \code{ABUNDANCE}) is used to calculate the indicator.  Here, \code{BIOMASS}
#'  is used to calculate Coefficient of Variation of the Biomass, Mean Lifespan,
#'  Biomass per Trophic Level, and Mean Maximum Length weighted by biomass.
#'  \code{ABUNDANCE} is used to calculate Mean Maximum Length weighted by
#'  abundance. See individual functions for more flexibility.
#'@inheritParams landings
#'@inheritParams communityCondition
#'@inheritParams allStructure
#'@param X A dataframe of fishery independent data derived from research vessel
#'  survey data or model output, with columns \code{YEAR}, \code{ID},
#'  \code{SPECIES}, \code{BIOMASS}, and \code{ABUNDANCE}. \code{YEAR}
#'  indicates the year the observation was recorded, \code{ID} is an area code
#'  indicating where the observation was recorded, \code{SPECIES} is a numeric
#'  code indicating the species sampled, and \code{BIOMASS}/\code{ABUNDANCE} is
#'  the corresponding biomass/abundance (stratified and corrected for
#'  catchability as required).
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
#'  corresponding \code{TL}, \code{MAXLENGTH}, \code{MAXAGE}, and \code{IVI}
#'  (trophic level, maximum recorded age, maximum recorded length, and intrinsic
#'  vulnerability index). Entries in the \code{SPECIES} column should be the
#'  unique values of species codes in \code{X} (or a subset thereof). If there
#'  are different species codes in \code{X} and \code{land}, the Intrinsic
#'  Vulnerability Index of the Landings indicator should be calculated
#'  separately using the function \code{IVILandings()}.
#'@param TL.grouping Size of the trophic level bin for which to aggregate
#'  biomass when calculating Biomass per Trophic Level. For example, if
#'  \code{TL.grouping = 1}, trophic levels are binned from 1.00 - 1.99, 2.00 -
#'  2.99, etc. If TL.grouping = 0.5, trophic levels are binned from 1.00 - 1.49,
#'  1.50 - 1.99, 2.00 - 2.49, 2.50 - 2.99, etc. Default is \code{TL.grouping =
#'  1} so that biomass is aggregated over discrete trophic levels.
#'@param wind Window for the moving average used to calculate the Coefficient of
#'  Variation of the Biomass. The first and last \code{floor(wind/2)} values of
#'  the indicator are assigned \code{NA} to account for the moving average.
#'  Default is \code{wind = 5} years.
#'@param negative If \code{negative = TRUE}, the Coefficient of Variation of the
#'  Biomass will be multiplied by -1 so that the expected response is to
#'  decrease with increasing fishing pressure. Default is \code{negative =
#'  FALSE}.
#'@param years A vector of years for which to calculate indicators.
#'@param export.path File path indicating where to save a .csv file of
#'  calculated indicators (named stability_export.id.csv; see below). If
#'  \code{export.file = NULL}, the indicator dataframe will not be exported as a
#'  .csv file. Default is \code{export.path = NULL}.
#'@param export.id Character string to modify the name of the .csv file (if
#'  export.path is specified), for example an area name or date of analysis. The
#'  exported .csv file is named stability_export.id.csv. Default is
#'  \code{export.id = NULL}.
#'@return Returns a dataframe with columns \code{ID}, \code{YEAR}, and
#'  indicators corresponding to the arguments supplied to the function.
#'  Standardized indicators are noted with \code{_s} in the name.
#'@family stability and resistance indicators
#'@references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'@author  Danielle Dempsey \email{Danielle.Dempsey@@dfo-mpo.gc.ca}, Adam Cook,
#'  Catalina Gomez, Alida Bundy
#'@examples
#'# Compile data
#'data(X)
#'data(land)
#'data(species.info)
#'data(species.table)
#'
#'# Calculate raw and standardized indicators
#'allStability(X = X, land = land, maxlength.group = "FINFISH",
#'    species.table = species.table, speciesinfo.table = species.info,TL.grouping = 1,
#'    wind = 5, negative = FALSE, years = c(2014:2019), raw = TRUE, std = TRUE)
#'@export


allStability <- function(X, land, 
                         maxlength.group,
                         species.table, speciesinfo.table,  
                         TL.grouping = 1, wind = 5, negative = FALSE, 
                         years,  raw = TRUE, std = TRUE,
                         glob.env = TRUE, export.path = NULL, export.id = NULL){
  
  if(glob.env == FALSE & length(export.path) == 0) {
    stop("error: please specify a valid export.path or set glob.env = TRUE")
  }
  
  if(raw == FALSE & std == FALSE) stop("error: both raw and std are FALSE")
  
  if("BIOMASS" %in% colnames(X)) {
    inds <- createDataframe(unique(X$ID), years)
  } else {inds <- createDataframe(unique(land$ID), years)}
  
  
  if("BIOMASS" %in% colnames(X)){
    
    # CV biomass
    CV_bio = CVBiomass(X = X, wind = wind, negative = negative, years = years)
    inds <- merge(inds, CV_bio, all.x = TRUE)
    
    # Max LifeSpan
    if("MAXAGE" %in% colnames(speciesinfo.table)){
      MMA = meanMaxAge(X = X, age.table = speciesinfo.table, metric = "BIOMASS", years = years)
      inds <- merge(inds, MMA, all.x = TRUE)
    }
    
    # Biomass per TL
    if("TL" %in% colnames(speciesinfo.table)){
      bio_TL = biomassPerTL(X = X, TL.table = speciesinfo.table, metric = "BIOMASS", 
                            TL.grouping = TL.grouping, years = years)
      inds <- merge(inds, bio_TL, all.x = TRUE)
    }
    
    if("MAXLENGTH" %in% colnames(speciesinfo.table) & length(maxlength.group) > 0){
      MML_bio = meanMaxLength(X = X, group = maxlength.group, species.table = species.table,
                        maxlength.table = speciesinfo.table, metric = "BIOMASS", years = years)
      inds <- merge(inds, MML_bio, all.x = TRUE)
    }
 
    if("ABUNDANCE" %in% colnames(X)){
      MML_abund = meanMaxLength(X = X, group = maxlength.group,  species.table = species.table,
                          maxlength.table = speciesinfo.table, metric = "ABUNDANCE", years = years)
      inds <- merge(inds, MML_abund, all.x = TRUE)
    }
  }
  
  # Landings-based indicators
  if("CATCH" %in% colnames(land)){
    
    # IVI Landings
    if("IVI" %in% colnames(speciesinfo.table)){
      IVI = IVILandings(land, IVI.table = speciesinfo.table, negative = negative, years = years)
      inds <- merge(inds, IVI, all.x = TRUE)
    }
  }
  
  if(std == TRUE){
    inds_std <-  standardize(inds)
    
    if(raw == FALSE) inds <- inds_std
    if(raw == TRUE) inds <- merge(inds, inds_std)
  }
  
  if(length(export.path) > 0){
    write.csv(inds, file = paste(export.path, "/stability_", 
                                 export.id, ".csv", sep = ""), row.names = FALSE)
  } 
  if(glob.env) inds
}
  
  
  
  

