#'@title Calculates all Resource Potential indicators
#'@description This function calculates all (or a subset) of the Resource
#'  Potential indicators for \eqn{j} areas and \eqn{i} years. The user can
#'  choose whether the function returns the indicator dataframe to the global
#'  environment, exports the dataframe to a .csv file, or both. The user can
#'  also choose whether the function returns the raw indicator values, the
#'  standaradized (z-score) values, or both.
#'@details This function calculates the Resource Potential indicators: Abundance
#'  and Biomass of the community, Resource Potential of predefined species
#'  groups, and Fishing-in-Balance (FiB). If data are not available to calculate
#'  one or more of these indicators, a subset will be returned. See the help
#'  file for the individual functions for information on how each indicator is
#'  calculated.
#'@inheritParams allStructure
#'@inheritParams resourcePotential
#'@inheritParams landings
#'@inheritParams fishingInBalance
#'@param X A dataframe of fishery independent data derived from research vessel
#'  survey data or model output, with columns \code{YEAR}, \code{ID},
#'  \code{SPECIES}, \code{BIOMASS}, and \code{ABUNDANCE}. \code{YEAR} indicates
#'  the year the observation was recorded, \code{ID} is an area code indicating
#'  where the observation was recorded, \code{SPECIES} is a numeric code
#'  indicating the species sampled, and \code{BIOMASS}/\code{ABUNDANCE} is the
#'  corresponding biomass/abundance (stratified and corrected for catchability
#'  as required).
#'@param resource.groups A vector indicating the species groups for which to
#'  calculate the resource potential. Each entry must be a character string
#'  matching the name of a column in \code{species.table}. If
#'  \code{resource.groups = NULL}, these indicators will not be calculated.
#'@param species.table A table where the column names match the entries in
#'  \code{resource.groups}. Column entries are the species codes indicating the
#'  species from \code{X} included in each group. \code{species.table} may also
#'  include columns for other species groups; these will be ignored.
#'@param speciesinfo.table A table with columns \code{SPECIES} and the
#'  corresponding \code{TL_LAND} (trophic level of landed species). Entries in
#'  the \code{SPECIES} column should be the unique values of species codes in
#'  \code{land} (or a subset thereof). Other columns will be ignored.
#'@param minTL The minimum trophic level of species to include to calculate FiB.
#'  If \code{minTL = NULL}, FiB wll not be calculated. Default is \code{minTL =
#'  0}.
#'@param TE Trophic efficiency, used to calculate FiB. Default is \code{TE =
#'  0.1}, i.e., a trophic efficiency of 10\%. If \code{TE = NULL}, FiB wll not
#'  be calculated.
#'@param base.start Year indicating the beginning of the baseline period for
#'  calculating FiB. The average landings and average mean trophic level of the
#'  landings over the baseline period are used as baseline values to calculate
#'  FiB. \code{land} must include data for the baseline period. If
#'  \code{base.start = NULL}, FiB will not be calculated.
#'@param base.end Year indicating the end of the baseline period for calculating
#'  FiB. The average landings and average mean trophic level of the landings
#'  over the baseline period are used as baseline values to calculate FiB.
#'  \code{land} must include data for the baseline period. If \code{base.end =
#'  NULL}, FiB will not be calculated.
#'@param export.path File path indicating where to save a .csv file of
#'  calculated indicators (named potential_export.id.csv; see below). If
#'  \code{export.file = NULL}, the indicator dataframe will not be exported as a
#'  .csv file. Default is \code{export.path = NULL}.
#'@param export.id Character string to modify the name of the .csv file (if
#'  export.path is specified), for example an area name or date of analysis. The
#'  exported .csv file is named potential_export.id.csv. Default is
#'  \code{export.id = NULL}.
#'@return Returns a dataframe with columns \code{ID}, \code{YEAR}, and
#'  indicators corresponding to the arguments supplied to the function.
#'  Standardized indicators are noted with \code{_s} in the name.
#'@family resource potential indicators
#'@references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'@author  Danielle Dempsey \email{Danielle.Dempsey@@dfo-mpo.gc.ca}, Adam Cook,
#'  Catalina Gomez, Alida Bundy
#'@examples
#'# Compile data
#'data(X)
#'data(land)
#'data(species.table)
#'data(species.info)
#'
#'# Species groups of interest
#'resource.groups <- c("ALL", "CLUPEIDS", "FINFISH", "FLATFISH",
#'    "FORAGE",  "GADOIDS", "GROUNDFISH")
#'
#'# Calculate standardized indicators
#'allPotential(X = X, land = land, species.table = species.table,
#'    speciesinfo.table = species.info, resource.groups = resource.groups,
#'    TE = 0.1, base.start = 2014, base.end = 2015, years = c(2014:2019),
#'    raw = FALSE, std = TRUE)
#'
#'@export


allPotential <- function(X, land, 
                         species.table, speciesinfo.table, resource.groups,
                         minTL = 0, TE = 0.1, base.start, base.end, 
                         years,  raw = TRUE, std = TRUE,
                         glob.env = TRUE, export.path = NULL, export.id = NULL){
  
  if(glob.env == FALSE & length(export.path) == 0) {
    stop("error: please specify a valid export.path or set glob.env = TRUE")
  }
  
  if(raw == FALSE & std == FALSE) stop("error: both raw and std are FALSE")
  
  if("BIOMASS" %in% colnames(X)) {
    inds <- createDataframe(unique(X$ID), years)
  } else inds <- createDataframe(unique(land$ID), years)

  # Abundance
  if("ABUNDANCE" %in% colnames(X)){
    abund = resourcePotential(X, metric = "ABUNDANCE", groups = "ALL",
                          species.table = species.table, years = years)
    inds <- merge(inds, abund, all.x = TRUE)
  }
  
  # Biomass 
  if("BIOMASS" %in% colnames(X) & length(resource.groups)){
      
        potential <- resourcePotential(X, metric = "BIOMASS", groups = resource.groups,
                                       species.table = species.table, years = years)
        inds <- merge(inds, potential, all.x = TRUE)
  }

  # Fishing in Balance
  if("CATCH" %in% colnames(land) & length(TE) > 0 & length(minTL) >0
     &length(base.start) > 0 & length(base.end) >0){
    FIB = fishingInBalance(land, TE = TE,  base.start = base.start, base.end = base.end, 
                          TL.table = speciesinfo.table,  minTL = minTL,  years = years)
    
    inds <- merge(inds, FIB, all.x = TRUE)
  }

  if(std == TRUE){
    inds_std <-  standardize(inds)
    
    if(raw == FALSE) inds <- inds_std
    if(raw == TRUE) inds <- merge(inds, inds_std)
  }
  
  if(length(export.path) > 0){
    write.csv(inds, file = paste(export.path, "/potential_", 
                                 export.id, ".csv", sep = ""), row.names = FALSE)
  } 
  if(glob.env) inds
}




