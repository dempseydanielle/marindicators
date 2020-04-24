#'@title Calculates all Biodiversity indicators
#'@description This function calculates all (or a subset) of the Biodiversity
#'  indicators for \eqn{j} areas and \eqn{i} years. The user can choose whether
#'  the function returns the indicator dataframe to the global environment,
#'  exports the dataframe to a csv file, or both. The user can also choose
#'  whether the function returns the raw indicator values, the standardized
#'  (z-score) values, or both.
#'@details This function calculates the Biodiversity indicators: Species
#'  Richness, Shannon's Index of Diversity, Margalef's Species Richness,
#'  Pielou's Species Evenness, Hill's N1, Hill's N2, Heip's Evenness Index, and
#'  Kempton's Q. If trophic level data are not available to calculate Kempton's
#'  Q, the remaining Biodiversity indicators will be returned. See the help file
#'  for the individual functions for information on how each indicator is
#'  calculated.
#'
#'  Notes on indicator calculations: In the individual functions, the user
#'  generally has control over which metric (e.g., \code{"BIOMASS"} or
#'  \code{"ABUNDANCE"}) and group (e.g., \code{"ALL"} or \code{"GROUNDFISH"})
#'  are used to calculate the indicator. Here, the same metric and group are
#'  used to calculate each indicator. Defaults are \code{metric = "ABUNDANCE"}
#'  and \code{group = "ALL"}.
#'@inheritParams shannon
#'@inheritParams allStructure
#'@param metric A character string indicating which column in \code{X} to use to
#'  calculate the indicators. Default is \code{metric = "ABUNDANCE"}.
#'@param groups A vector indicating the species group(s) for which to calculate
#'  the indicators. If \code{groups = "ALL"}, all species will be included;
#'  otherwise, each entry must be a character string matching the name of a
#'  column in \code{species.table}. Default is \code{groups = "ALL"}.
#'@param TL.table A dataframe with columns \code{SPECIES} and the corresponding
#'  \code{TL} (trophic level). Entries in the \code{SPECIES} column should be
#'  the unique values of species codes in \code{X} (or a subset thereof). Other
#'  columns in \code{TL.table} are ignored. If \code{TL.table = NULL}, Kempton's
#'  Q will not be calculated.
#'@param percentiles The percentiles used to determine R1 and R2 for calculating
#'  Kempton's Q. Default is \code{percentiles = c(0.25, 0.75)}.
#'@param minTL Minimum trophic level for species included to calculate Kempton's
#'  Q. Default is \code{minTL = 0}.
#'@param years A vector of years for which to calculate indicators.
#'@param export.path File path indicating where to save a .csv file of
#'  calculated indicators (named biodiversity_export.id.csv; see below). If
#'  \code{export.file = NULL}, the indicator dataframe will not be exported as a
#'  .csv file. Default is \code{export.path = NULL}.
#'@param export.id Character string to modify the name of the .csv file (if
#'  export.path is specified), for example an area name or date of analysis. The
#'  exported .csv file is named biodiversity_export.id.csv. Default is
#'  \code{export.id = NULL}.
#'@return Returns a dataframe with columns \code{ID}, \code{YEAR}, and
#'  indicators corresponding to the arguments supplied to the function.
#'  Standardized indicators are noted with \code{_s} in the name.
#'@importFrom stats aggregate
#'@family biodiversity indicators
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@examples
#'data(X)
#'
#'# Calculate raw and standardized indicators
#'allBiodiversity(X = X, metric = "ABUNDANCE", groups = "ALL", TL.table = species.info,
#'    percentiles = c(.25, 0.75), minTL = 0, years = c(2014:2019), raw = TRUE, std = TRUE)
#'@export

allBiodiversity <- function(X, metric = "ABUNDANCE", groups = "ALL", species.table = NULL, 
                            TL.table, percentiles = c(.25, 0.75), minTL = 0, years, 
                            raw = TRUE, std = TRUE, 
                            glob.env = TRUE, export.path = NULL, export.id = NULL){
  
  
  if(glob.env == FALSE & length(export.path) == 0) {
    stop("error: please specify a valid export.path or set glob.env = TRUE")
  }
  
  if(raw == FALSE & std == FALSE) stop("error: both raw and std are FALSE")
  
  S = speciesRichness(X = X, metric = metric, groups = groups, species.table = species.table, years = years)
  H = shannon(X = X, metric = metric, groups = groups, species.table = species.table, years = years)
  marg = margalef(X = X, metric = metric, groups = groups, species.table = species.table, years = years)
  pie = pielouEvenness(X = X, metric = metric, groups = groups, species.table = species.table, years = years)
  H1 = hillN1(X = X, metric = metric, groups = groups, species.table = species.table, years = years)
  H2 = hillN2(X = X, metric = metric, groups = groups, species.table = species.table, years = years)
  Heips = heips(X = X, metric = metric, groups = groups, species.table = species.table, years = years)

  inds <- merge(S, H)
  inds <- merge(inds, marg)
  inds <- merge(inds, pie)
  inds <- merge(inds, H1)
  inds <- merge(inds, H2)
  inds <- merge(inds, Heips)

  if("TL" %in% colnames(TL.table)){
    
    Q = kemptonQ(X = X, metric = metric, groups = groups, species.table = species.table, 
               TL.table = TL.table, years = years, percentiles = percentiles, minTL = minTL)
  
  inds <- merge(inds, Q)

  }
  

  if(std == TRUE){
    inds_std <-  standardize(inds)
    
    if(raw == FALSE) inds <- inds_std
    if(raw == TRUE) inds <- merge(inds, inds_std)
  }

  if(length(export.path) > 0){
    write.csv(inds, file = paste(export.path, "/biodiversity_", 
                                 export.id, ".csv", sep = ""), row.names = FALSE)
  } 
  if(glob.env) inds
}
                            