#'@title Calculates all Biodiversity indicators
#'@description This function calculates all (or a subset) of the Biodiversity
#'  indicators for \eqn{j} areas and \eqn{i} years.
#'@details This function calculates the Biodiversity indicators: Species
#'  Richness, Shannon's Index of Diversity, Margalef's Species Richness,
#'  Pielou's Species Evenness, Hill's N1, Hill's N2, Heip's Evenness Index, and
#'  Kempton's Q. If trophic level data are not available to calculate Kempton's
#'  Q, the remaining Biodiversity indicators will be returned. See the help file
#'  for the individual functions for information on how each indicator is
#'  calculated.
#'@inheritParams shannon
#'@param metric A character string indicating which column in \code{X} to use to
#'  calculate the indicators. Default is \code{metric = "ABUNDANCE"}.
#'@param group A character string indicating the species group for which to
#'  calculate the indicators. If \code{group = "ALL"}, all species will be
#'  included; otherwise, \code{group} should match a column name in
#'  \code{species.table}.
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
#'@return Returns a dataframe with columns \code{ID}, \code{YEAR}, and
#'  indicators corresponding to the arguments supplied to the function.
#'
#'  If there is no data for an indicator at spatial scale \eqn{j} in year
#'  \eqn{i}, indicator value is assigned \code{NA}.
#'@importFrom stats aggregate
#'@family biodiversity indicators
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@export

allBiodiversity <- function(X, metric = "ABUNDANCE", group = "ALL", species.table = NULL, TL.table, 
                            percentiles = c(.25, 0.75), minTL = 0, years){
  
  S = speciesRichness(X = X, metric = metric, group = group, species.table = species.table, years = years)
  H = shannon(X = X, metric = metric, group = group, species.table = species.table, years = years)
  marg = margalef(X = X, metric = metric, group = group, species.table = species.table, years = years)
  pie = pielouEvenness(X = X, metric = metric, group = group, species.table = species.table, years = years)
  H1 = hillN1(X = X, metric = metric, group = group, species.table = species.table, years = years)
  H2 = hillN2(X = X, metric = metric, group = group, species.table = species.table, years = years)
  Heips = heips(X = X, metric = metric, group = group, species.table = species.table, years = years)

  inds <- cbind(S, H$ShannonDiversity, marg$MargalefRichness_ALL, pie$PielouEvenness, H1$HillDiversity, H2$HillDominance, Heips$Heips)
  names(inds) <- c("ID", "YEAR", names(S)[3], names(H)[3], names(marg)[3], names(pie)[3],
                   names(H1)[3], names(H2)[3], names(Heips)[3])
  
  if("TL" %in% colnames(TL.table)){
    
    Q = kemptonQ(X = X, metric = metric, group = group, species.table = species.table, 
               TL.table = TL.table, years = years, percentiles = percentiles, minTL = minTL)
  
  inds <- cbind(inds, Q$KemptonQ)
  names(inds)[10] <- names(Q)[3]
  }
  
  inds
}
                            