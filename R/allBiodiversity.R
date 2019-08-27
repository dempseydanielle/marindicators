#'@title Calculates all biodiversity indicators
#'@description This function calculates all (or a subset of) biodiversity
#'  indicators for \eqn{j} areas and \eqn{i} years.
#'@details This function calculates the biodiversity indicators: species
#'  richness, Shannon's index of diversity, Margalef's species richness,
#'  Pielou's species evenness, Hill's N1, Hill's N2, Heip's evenness index, and
#'  Kempton's Q. If trophic level data are not available to calculate Kempton's
#'  Q, the remaining biodiversity indicators will be returned. See the help file
#'  for the individual functions for information on how each indicator is
#'  calculated.
#'@inheritParams shannon
#'@param metric A character string indicating which column in X to use to
#'  calculate the indicators. Default is "ABUNDANCE".
#'@param group A character string indicating the species group for which to
#'  calculate the indicators. If group = "ALL", all species will be included;
#'  otherwise, group should match a column name in species.table.
#'@param TL.table A dataframe with columns "SPECIES" and the corresponding "TL"
#'  (trophic level). Entries in the "SPECIES" column should be the unique values
#'  of species codes in X (or a subset thereof). Other columns in TL.table are
#'  ignored. If TL.table = NULL, Kempton's Q will not be calculated.
#'@param percentiles The percentiles used to determine R1 and R2 for calculating
#'  Kempton's Q. Default is percentiles = c(0.25, 0.75).
#'@param minTL Minimum trophic level for species included to calculate Kempton's
#'  Q. Default is minTL = 0.
#'@param years A vector of years for which to calculate indicators.
#'@return Returns a dataframe with columns "ID", "YEAR", and indicators
#'  corresponding to the arguments supplied to the function.
#'
#'  If there is no data for an indicator at spatial scale \eqn{j} in year
#'  \eqn{i}, indicator value is assigned NA.
#'@importFrom stats aggregate
#'@family biodiversity indicators
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@export

allBiodiversity <- function(X, metric = "ABUNDANCE", group = "ALL", species.table = NULL, TL.table, 
                            percentiles = c(.25, 0.75), minTL = 3, years){
  
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
                            