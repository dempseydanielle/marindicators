#'@title Calculates all biodiversity indicators
#'@description This function calculates all (or a subset of) biodiversity
#'  indicators for \eqn{j} areas and \eqn{i} years.
#'@details **Say that calls each function separately Recommended data: Fishery
#'  independent survey data or model output; fish and invertebrates.
#'@inheritParams kemptonQ
#'@param TL.table A dataframe with columns "SPECIES" and the corresponding "TL"
#'  (trophic level). Entries in the "SPECIES" column should be the unique values
#'  of species codes in X (or a subset thereof). Other columns in TL.table are
#'  ignored. If TL.table = NULL Kempton's Q will not be calculated.
#'@return Returns a dataframe with columns "ID", "YEAR", "SpeciesRichness",
#'  "ShannonDiversity", "MargalefRichness_group", "PielouEvenness",
#'  "HillDiversity", "HillDominance", "Heips", "KemptonQ"
#'
#'  If there is no data for an indicator at spatial scale \eqn{j} in year
#'  \eqn{i}, indicator value is assigned NA.
#'@importFrom stats aggregate
#'@family biodiversity indicators
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@export

allBiodiversity <- function(X, metric = "ABUNDANCE", group = "ALL", species.table, TL.table, 
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
                            