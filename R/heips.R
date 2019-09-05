#' @title Calculates Heip's evenness index 
#' @description This function calculates Heip's evenness index (HE) of the
#'   community for \eqn{j} areas and \eqn{i} years.
#' @details Heip's Evenness Index (HE): \deqn{HE = (exp^{H'} - 1)/(S - 1)} where
#'   \eqn{H'} is Shannon's index of diversity and \eqn{S} is species richness.
#'   This index ranges from 0 to 1 and measures how equally the species richness
#'   contributes to the total abundance or biomass of the community (Heip, 1974).
#' @inheritParams  shannon
#' @return Returns a dataframe with 3 columns: \code{ID}, \code{YEAR}, and \code{Heips}.
#'
#'   If there is no data for spatial scale \eqn{j} in year \eqn{i}, indicator
#'   value is assigned NA.
#' @family biodiversity indicators
#' @references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   Heip C. 1974. A new index measuring evenness. J. Mar. Biol. Asso. UK 54:
#'   555-557.
#'
#'   Kenchington T, Kenchington E (2013) Biodiversity Metrics for Use in the
#'   Ecosystem Approach to Oceans Management. Can. Tech. Rep. Fish. Aquat. Sci.
#'   3059: vi+188p.
#' @author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#' @export

heips <- function(X, group, species.table = NULL, metric = "ABUNDANCE", years)  {
  
  H <-shannon(X = X, group = group, species.table = species.table,
              metric = metric, years = years)            # calculate Shannon's diversity for each year
  S <- speciesRichness(X = X, group = group, species.table = species.table,
                       metric = metric, years = years)   # calculate species richness for each year
  
  H$heips = (exp(H$ShannonDiversity)-1)/(S$SpeciesRichness-1)
  ind <- H
  
  ind$ShannonDiversity <- NULL                     # remove Shannon's diversity from ind
  names(ind) = c("ID", "YEAR", "Heips")            # name the ind dataframe
  ind <- ind[order(ind$ID), ]
  ind      
}
