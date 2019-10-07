#' @title Calculates Heip's Evenness Index
#' @description This function calculates Heip's Evenness Index (HE) of the
#'   community for \eqn{j} areas and \eqn{i} years.
#' @details Heip's Evenness Index (HE): \deqn{HE = (exp^{H'} - 1)/(S - 1)} where
#'   \eqn{H'} is Shannon's index of diversity and \eqn{S} is species richness.
#'   This index ranges from 0 to 1 and measures how equally the species richness
#'   contributes to the total abundance or biomass of the community (Heip,
#'   1974).
#' @inheritParams  shannon
#' @return Returns a dataframe with 3 columns: \code{ID}, \code{YEAR}, and
#'   \code{Heips}.
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
#'   Catalina Gomez, Alida Bundy
#' @examples
#' data(X)
#' heips(X, groups = "ALL", species.table = NULL, metric = "ABUNDANCE", years = c(2014:2019))
#' @export

heips <- function(X, groups, species.table = NULL, metric = "ABUNDANCE", years)  {
  
  for(k in 1:length(groups)){          # loop over species groups

    ind.k <- NULL
    H <- NULL
    S <- NULL
    
    H <- shannon(X = X, groups = groups[k], species.table = species.table,
                 metric = metric, years = years)            # calculate Shannon's diversity for each year
    S <- speciesRichness(X = X, groups = groups[k], species.table = species.table,
                         metric = metric, years = years)   # calculate species richness for each year
    
    H$heips = (exp(H[,3]) - 1)/(S[,3] - 1)
    ind.k <- H
    
    ind.k[,3] <- NULL                                           # remove Shannon's diversity from ind
    ind.name <- paste("Heips_", groups[k], sep = "")            # name indicator: Heips_group
    names(ind.k) = c("ID", "YEAR", ind.name)                    # name the ind dataframe
    ind.k <- ind.k[order(ind.k$ID), ]
    
    if(k == 1) ind = ind.k
    
    ind <- merge(ind, ind.k)
    
  }
  ind      
}
