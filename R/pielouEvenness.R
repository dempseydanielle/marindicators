#'@title Calculates Pielou's Species Evenness
#'@description This function calculates Pielou's Species Evenness for \eqn{j}
#'  areas and \eqn{i} years.
#'@details Pielou's Species Evenness: \deqn{J' = -\Sigma p_i ln( p_i )/ln(S)}
#'  \eqn{p_i} is the proportion of the total sample contributed by the i(th)
#'  species and \eqn{S} is the number of species recorded in the sample.
#'  Pielou's Index is the Shannon-Weiner Index computed for the sample \eqn{S}
#'  and represents a measure of evenness of the community (Pielou, 1966).
#'@inheritParams shannon
#'@return Returns a dataframe with 3 columns: \code{ID}, \code{YEAR}, and
#'  \code{PielouEvenness}.
#'
#'  If there is no data for spatial scale \eqn{j} in year \eqn{i}, indicator
#'  values is assigned \code{NA}.
#'@family biodiversity indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Greenstreet SP, Rogers SI. 2006. Indicators of the health of the North Sea
#'  fish community: identifying reference levels for an ecosystem approach to
#'  management. ICES J Mar Sci J du Cons 63:573 593
#'
#'  Pielou EC. 1975. Ecological Diversity. Wiley, New York. ISBN 0-471-68925-4
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@examples
#'data(X)
#'pielouEvenness(X, group = "ALL", metric = "ABUNDANCE", years = c(2014:2019))
#'@export


pielouEvenness <- function(X, group, species.table = NULL, metric = "ABUNDANCE", years)  {

  H <-shannon(X, group = group, species.table = species.table, 
              metric = metric, years = years)           # calculate Shannon's diversity for each year
  S <- speciesRichness(X, group = group, species.table = species.table,
                       metric = metric, years = years)  # calculate species richness for each year

  H$Pielous = H$ShannonDiversity/log(S$SpeciesRichness) # calculate Pielou's species evenness
  
  ind <- H
  ind$ShannonDiversity <- NULL                      # remove Shannon's diversity from ind
  names(ind) = c("ID", "YEAR", "PielouEvenness")    # name the ind dataframe
  ind <- ind[order(ind$ID), ] 
  ind
}
