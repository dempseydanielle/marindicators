#'@title Calculates Hill's index of diversity (Hill, 1973)
#'@description This function takes a dataframe of fisheries independent survey
#'  data and calculates Hill's index of diversity for \eqn{j} areas and \eqn{i}
#'  years.
#'@details Hill's N1 diversity index is the exponential of the Shannon-Weiner
#'  index.\deqn{HillN1 = e^{-\Sigma p_i ln( p_i )}} \eqn{p_i} is the proportion
#'  of the total sample contributed by the i(th) species and \eqn{S} is the
#'  number of species recorded in the sample. This index is sensitive to the
#'  number of species recorded in the sample.
#'
#'  Recommended data: Fishery independent surveys or model output; fish and
#'  invertebrates
#'@inheritParams shannon
#'@return Returns a dataframe with 3 columns: "ID", YEAR", and "HillDiversity".
#'
#'  If there is no data for spatial scale \eqn{j} in year \eqn{i}, indicator
#'  values is assigned NA.
#'@family biodiversity indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Greenstreet SP, Rogers SI. 2006. Indicators of the health of the North Sea
#'  fish community: identifying reference levels for an ecosystem approach to
#'  management. ICES J Mar Sci J du Cons 63:573-593
#'
#'  Hill MO. 1973. Diversity and evenness: a unifying notation and its
#'  consequences. Ecology 54: 427-431.
#'@author Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@export


hillN1 <- function(X, group, species.table = NULL, metric = "ABUNDANCE", years) {
  
  H <- shannon(X = X, group = group, species.table = species.table,
               metric = metric, years = years)     # calculate Shannon's index of diversity
	
  H$hill1 <- exp(H$ShannonDiversity)               # calculate Hill's index of diversity
  
  ind <- H
	ind$ShannonDiversity <- NULL                     # remove Shannon's diversity from ind
	
	names(ind) = c("ID", "YEAR", "HillDiversity")    # name the ind dataframe
	ind <- ind[order(ind$ID), ]
	ind                                              # return Hill's index of diversity
	
	}







