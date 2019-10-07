#'@title Calculates Hill's Index of Diversity (N1)
#'@description This function calculates Hill's Index of Diversity (N1) for
#'  \eqn{j} areas and \eqn{i} years.
#'@details Hill's N1 Diversity Index is the exponential of the Shannon-Weiner
#'  index.\deqn{HillN1 = e^{-\Sigma p_i ln( p_i )}} \eqn{p_i} is the proportion
#'  of the total sample contributed by the i(th) species and \eqn{S} is the
#'  number of species recorded in the sample. This index is sensitive to the
#'  number of species recorded in the sample (Hill, 1973).
#'@inheritParams shannon
#'@return Returns a dataframe with 3 columns: \code{ID}, \code{YEAR}, and
#'  \code{HillDiversity}.
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
#'  management. ICES J Mar Sci J du Cons 63:573-593
#'
#'  Hill MO. 1973. Diversity and evenness: a unifying notation and its
#'  consequences. Ecology 54: 427-431.
#'@author Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca}, Catalina
#'  Gomez, Alida Bundy
#'@examples 
#'data(X)
#'hillN1(X, groups = "ALL", metric = "ABUNDANCE", years = c(2014:2019))
#'@export


hillN1 <- function(X, groups, species.table = NULL, metric = "ABUNDANCE", years) {
  
  for(k in 1:length(groups)){                      # loop over species groups
    
    ind.k <- NULL
    H <- NULL
    
    H <- shannon(X = X, groups = groups[k], species.table = species.table,
                 metric = metric, years = years)     # calculate Shannon's index of diversity
    
    H$hill1 <- exp(H[,3])               # calculate Hill's index of diversity
    
    ind.k <- H
    ind.k[,3] <- NULL                     # remove Shannon's diversity from ind
    
    
    ind.name <- paste("HillDiversity_", groups[k], sep = "")            # name indicator: HillDiversity_group
    names(ind.k) = c("ID", "YEAR", ind.name)                            # name the ind dataframe
    ind.k <- ind.k[order(ind.k$ID), ] 
    
    if(k == 1) ind = ind.k
    
    ind <- merge(ind, ind.k)
    
  }
  
    ind                                              # return Hill's index of diversity
    
  }
  






