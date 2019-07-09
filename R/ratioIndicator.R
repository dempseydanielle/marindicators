#'@title I don't think this is in the Tech Report
#'@description Text
#'@details Text
#'@param X add text here
#'@param group text
#'@param metric add text here
#'@param use.defined add text here
#'@family ecosystem structure and function indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}
#'@export


#ratio between groups
ratioIndicator <- function(X, groups, metric =c('BIOMASS','ABUNDANCE'), start.year, end.year) {
	
  years = c(start_year:end_year)
  ind = data.frame(NULL)
  
  for (i in 1:length(years)){
    
    year.i = years[i]
    
    X.i = X[X$years == year.i, ]
    
    group1 <- resourcePotential(X = X.i, group = groups[1], metric = metric)  
    group2 <- resourcePotential(X = X.i, group = groups[2], metric = metric)  
    ind <- V$BIOMASS.y/(V$BIOMASS.x+V$BIOMASS.y) # I think this should be one over the other, not one over the total
    
  }
	return(V)	
	
  }
  
  
  
  
  