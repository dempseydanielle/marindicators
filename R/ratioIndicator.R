#'@title not entirely sure that this one does
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
ratioIndicator <- function(X,group,metric,user.defined) {
	Y <- resourcePotential(X=X,group=group[1],metric=metric,user.defined=user.defined)  
	Z <- resourcePotential(X=X,group=group[2],metric=metric,,user.defined=user.defined)  
	V <- merge(Y,Z,all.x=T,by='ID')
	V$RATIO <- V$BIOMASS.y/(V$BIOMASS.x+V$BIOMASS.y)
	return(V)	
	}