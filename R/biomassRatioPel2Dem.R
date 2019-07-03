#' @title Calculates the pelagic to demersal biomass ratio
#' @description This function takes a dataframe with columns **** and calculates
#'   the pelagic to demersal biomass ratio
#' @details **Recommended data: Fishery independent surveys, fish and
#'   invertebrates.
#' @param X add text here
#' @family ecosystem structure and function indicators
#' @references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   Bundy A, Heymans JJ, Morissette L, Savenkoff C (2009) Seals, cod and forage
#'   fish: A comparative exploration of variations in the theme of stock
#'   collapse and ecosystem change in four Northwest Atlantic ecosystems. Prog
#'   Oceanogr 81:188–206
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export

biomassRatioPel2Dem <- function(X) {
		
	Y <- resourcePotential(X=X,group='GROUNDFISH',metric='BIOMASS')  
	Z <- resourcePotential(X=X,group='PELAGIC',metric='BIOMASS')  
	V <- merge(Y,Z,all.x=T,by='ID')
	V$RATIO <- V$BIOMASS.y/V$BIOMASS.x
	V <- V[,c('ID','RATIO')]
	return(V)
	}	
	