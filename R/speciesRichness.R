# DD Changed the NAME of this function from speciesRichness to speciesrichness
#' 
#' @title Calculates species richness (S_y)
#' @description This function takes a dataframe with columns **** and calculates
#'   species richness.
#' @details S is the count of the number of species recorded in all trawl
#'   catches collected in any one year (y). 
#'   
#'   Recommended data: Fishery independent
#'   surveys, fish and invertebrates
#'
#' @param X is probably a dataframe with certain columns.
#' @param group is where you select which groups to include
#' @param grps not sure what this is because it is NOT called inside the
#'   function
#' @family biodiversity indicators
#' @references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   Greenstreet SP, Fraser HM, Rogers SI, Trenkel VM, Simpson SD, Pinnegar JK
#'   (2012) Redundancy in metrics describing the composition, structure, and
#'   functioning of the North Sea demersal fish community. ICES J Mar Sci
#'   69:8–22
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export

speciesrichness <- function(X, group=c('FINFISH','ALL'), grps)  {

	if(group=='FINFISH') {
		X <- X[X$SPECIES<1000,]
		}
	
	uI <- unique(X$ID)
	sr.est <- numeric()
	
	for(i in 1:length(uI)) {
	 	Y <- X[X$ID==uI[i],]
	 	sr.est[i] <- length(unique(Y$SPECIES))
		}
	out <- as.data.frame(cbind(uI,sr.est))
	out[,2] <- as.numeric(out[,2])
	return(out)
	}
