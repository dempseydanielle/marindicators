#' @title Also calculates Margalef's species richness (see
#'   MargalefSpeciesRichness.R)
#' @description This function takes a dataframe with columns **** and calculates
#'   Margalef's species richness
#' @details Margalef's species richness: \deqn{S = (S_y - 1)/log(F_y)} S is the
#'   count of the number of species recorded in all trawl catches collected in
#'   any one year (y). F is the total count of all individuals caught in all
#'   trawl catches in any one year (y). 
#'   
#'   **Recommended data: Fishery independent surveys, fish and invertebrates.
#' @param X is probably a dataframe with certain columns.
#' @param group is where you select which groups to include
#' @param metric is where you choose if you want to calculate using biomass or
#' @family biodiversity indicators
#' @references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   Greenstreet SP, Fraser HM, Rogers SI, Trenkel VM, Simpson SD, Pinnegar JK
#'   (2012) Redundancy in metrics describing the composition, structure, and
#'   functioning of the North Sea demersal fish community. ICES J Mar Sci
#'   69:8–22
#'
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export

margalef<- function(X, group=c('FINFISH', 'ALL'), metric=c('BIOMASS', 'ABUNDANCE')) {
#needed to limit the range of margalef to emliminate soem of our sets where biomass or abundances are 
#extremely low ie~0 and there are several species as values go crazy November 26, 2012 01:02:56 PM AMC
	if(group == 'FINFISH') {
		X <- X[as.numeric(X$SPECIES)<1000,]
		}
		X <- X[X[metric]>0,]
	uI <- unique(X$ID)
	MDI.est <- numeric()
	 for(i in 1:length(uI)) {
	 	Y <- X[X$ID==uI[i],]	 	
	 	Y <- Y[order(Y[metric]),metric]   
	 	U <- length(Y)
	 	
	 	oo <- (U-1)/log(sum(Y)) 
	 	if(oo>0 && oo < 50) {
	 		MDI.est[i] <- oo
	 		}	else {
	 		MDI.est[i] <- NA
	 		}
	 	}
	out <- as.data.frame(cbind(uI,MDI.est))
	names(out)[1] <-'ID'
	out[,2] <- as.numeric(out[,2])
	return(out)
}
