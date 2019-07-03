#' @title Also calculates Margalef's species richness (see Margalef.R)
#' @description This function takes a dataframe with columns **** and calculates
#'   Margalef's species richness
#' @details Margalef's species richness: \deqn{S = (S_y - 1)/log(F_y)} S is the
#'   count of the number of species recorded in all trawl catches collected in
#'   any one year (y). F is the total count of all individuals caught in all
#'   trawl catches in any one year (y). 
#'   
#'   **Recommended data: Fishery independent surveys, fish and invertebrates.
#'
#' @param X is probably a dataframe with certain columns.
#' @param group is where you select which groups to include
#' @param metric is where you choose if you want to calculate using biomass or
#'   abundance
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

margalefSpeciesRichness <- function(X,group=c('FINFISH','ALL'),metric=c('BIOMASS','ABUNDANCE'))  {

	if(group=='FINFISH') {
		X <- X[X$SPECIES<1000,]
		}
	if(group=='GROUNDFISH' )  X <- X[X$SPECIES %in% c(10:22,24:59,140,141,142,143,110,111,112,113,114,115,116,117,118,200,201,202,203,204,205,206,207,208,209,210,211,220,221,300,301,304,310,320,340,350,400,620:650),]    	

	uI <- unique(X$ID)
	marsr.est <- numeric()
	 for(i in 1:length(uI)) {
	 	Y <- X[X$ID==uI[i],]
	 	Y <- Y[order(Y[metric]),metric]   
	 	marsr.est[i] <- (length(Y)-1)/log(sum(Y))
	 	if(marsr.est[i]>20 || marsr.est[i]<0) marsr.est[i]<-NA
	 	}
	out <- as.data.frame(cbind(uI,marsr.est))
	names(out)[1] <-'ID'
	out[,2] <- as.numeric(out[,2])
	return(out)
	}
