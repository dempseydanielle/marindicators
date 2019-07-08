#' @title Calculates Fulton's Condition Index for the community
#' @description This function takes a dataframe with columns **** and calculates
#'   Fulton's Condition Index (\deqn{K}) for the community (weighted by
#'   abundance)
#' @details Fulton's Condition Index (\eqn{K}): \deqn{K = \Sigma(K_j *
#'   A_j)/\Sigma A_j} where the sum is over all species, \eqn{j}, \eqn{A_j} is
#'   the abundance of species \eqn{j}, and \deqn{K_j = 100*W_j/L_j^3} where
#'   \eqn{W_j} is the mean weight at length \eqn{L} for species \eqn{j}.
#'
#'   **Recommended data: Fishery independent surveys, fish.
#' @param X add text here
#' @param metric add text here
#' @param gp add text here
#' @param yr add text here
#' @param user.defined add text here
#' @param group add text here
#' @param path add text here
#' @family ecosystem structure and function indicators
#' @references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   DFO (2003) State of the Eastern Scotian Shelf ecosystem. Dartmouth, Nova
#'   Scotia
#'
#'   Choi JS, Frank KT, Petrie BD, Leggett WC (2005) Integrated Assessment of a
#'   Large Marine Ecosystem: a case study of the devolution of the Eastern
#'   Scotian Shelf, Canada. Oceanogr Mar Biol An Annu Rev 43:47–67
#'
#'   Rochet M, Rice JC (2005) Do explicit criteria help in selecting indicators
#'   for ecosystem-based fisheries management? ICES J Mar Sci 62:528–539
#'
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#' @export

# Do I want to make X someting more descriptive? RV_dat? dat?
communityFultonK <- function(X, metric='ABUNDANCE',
                             gp=groups,yr=yrs[i], user.defined=F, 
                             group=c('FINFISH','SKATES','CLUPEIDS','GROUNDFISH','FLATFISH','GADOIDS','FORAGE',
					'LBENTHIVORE','MBENTHIVORE','PISCIVORE','PLANKTIVORE','ZOOPISCIVORE')) {
		#X is input data
		#Finfish data only
		#using length stratited estimates calculate the mean weight of fish at every sampled cm
	
	# see original script for species groupings and where the length and weight come from 
  # Note: Y is X subsetted for the species group(s) of interest (i.e., no inverts)
	out<-data.frame(ID=unique(Y$ID),mK=NA)
	W <- defineGroups(dat = wt,gp=gp)                     # AC function specific to DFO(?) data, i.e., species codes
	W <- aggregate(FWT ~ FLEN+SPECIES+ID,data=W,FUN=mean) # fish weights by length and species
	if(any(unique(Y$SPECIES) %in% unique(W$SPECIES)))  {  # if there are the same species in Y (biomass) and W (weight). . . 
	Z <- merge(Y,W,by=c('ID','SPECIES','FLEN'),all.y=T)   # merge them
	Z <- merge(Z,aggregate(ABUNDANCE~ID,data=Z,FUN=sum),by='ID') # now also include the total abundance of all species

	K <- FWT / FLEN^3*100
	
	ind <- aggregate(K*ABUNDANCE.x/ABUNDANCE.y ~ ID, data = Z, FUN = sum) # maybe change ~ID to ~year? OR loop over each year
	# also: give better names to the two abundances
	# double check how these all line up with each other 
	names(ind) = c(...)

	
	}	
	ind
}
