#'@title not sure what this one does
#'@description This function takes a dataframe with columns **** and calculates
#'  the mean trophic level of the community, weighted by biomass
#'@details Mean trophic level (TL): \deqn{TL = \Sigma(TL_i*B_i)/\Sigma B_i}
#'  \eqn{TL_i} is trophic level of species i, and \eqn{B_i} is the biomass of
#'  species i.
#'
#'  This indicator is based on trophic levels (TL) of all species with available
#'  biomass time series, weighted by annual species-specific biomass, to reflect
#'  the structure of the community.
#'
#'  Recommended data: Fishery independent surveys, fish and invertebrates.
#'@param X add text here
#'@param lpred.data.table to delete?
#'@param metric set to biomass
#'@family ecosystem structure and function indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Christensen 1998 (not in tech report)
#'
#'  Shannon et al 2014
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}
#'@export


meanTrophicLevelCommunityLength <- function(X, TL.table = NA, metric = 'BIOMASS') {
	
  if (TL.table = NA) {
    load("R/sysdata.rda/indiseas_TL.rda")
    TL.table <- indiseas_TL
    rm(indiseas_TL)
  }

	
	# not sure what mTLc is. . . .can't get this line to run
	# I think I might have to have X in here
	mTLc <- sqldf(paste("select ID,flen,species,",metric,",GROUP_NAME,TL from X s, ol t where s.species=t.research and s.flen between t.MINL and t.MAXL;",sep=""))
	 
	# I still think there is a better way to do this.
	# Probably need to get rid of 'ID' and loop over year
	ind <- aggregate(X[metric]*X['TL'], by=X['ID'], FUN=sum)[,2]/aggregate(X[metric], by=X['ID'], FUN=sum)[,2]

	ind
		}
