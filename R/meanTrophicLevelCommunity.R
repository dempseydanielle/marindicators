#'@title Calculates the mean trophic level of the community
#'@description This function takes a dataframe with columns **** and calculates
#'  the mean trophic level of the community, weighted by biomass
#'@details Mean trophic level (TL): \deqn{TL = \Sigma TL_i*B_i)/\Sigma B_i}
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


meanTrophicLevelCommunity <- function(X,pred.data.table='indiseas_wss_tl',metric='BIOMASS') {
	ol <- sqlQuery(channel,paste("select research species,avg(TL) from ",pred.data.table," group by research;",sep=""))
	X <- merge(X,ol,by='SPECIES')
	uI <- unique(X$ID)
	out <- numeric()
	for(i in 1:length(uI)) {
	                Y <- X[X$ID==uI[i],]
	                out[i] <- aggregate(Y[metric]*Y['AVG(TL)'],by=Y['ID'],FUN=sum)[,2]/aggregate(Y[metric],by=Y['ID'],FUN=sum)[,2]
		}
		
		out <- as.data.frame(cbind(uI,out))
		out[,2] <- as.numeric(out[,2])
		names(out) <- c('ID','MTLCom')
	return(out)
		}
