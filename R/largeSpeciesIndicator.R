#'@title Calculates the Large Species Indicator
#'@description This function takes a dataframe with columns **** and calculates
#'  the Large Species Indicator (LSI)
#'@details Large Species Indicator (LSI): \deqn{LSI = \Sigma B_i(L_max >85
#'  cm)/\Sigma B_i} where \eqn{B_i} is biomass of individual species, i, and
#'  \eqn{L_max} is the maximum asymptotic length (cm; here the default is 85
#'  cm).
#'
#'  Recommended data: Fishery independent surveys, fish.
#'@param X add text here
#'@param lmax (set to 85)
#'@param metric add text here
#'@param linf.data.table to delete
#'@family ecosystem structure and function indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Shephard S, Fung T, Houle JE, Farnsworth KD, Reid DG, Rossberg AG (2012)
#'  Size-selective fishing drives species composition in the Celtic Sea. ICES J
#'  Mar Sci 69:223-234 
#'
#'  Houle JE, Farnsworth KD, Rossberg AG, Reid DG (2012) Assessing the
#'  sensitivity and specificity of fish community indicators to management
#'  action. Can J Fish Aquat Sci 69:1065-1079
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}
#'@export



largeSpeciesIndicator <- function(X,lmax=85,linf.data.table='indiseas_max_length',metric='BIOMASS') {
	ol <- sqlQuery(channel,paste("select * from ",linf.data.table,";",sep=""))
	ss <- ol$SPECIES[ol$MAXLEN99>lmax]
	uI <- unique(X$ID)
	out <- list()
	for(i in 1:length(uI)) {
	                Y <- X[X$ID==uI[i],]
	                A <- sum(Y[Y$SPECIES %in% ss, metric])
	                B <- sum(Y[, metric])
	            out[[i]] <- cbind(uI[i],A/B)
		}
		out <- as.data.frame(do.call(rbind,out))
		out[,2] <- as.numeric(out[,2])
		names(out) <- c('ID','LSI')
	return(out)
		}
