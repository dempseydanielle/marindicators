#'@title Calculates the mean maximum length of fish in the community
#'@description This function takes a dataframe with columns **** and calculates
#'  the mean maximum length (MML) of fish in the community weighted by biomass
#'  or abundance
#'@details Mean Maximum Length (MML): \deqn{MML = \Sigma (L_{max,i}*M_i)/\Sigma
#'  M_i} where \eqn{L_{max,i}} is the maximum asymptotic length (cm) of species
#'  \eqn{i}, and \eqn{M_i} is biomass or abundance of species \eqn{i} (excluding
#'  invertebrates).
#'
#'  Recommended data: Fishery independent surveys, fish.
#'@param X add text here
#'@param table.of.length.data add text here --or delete
#'@param metric add text here
#'@family stability and resistance indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Houle JE, Farnsworth KD, Rossberg AG, Reid DG (2012) Assessing the
#'  sensitivity and specificity of fish community indicators to management
#'  action. Can J Fish Aquat Sci 69:1065-1079
#'
#'  Shin YJ, Rochet MJ, Jennings S, Field JG, Gislason H (2005) Using size-based
#'  indicators to evaluate the ecosystem effects of fishing. In: ICES Journal of
#'  Marine Science. p 384-396
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}
#'@export

meanMaxL <- function(X,table.of.length.data='INDISEAS_MAX_LENGTH',metric=c('BIOMASS','ABUNDANCE'),path=NA) {
		#this indicator is for finfish only
		#X is input data
		
		if(is.na(path)) 	len <- sqlQuery(channel,paste("select * from ",table.of.length.data,";",sep=""))
		if(!is.na(path)) 	len <- read.csv(file.path(path,"extra info",paste(table.of.length.data,".csv",sep="")))
		X <- merge(X,len,by='SPECIES')
		uI <- unique(X$ID) 	
		mmL <-numeric()
		for(i in 1:length(uI)) {
			Y <- X[X$ID==uI[i],]
			mmL[i] <- sum(Y[metric]*Y['MAXLEN99'])/sum(Y[metric])	
		   }
		   out <- as.data.frame(cbind(uI,mmL))
		   names(out)[1] <-'ID'
		   out[,2] <- as.numeric(out[,2])
		   return(out)		
	}