#'@title Calculates the mean maximum age of fish in the community
#'@description This function takes a dataframe with columns **** and calculates
#'  the mean maximum age (MMA) of fish in the community
#'@details Mean Maximum Lifespan (MMA): \deqn{MMA = \Sigma
#'  (age_max,i*B_i)/\Sigma B_i} where \eqn{B_i} is biomass of species i. The
#'  mean lifespan or longevity is considered to be a fixed parameter per
#'  species. Lifespan may vary under fishing pressure, so IndiSeas adopted the
#'  maximum longevity observed for each species. The variation of this indicator
#'  captures changes in species composition. **Check that i is subscript in PDF
#'
#'  Recommended data: Fishery independent surveys, fish.
#'@param X add text here
#'@param table.of.age.data add text here --or delete
#'@param metric add text here
#'@family stability and resistance indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Shin YJ, Bundy A, Shannon LJ, Simier M, Coll M, Fulton EA, Link JS, Jouffre
#'  D, Ojaveer H, MacKinson S, Heymans JJ, Raid T (2010) Can simple be useful
#'  and reliable? Using ecological indicators to represent and compare the
#'  states of marine ecosystems. ICES J Mar Sci 67:717-731
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}
#'@export


	meanMaxAge <- function(X,table.of.age.data='INDISEAS_MAX_AGE',metric=c('BIOMASS','ABUNDANCE')) {
		#this indicator is for finfish and squid only
		#X is input data
		
		uI <- unique(X$ID) 	
		len <- sqlQuery(channel,paste("select SPECIES, MAXAGE from ",table.of.age.data,";",sep=""))
		X <- merge(X,len,by='SPECIES')
			mmL <-numeric()
			for(i in 1:length(uI)) {
				Y <- X[X$ID==uI[i],]
				if(nrow(Y)>1) {
				mmL[i] <- sum(Y[metric]*Y['MAXAGE'])/sum(Y[metric])	
				}
				else {
				mmL[i]<-NA
				}
			   }
		   out <- as.data.frame(cbind(uI,mmL))
		   names(out)[1] <-'ID'
		   out[,2] <- as.numeric(out[,2])
		   return(out)		
	}