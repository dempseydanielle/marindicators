#'@title Calculates the proportion of predatory fish in the community
#'@description This function takes a dataframe with columns **** and calculates
#'  the proportion of predatory fish in the community (PropPred)
#'@details Proportion of predatory fish in the community is estimated by:
#'  \deqn{PropPred = Biomass Predatory Fish Surveyed/Total Biomass Surveyed} Predatory fish
#'  species are defined as all surveyed fish species that are not largely
#'  planktivorous (i.e. phytoplankton and zooplankton feeders should be
#'  excluded; Shin et al. 2010). A fish species is classified as predatory if it
#'  is piscivorous, or if it feeds on invertebrates that are larger than the
#'  macrozooplankton category (.2 cm). Detritivores should not be classified as
#'  predatory fish.
#'
#'  This indicator captures changes in the trophic structure and changes in the
#'  functional diversity of fish in the ecosystem. Data used: Fishery
#'  independent surveys, predatory fish.
#'
#'  Recommended data: Fishery independent surveys, fish and invertebrates.
#'@param X add text here
#'@param pred.data.table to delete?
#'@param metric set to biomass
#'@family ecosystem structure and function indicators
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


predatoryFish <- function(X,pred.data.table='indiseas_pred_retained_wss',metric='BIOMASS') {
		#ol <- sqlQuery(channel,paste("select species from ",pred.data.table," where predatory=1;",sep=""))
		#ss <- ol$SPECIES
		ss <- c(10,11,12,13,14,15,16,19,30,31,40,41,42,43,44,50,51,52,111,112,114,119,122,
			142,143,149,150,156,200,201,202,203,204,220,221,241,300,301,303,304,307,320,400,410,
			411,412,414,500,501,604,619,620,621,623,626,640,641,642,647,712,741,880,2526,4511,141,
			49,25,59,32,33,747,72,190,192,71,172,256,73,230,238,246,231,233,4514,27)
	
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
			names(out) <- c('ID','pF')
		return(out)
	}
		