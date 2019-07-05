#' @title Calculates the biomass or abundance per discrete trophic level
#' @description This function takes a dataframe with columns **** and calculates
#'   the biomass or abundance per discrete trophic level.
#' @details **Recommended data: Fishery independent surveys, fish and
#'   invertebrates.
#' @param X add text here
#' @param table.of.trophic.level.data add text here
#' @param metric add text here
#' @param TL.grouping add text here
#' @param path add text here
#' @family stability and resistance indicators
#' @references Coll M, Shannon LJ, Moloney CL, Palomera I, Tudela S,
#'   2006. Comparing trophic flows and fishing impacts of a NW Mediterranean
#'   ecosystem with coastal upwellings by means of standardized ecological
#'   models and indicators. Ecol. Model. 198, 53-70. (not in references!)
#'
#'   Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the selection and
#'   evaluation of ecological indicators. Can. Tech. Rep. Fish. Aquat. Sci.
#'   3232: xii + 212 p.
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#' @export


biomassPerTL <- function(X,table.of.trophic.level.data='INDISEAS_WSS_TL',metric=c('BIOMASS','ABUNDANCE'), TL.grouping=1,path=NA) {
		#X is input data
		#not length based
		#breaks can be divided into three levels of grouping
		uI <- unique(X$ID) 	
		if(is.na(path)) TLS 	<- sqlQuery(channel,paste("select research species, avg(TL) TL from ",table.of.trophic.level.data," group by research;",sep=""))
		if(!is.na(path)) TLS 	<- read.csv(file.path(path,"extra info",paste(table.of.trophic.level.data,".csv",sep="")))
		breaks <- seq(1,10,by=TL.grouping)
		TLS['TL'] <- breaks[findInterval(TLS[,'TL'],breaks)]
		
		X <- merge(X,TLS,by='SPECIES')		
		mmL <-list()
		
		for(i in 1:length(uI)) {
			Y <- X[X$ID==uI[i],]
			mmL[[i]] <- aggregate(Y[metric],by=c(Y[c('ID','TL')]),FUN=sum)
		   }
		   out <- as.data.frame(do.call(rbind,mmL))
		   return(out)		
	}