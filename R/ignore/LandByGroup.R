LandByGroup <- function(land=dat,group=c('FINFISH','CLUPEIDS','GROUNDFISH','FLATFISH','GADOIDS','FORAGE',
					'INVERTEBRATES','ALL','LARGE_PELAGIC')) {
		u <- sqlQuery(channel,paste("select * from gomezc.indiseas_allcodes;"))
		if(group !='ALL') {
		  	u <- na.omit(u[,c('ALLCODES','ALLNAMES',group)])
			land <- land[land$SPECIES %in% u$ALLCODES,]
			}
		o <- aggregate(CATCH~YEAR+NAMES,data=land,FUN=sum)
		return(o)
}
	