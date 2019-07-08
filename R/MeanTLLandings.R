#'@title Calculates the mean trophic level of fisheries landings
#'@description This function takes a dataframe with columns **** and calculates
#'  the mean trophic level of fisheries landings
#'@details Mean trophic level of fisheries landings \eqn{TL_{Land}}:
#'  \deqn{TL_{Land} = \Sigma (TL_i*Y_i)/Y} where \eqn{TL_i} is the trophic level
#'  of species \eqn{i}, \eqn{Y_i} is the landings of species \eqn{i}, and
#'  \eqn{Y} is the total landings of all species. Trophic Level of individual
#'  species is estimated either through an Ecopath model or dietary analysis, or
#'  taken from a global database such as Fishbase.
#'
#'  This indicator captures the average trophic level of the species exploited
#'  in the fishery. In general, this indicator reflects a gradual transition in
#'  landings from long-lived, high trophic level, piscivorous bottom fish toward
#'  short-lived, low trophic level invertebrates and planktivorous pelagic fish.
#'
#'  Recommended data: Commercial fisheries landings, fish and invertebrates
#'@param land add text here
#'@param cutoff add text here
#'@family fishing pressure indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Pauly D, Christensen V, Dalsgaard J, Froese R, Torres F (1998) Fishing Down
#'  Marine Food Webs. Science 279:860-863
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#'@export

MeanTLLandings <- function (land=dat, cutoff=0) {
	res <- sqlQuery(channel,paste('select * from indiseas_allcodes2res;'))
	names(res)[1]<-'SPECIES'
		#In Land there duplicate entires for some species which allows for proportions of total landings to be calucaulted  as aggregate(LAND*PROPORTION_OF_LANDINGS~YEAR,data=Land,FUN=sum)
		Land <- merge(land,res)
		TL <- sqlQuery(channel,paste('select * from indiseas_WSS_TL;'))
			TLL <- merge(Land,TL)
			if(cutoff==0) {
			TLL$pp <- TLL$CATCH*TLL$LANDED*TLL$PROPORTION_OF_LANDINGS #this is for the proprtion of different species
			TLL$LL <- TLL$pp*TLL$TL
			}
			if(cutoff>0) {
				TLL$id <- TLL$TL>=cutoff
				TLL$pp <- TLL$CATCH*TLL$LANDED*TLL$PROPORTION_OF_LANDINGS*TLL$id #this is for the proprtion of different species
				TLL$LL <- TLL$pp*TLL$TL*TLL$id
			}
		mTL <- merge(aggregate(LL~YEAR+NAMES,data=TLL,FUN=sum),aggregate(pp~YEAR+NAMES,data=TLL,FUN=sum))
		mTL$mTL <- mTL[,3]/mTL[,4]
		return(mTL[,c('YEAR','NAMES','mTL')])

}
