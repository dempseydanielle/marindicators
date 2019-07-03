#' @title Calculates Fulton's Condition Index for the community
#' @description This function takes a dataframe with columns **** and calculates
#'   Fulton's Condition Index (K) for the community (weighted by abundance)
#' @details Fulton's Condition Index (K): \deqn{K = \Sigma(K_j * A_j)/\Sigma(A_j)}
#'   where \eqn{A_j} is the abundance of species j, and \deqn{K_j = W_j/L_j^3 x
#'   100} where \eqn{W_j} is the mean weight at length (L) for species j
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
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export


communityFultonK <- function(X,metric='ABUNDANCE',gp=groups,yr=yrs[i],user.defined=F, group=c('FINFISH','SKATES','CLUPEIDS','GROUNDFISH','FLATFISH','GADOIDS','FORAGE',
					'LBENTHIVORE','MBENTHIVORE','PISCIVORE','PLANKTIVORE','ZOOPISCIVORE'),path=NA) {
		#X is input data
		#Finfish data only
		#using length stratited estimates calculate the mean weight of fish at every sampled cm
		
		if(user.defined) {
			Y <- X[X$SPECIES %in% group,]	
			} else {
				if(group=='FINFISH') 		Y <- X[X$SPECIES %in% c(1:1000),] 
				if(group=='SKATES')     	Y <- X[X$SPECIES %in% c(200,201,202,203,204,205,206,207,208,209,210,211),]
				if(group=='CLUPEIDS')		Y <- X[X$SPECIES %in% c(60,61,62,63),]
				if(group=='GROUNDFISH' )  Y <- X[X$SPECIES %in% c(10:22,24:59,140,141,142,143,110,111,112,113,114,115,116,117,118,200,201,202,203,204,205,206,207,208,209,210,
													211,220,221,300,301,304,310,320,340,350,400,620:650),]    	
				if(group=='FLATFISH')     	Y <- X[X$SPECIES %in% c(30,31,40,41,42,43,44,45,49,140,141,142,143),]
				if(group=='GADOIDS')     	Y <- X[X$SPECIES %in% c(10,11,12,13,14,15,16,17,18,19,110,111,112,113,114,115,116,117,118),]
				if(group=='FORAGE')     	Y <- X[X$SPECIES %in% c(60,61,62,63,64,610,160),]
				if(group=='LBENTHIVORE')    Y <- X[X$SPECIES %in% c(50,200),]
				if(group=='MBENTHIVORE')    Y <- X[X$SPECIES %in% c(11,241,	40,	41,	42,	43,	123,	142,	143,	202,	203,	301,	304,	414,	501,	505,	640,	114,	115),]
				if(group=='PISCIVORE')    	Y <- X[X$SPECIES %in% c(10,	12,	15,	16,	30,	31,	112,	201,	204,	220,	300,	320,	400),]
				if(group=='PLANKTIVORE')    Y <- X[X$SPECIES %in% c(60,61,62,70,160,610,701,64),]
				if(group=='ZOOPISCIVORE')    Y <- X[X$SPECIES %in% c(13,14,19,23),]
                if(group=='ALL')                                                Y <- X
				}
	if(is.na(path)) 	wt <- sqlQuery(channel,paste("select distinct strat,spec species,flen,fwt from groundfish.gsinf i, groundfish.gsdet d where i.mission=d.mission and i.setno=d.setno and to_char(sdate,'yyyy') = ",yr," and to_char(sdate,'mm') in ('06','07','08') and strat between '440' and '495' and fwt is not null and flen is not null;",sep=""))
	if(!is.na(path))	load(file.path(path,"data","lenwgt",paste("/lw",yr,".Rdata",sep="")))
	out<-data.frame(ID=unique(Y$ID),mK=NA)
	W <- defineGroups(dat=wt,gp=gp)
	W <- aggregate(FWT~FLEN+SPECIES+ID,data=W,FUN=mean)
	if(any(unique(Y$SPECIES) %in% unique(W$SPECIES))) {
	Z <- merge(Y,W,by=c('ID','SPECIES','FLEN'),all.y=T)
	Z <- merge(Z,aggregate(ABUNDANCE~ID,data=Z,FUN=sum),by='ID')
	Z$K <- Z$FWT / Z$FLEN^3*100
	
	out <- aggregate(K*ABUNDANCE.x/ABUNDANCE.y~ID,data=Z,FUN=sum)
	names(out) <- c('ID','mK')
	
	}	
	return(out)

}
