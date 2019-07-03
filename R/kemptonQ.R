#' @title Calculates Kempton's Biodiversity Index
#' @description This function takes a dataframe with columns **** and calculates
#'   Kempton's Biodiversity Index (Q)
#' @details Kempton's Biodiversity Index (Q): \deqn{Q = S/2*log(R_2/R_1)} S is
#'   the total number of species or functional groups, R1 and R2 are the lower
#'   and upper quartiles of the species abundance distribution. Q is a relative
#'   index of biomass diversity calculated from the Kempton’s Q75 index
#'   developed for expressing species diversity. This index includes those
#'   species or functional groups with a trophic level (TL) of three or higher.
#'   R1 and R2 are defaulted to .25 and .75 **Recommended data: Fishery
#'   independent surveys, fish and invertebrates
#' @param X add text here
#' @param percentiles R1 and R2 are defaulted to .25 and .75 add text here
#' @param metric add text here
#' @family biodiversity indicators
#' @references Ainsworth et al. 2006 (not in tech report)
#'
#'   Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the selection and
#'   evaluation of ecological indicators. Can. Tech. Rep. Fish. Aquat. Sci.
#'   3232: xii + 212 p.
#'
#'   Kempton R, Taylor L (1976) Models and statistics for species diversity.
#'   Nature 262:818-820
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export



kemptonQ<- function(X, percentiles=c(.25,0.75),minTL=3,metric=c('BIOMASS','ABUNDANCE')) {
		#percentiles is the percentiles for r2 and r1
		#based on Kempton and Taylor 1976 Nature 262
		#minTL sets a minimum trophic level for inclusion in group for all species set minTL=0
		#not length based TL, just any species with TL >3 at any size August 29, 2013 09:19:50 AM 
			if(minTL>0) {
			sp <- sqlQuery(channel,paste("select distinct RESEARCH species from indiseas_wss_tl where tl>",minTL," order by research;",sep=""))[,1]
			X <- X[X$SPECIES %in% sp,]
			}
			
			uI <- unique(X$ID)
			Q.est <- numeric()
			 for(i in 1:length(uI)) {
			 	Y <- X[X$ID==uI[i],]
			 	Y <- Y[order(Y[metric]),metric]   
			 	U <- length(Y)
			 	if(U>2) {
			 	w <- c(round(U*percentiles[1],0),round(U*percentiles[2],0))
			 	if(w[1]==0) w[1]<-1
				Q.est[i] <- U*(percentiles[2]-percentiles[1])/log(Y[w[2]]/Y[w[1]])
				}  else {
				Q.est[i]<-NA
				}
			}
			out <- as.data.frame(cbind(uI,Q.est))
			out[,2] <- as.numeric(out[,2])
			names(out)[1] <-'ID'
			return(out)
	}
#example
#a<-biomassData(q.corr=T)
#a$ID <- paste(a$MISSION,a$SETNO)	
#ew <-kemptonQ(a,metric='ABUNDANCE')

