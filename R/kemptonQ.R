#' @title Calculates Kempton's Biodiversity Index
#' @description This function takes a dataframe with columns **** and calculates
#'   Kempton's Biodiversity Index (Q)
#' @details Kempton's Biodiversity Index (Q): \deqn{Q = S/2*log(R_2/R_1)}
#'   \eqn{S} is the total number of species or functional groups, \eqn{R_1} and
#'   \eqn{R_2} are the lower and upper quartiles of the species abundance
#'   distribution. \eqn{Q} is a relative index of biomass diversity calculated
#'   from the Kempton's Q75 index developed for expressing species diversity.
#'   This index includes those species or functional groups with a trophic level
#'   of three or higher. \eqn{R_1} and \eqn{R_2} are defaulted to 0.25 and 0.75,
#'   respectively.
#'
#'   **Something seems fishy about how this equation is implemented or written.
#'   I think it might just be that the equation assumes that using 0.25 and 0.75
#'   (0.75 - 0.25 = 0.5 = 1/2)
#'  
#'
#'   **Recommended data: Fishery independent surveys, fish and invertebrates
#' @param X add text here
#' @param percentiles R1 and R2 are defaulted to .25 and .75 add text here
#' @param metric add text here
#' @param minTL sets a minimum trophic level for inclusion. Default is minTl =
#'   3. For all species, set minTL = 0
#' @family biodiversity indicators
#' @references Ainsworth, C, Pitcher, T (2006) Modifying Kempton's species
#'   diversity index for use with ecosystem simulation models. Ecological
#'   Indicators. 6. 623-630. 10.1016/j.ecolind.2005.08.024.  (not in tech
#'   report. Could be this one or the onw below)
#'
#'   Ainsworth, C, Varkey, D, Pitcher, TJ  (2006)  Preliminary ecosystem
#'   simulation models for the Bird’s Head Seascape, Papua.  Mid-term narrative
#'   technical report.  Birds Head Seascape Ecosystem-Based Management Project.
#'   University of British Columbia Fisheries Centre.  December, 2006, 274 pp.
#'
#'   Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the selection and
#'   evaluation of ecological indicators. Can. Tech. Rep. Fish. Aquat. Sci.
#'   3232: xii + 212 p.
#'
#'   Kempton R, Taylor L (1976) Models and statistics for species diversity.
#'   Nature 262:818-820
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#' @export



kemptonQ<- function(X, percentiles = c(.25, 0.75), minTL = 3, metric = c('BIOMASS','ABUNDANCE')) {
		#percentiles is the percentiles for r2 and r1
		#based on Kempton and Taylor 1976 Nature 262
		#minTL sets a minimum trophic level for inclusion in group for all species set minTL=0
		#not length based TL, just any species with TL >3 at any size August 29, 2013 09:19:50 AM 
	# find a way to subset the data based on trophic level
  	if(minTL>0) {
			sp <- sqlQuery(channel,paste("select distinct RESEARCH species from indiseas_wss_tl where tl>",minTL," order by research;",sep=""))[,1]
			X <- X[X$SPECIES %in% sp,]
			}
  
  # might be better to replace U with S and use the speciesrichness function
  Y <- Y[order(X[metric]),metric]  # why ordered???  --> oh this might have somethign to do with the percentiles
  U <- length(Y)                   # why does this matter?
  # should just be able to use the "speciesrichness" function
  if(U > 2) {                      # why does this matter?
    w <- c(round(U*percentiles[1], 0), round(U*percentiles[2], 0))
    if(w[1]==0) w[1]<-1 # why this??? I think so not diving by zero in next line
    # how do the w[] get into Y?? is it an index??
    Q.est[i] <- U*(percentiles[2]-percentiles[1])/log(Y[w[2]]/Y[w[1]]) # something seems weird about this equation
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

