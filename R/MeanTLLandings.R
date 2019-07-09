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

MeanTLLandings <- function (land = dat, cutoff = 0, res.table = NA, TL.table = NA) {
	
  if (res.table = NA) {
    load("R/sysdata.rda/indiseas_res.rda")
    res.table <- indiseas_res
    names(res.table)[1] <- 'SPECIES'
    rm(indiseas_res)
  }
  
  if (TL.table = NA) {
    load("R/sysdata.rda/indiseas_TL.rda")
    TL.table <- indiseas_TL
    rm(indiseas_TL)
  }
  
  # DD: not sure exactly what this means, but doesn't sound very generic
  #In Land there duplicate entires for some species which allows for proportions of total landings to be calucaulted  as aggregate(LAND*PROPORTION_OF_LANDINGS~YEAR,data=Land,FUN=sum)
  land.res <- merge(land, res.table)
  land.TL <- merge(land.res, TL)
  
  # might want to re=work these
  # does it need a loop over years?
  if(cutoff == 0) { # not sure what pp is  (or LL)
    land.TL$pp <- land.TL$CATCH*land.TL$LANDED*land.TL$PROPORTION_OF_LANDINGS #this is for the proprtion of different species
		land.TL$LL <- land.TL$pp*land.TL$TL
		}
  
  if(cutoff>0) {
    land.TL$id <- land.TL$TL >= cutoff
		land.TL$pp <- land.TL$CATCH*land.TL$LANDED*land.TL$PROPORTION_OF_LANDINGS*land.TL$id #this is for the proprtion of different species
		land.TL$LL <- land.TL$pp*land.TL$TL*land.TL$id
			}
  
  ind.calc <- merge(aggregate(LL ~ YEAR, data = land.TL, FUN = sum), # this hurts my brain. figure ot what it does 
               aggregate(pp ~ YEAR, data = land.TL, FUN = sum))
	ind <- ind.calc[,3]/ind.calc[,4] # is this the weighted landings/ total landings?
	
	ind

}
