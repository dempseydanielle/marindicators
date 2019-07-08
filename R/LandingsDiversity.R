#'@title Calculates the diversity of the target species
#'@description This function takes a dataframe with columns **** and calculates
#'  the diversity of the target species (\eqn{TS_y})
#'@details The diversity of the target species for year y (\eqn{TS_y}) is the
#'  count of the number of target species recorded in all trawl catches
#'  collected in that year.
#'
#'  Recommended data: commercial fisheries landings, fish and invertebrates
#'@param land add text here
#'@param grps add text here
#'@family fishing pressure indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Degnbol P (2005) Indicators as a means of communicating knowledge. In: ICES
#'  Journal of Marine Science. p 606–611
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}
#'@export


LandingsDiversity <- function(land = dat, grps = groups) {
	
  years = c(start.year:end.year)
  ind = data.frame(NULL)
  
  for (i in 1:length(years)){
    
    year.i = years[i]
    land.i = land[land$year == year.i,]
    n.species.i = length(unique(land.i$SPECIES_NAMES))
    
    ind[i] = n.species.i
    
  }
  
  
  # re-wrote this with a loop over each year
  
  a <- unique(land[,c('YEAR','NAMES')]) # I think this extracts the unique combinations of year and speices
	a$NSP <- NA # what is NSP?
	
	for (i in 1:nrow(a)) {
		h <- land[land$YEAR==a[i,1] & land$NAMES==a[i,2],]
		a[i,'NSP'] <- length(unique(h$SPECIES))
	}
	
	return(a)
}  

}
