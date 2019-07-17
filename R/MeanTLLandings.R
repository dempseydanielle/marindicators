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

MeanTLLandings <- function (land, TL.table.land = "scotianshelf", prop.land.table = "scotianshelf",
                            cutoff = 0) {
	
  if (prop.land.table == "scotianshelf"){            # if analyzing Scotian Shelf, import built-in data
    load("R/sysdata.rda/indiseas_proplandings.rda")  # For species that are not identified to the lowest level
    prop.land.table = prop.land                      # in the landings data, estimate the proportion of each 
    names(prop.land.table)[1] <- "SPECIES"           # landed
    rm(prop.land)
  }
  
  if (TL.table.land == "scotianshelf") {               # for Scotian Shelf ecosystem, import stored IndiSeas data
    load("R/sysdata.rda/wss_tl_length.rda")
  }
  
  land.prop <- merge(land, prop.land.table)
  land.TL <- merge(land.prop, TL.table.land)
  
  land.TL$id <- land.TL$TL >= cutoff                                                         # returns TRUE when TL is >= cutoff
  land.TL$pp <- land.TL$CATCH * land.TL$LANDED * land.TL$PROPORTION_OF_LANDINGS * land.TL$id #this is for the proprtion of different species
  land.TL$LL <- land.TL$pp * land.TL$TL * land.TL$id                                       # calculates landings_species.i * TL_species.i

  
  ind <- merge(aggregate(LL ~ ID+YEAR, data = land.TL, FUN = sum), # this hurts my brain. figure ot what it does 
               aggregate(pp ~ ID + YEAR, data = land.TL, FUN = sum))
	ind$ind <- ind$LL/ind$pp # is this the weighted landings/ total landings?
	ind$LL <- NULL
	ind$pp <- NULL
	names(ind) <- c("ID", "YEAR", "MeanTL.Landings")
	ind

}
