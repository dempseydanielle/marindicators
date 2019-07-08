#'@title Calculates the landings per fished group
#'@description This function takes a dataframe with columns **** and calculates
#'  the catch from commercial fisheries that is landed ashore.
#'@details **This text could change depending on structure of the data. Includes
#'  nine fished groups: Finfish, Skates, Clupeids, Flatfish, Gadoid, Groundfish,
#'  Invertebrates, Forage fish and Large pelagic fish.
#'
#'  Recommended data: commercial fisheries landings
#'@param land add text here
#'@param group add text here
#'@family fishing pressure indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Bundy A, Heymans JJ, Morissette L, Savenkoff C (2009) Seals, cod and forage
#'  fish: A comparative exploration of variations in the theme of stock collapse
#'  and ecosystem change in four Northwest Atlantic ecosystems. Prog Oceanogr
#'  81:188–206
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}
#'@export


# this will depend on what "groups" the user has
LandByGroup <- function(land = dat, groups) {
  
  u <- sqlQuery(channel,paste("select * from gomezc.indiseas_allcodes;")) # not sure what this is
  
  if(group !='ALL') {
    u <- na.omit(u[,c('ALLCODES', 'ALLNAMES', groups)])
			land <- land[land$SPECIES %in% u$ALLCODES,]
			}
		o <- aggregate(CATCH~YEAR+NAMES,data=land,FUN=sum)
		return(o)
}
	