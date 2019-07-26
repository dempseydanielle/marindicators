#'@title Assigns species to species groups
#'@description This function assigns species into groups
#'  'FINFISH','SKATES','CLUPEIDS','GROUNDFISH',
#'  FLATFISH','GADOIDS','FORAGE','LBENTHIVORE',
#'  MBENTHIVORE','PISCIVORE','PLANKTIVORE', and 'ZOOPISCIVORE'
#'@details Will need text here explaining that specific to Scotian Shelf AND
#'  how to find out which species are in each group
#'@param X dataframe of fishery independent survey data with columns "YEAR",
#'  "ID", "SPECIES", and "BIOMASS" and/or "ABUNDANCE". "ID" is an area code
#'  designating where the observation was recorded. "SPECIES" is a numeric code
#'  indicating the species sampled.
#'@param group character string indicating which group of species to include.
#'  Note that this subsetting is based on the Fisheries and Oceans Canada
#'  species codes for the Scotian Shelf. For other regions it may be prudent to
#'  subsetdata to species groups of interest prior to using the function and
#'  then choose group = "ALL". Type ?speciesgroups for more information.
#'@return text here
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#'@export


speciesgroups <- function(X, group = c('ALL','FINFISH','SKATES','CLUPEIDS','GROUNDFISH',
                                       'FLATFISH','GADOIDS','FORAGE','LBENTHIVORE',
                                       'MBENTHIVORE','PISCIVORE','PLANKTIVORE','ZOOPISCIVORE')){

      if(group=='FINFISH') 	   	Y <- X[X$SPECIES %in% c(1:1000),] 
      if(group=='SKATES')     	Y <- X[X$SPECIES %in% c(200,201,202,203,204,205,206,207,208,209,210,211),]
      if(group=='CLUPEIDS')		  Y <- X[X$SPECIES %in% c(60,61,62,63),]
      if(group=='GROUNDFISH' )  Y <- X[X$SPECIES %in% c(10:22,24:59,140,141,142,143,110,111,112,113,114,115,116,117,118,200,201,202,203,204,205,206,207,208,209,210,
                                                    211,220,221,300,301,304,310,320,340,350,400,620:650),]    	
      if(group=='FLATFISH')     	Y <- X[X$SPECIES %in% c(30,31,40,41,42,43,44,45,49,140,141,142,143),]
      if(group=='GADOIDS')     	  Y <- X[X$SPECIES %in% c(10,11,12,13,14,15,16,17,18,19,110,111,112,113,114,115,116,117,118),]
      if(group=='FORAGE')     	  Y <- X[X$SPECIES %in% c(60,61,62,63,64,610,160),]
      if(group=='LBENTHIVORE')    Y <- X[X$SPECIES %in% c(50,200),]
      if(group=='MBENTHIVORE')    Y <- X[X$SPECIES %in% c(11,241,	40,	41,	42,	43,	123,	142,	143,	202,	203,	301,	304,	414,	501,	505,	640,	114,	115),]
      if(group=='PISCIVORE')    	Y <- X[X$SPECIES %in% c(10,	12,	15,	16,	30,	31,	112,	201,	204,	220,	300,	320,	400),]
      if(group=='PLANKTIVORE')    Y <- X[X$SPECIES %in% c(60,61,62,70,160,610,701,64),]
      if(group=='ZOOPISCIVORE')   Y <- X[X$SPECIES %in% c(13,14,19,23),]
      if(group=='ALL')            Y <- X

  Y
}

