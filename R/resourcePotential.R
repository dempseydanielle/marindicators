#'@title Calculates the resource potential of pre-defined species groups
#'@description This function calculates the biomass or abundance of a
#'  pre-defined group of species for \eqn{j} areas and \eqn{i} years.
#'@details This indicator reflects temporal dynamics species groups specific to
#'  the Scotian Shelf.
#'
#'  Recommended data: Fishery independent surveys, fish and invertebrates.
#'@param X dataframe of fishery independent survey data with columns "YEAR",
#'  "ID", "SPECIES", and "BIOMASS" and/or "ABUNDANCE". "ID" is an area code
#'  designating where the observation was recorded. "SPECIES" is a numeric code
#'  indicating the species sampled.
#'@param user.defined for a user-defined group of species, put a vector of
#'  species codes to include here. Default is "FALSE"
#'@param group character string indicating which group of species to include.
#'  Note that this subsetting is based on the Fisheries and Oceans Canada
#'  species codes for the Scotian Shelf. For other regions it may be prudent to
#'  subsetdata to species groups of interest prior to using the function and
#'  then choose group = "ALL". Type ?speciesgroups for more information.
#'@param metric character string indicating whether to use "BIOMASS" or
#'  "ABUNDANCE" to calculate indicator.
#'@param years vector of years for which to calculate indicator
#'@return Returns a dataframe with 3 columns. "ID", "YEAR", and "metric_group".
#'
#'  If there is no data for a given year, the indicator value will be "NA". If
#'  biomass of species “x” was not captured in the survey, species “x” is still
#'  likely to present, just not detected or perhaps not recorded. Replacing with
#'  zero would have an impact on trends, whereas treatin as NA does not.
#'@family resource potential indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.


#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#'@export


resourcePotential <- function(X, user.defined = F, 
                              group = c('FINFISH','ALL','SKATES',
                                      'CLUPEIDS','GROUNDFISH','FLATFISH','GADOIDS',
                                      'INVERTEBRATES','FORAGE','PELAGIC','LBENTHIVORE',
                                      'MBENTHIVORE','PISCIVORE', 'PLANKTIVORE','ZOOPISCIVORE'),
                              metric = c('BIOMASS','ABUNDANCE'), years){
  
  df <- NULL
  uI <- unique(X$ID)
  for(j in 1:length(uI)){
    ID.j <- rep(uI[j], times = length(years))
    df.j <- data.frame(ID.j, years)
    df <- rbind(df, df.j)
  }
  names(df) <- c("ID", "YEAR")
  
			Y<-FALSE
			#IF USER DEFINED FOR GROUP PUT IN A VECTOR OF THE SPECIES CODES YOU WANT TO INCLUDE
			if(user.defined) {
			Y <- X[X$SPECIES %in% group,]	
			
			} else {
				if(group=='ALL') 			Y <- X
				if(group=='FINFISH') 	Y <- X[X$SPECIES %in% c(1:1000),] 
				if(group=='SKATES')   Y <- X[X$SPECIES %in% c(200,201,202,203,204,205,206,207,208,209,210,211),]
				if(group=='CLUPEIDS')	Y <- X[X$SPECIES %in% c(60,61,62,63),]
				if(group=='GROUNDFISH')  Y <- X[X$SPECIES %in% c(10:22,24:59,140,141,142,143,110,111,112,113,114,
				                                                 115,116,117,118,200,201,202,203,204,205,206,207,
				                                                 208,209,210,211,220,221,300,301,304,310,320,340,
				                                                 350,400,620:650),]    	
				if(group=='FLATFISH')     	Y <- X[X$SPECIES %in% c(30,31,40,41,42,43,44,45,49,140,141,142,143),]
				if(group=='GADOIDS')       	Y <- X[X$SPECIES %in% c(10,11,12,13,14,15,16,17,18,19,110,111,112,113,114,115,116,117,118),]
				if(group=='INVERTEBRATES')  Y <- X[X$SPECIES %in% c(2000:8999),]
				if(group=='FORAGE')       	Y <- X[X$SPECIES %in% c(60,61,62,63,64,610,160),]
				if(group=='PELAGIC')     	  Y <- X[X$SPECIES %in% c(60,61,62,63,64,610,160,70),]
				if(group=='LBENTHIVORE')    Y <- X[X$SPECIES %in% c(50,200),]
				if(group=='MBENTHIVORE')    Y <- X[X$SPECIES %in% c(11,241,	40,	41,	42,	43,	123,	142,	143,	202,	203,	301,	304,	414,	501,	505,	640,	114,	115),]
				if(group=='PISCIVORE')    	Y <- X[X$SPECIES %in% c(10,	12,	15,	16,	30,	31,	112,	201,	204,	220,	300,	320,	400),]
				if(group=='PLANKTIVORE')    Y <- X[X$SPECIES %in% c(60,61,62,70,160,610,701,64),]
				if(group=='ZOOPISCIVORE')   Y <- X[X$SPECIES %in% c(13,14,19,23),]

			}
			
			if(nrow(Y)==0 || Y==FALSE ) dat <- data.frame(ID = unique(X$ID), Y=0)
			else {
			  
			  ind <- aggregate(Y[metric], by= c(Y['ID'], Y['YEAR']), FUN = sum)    # add up metric for the species group for each year + spatial scale
			  ind <- merge(df, ind, by = c("ID", "YEAR"), all.x = T)
			  ind.name <- paste(metric, "_", group, sep ="")                       # name indicator: metric_group
			  names(ind) = c("ID", "YEAR", ind.name)                             
			  ind = ind[order(ind$ID), ]                                           # order by ID (to match output of other functions)
			}
	
			ind                                                                    # return indicator values for unique(X$YEAR) 
		}	
			
