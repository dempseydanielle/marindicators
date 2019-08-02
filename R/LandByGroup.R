#'@title Calculates the landings per fished group
#'@description This function calculates the the catch of predefined speceies
#'  groups from commercial fisheries that is landed ashore for \eqn{j} areas and
#'  \eqn{i} years.
#'@details Calculates the total landings of predefined species groups.
#'
#'  Recommended data: commercial fisheries landings
#'@param land dataframe of commercial landings data with columns "YEAR", "ID",
#'  "ALLCODES" and "CATCH". "ID" is an area code designating where the
#'  observation was recorded. "ALLCODES" is a numeric commercial species code
#'  indicating the species landed, and "CATCH" is the corresponding landed
#'  weight. Additional columns are required for each species group of interest.
#'  These columns have value of "1" in the rows of species included in the group
#'  and "NA" in all other rows.
#'@param group string indicating the species group for which to calculate the
#'  landings. Should match one of the column names of land. If group = "ALL" the
#'  total landings of all species will be calculated.
#'@param years vector of years for which to calculate indicator
#'@return returns a dataframe with three columns: "ID", "YEAR", and
#'  "group_Landings".
#'
#'  If there is no data for a given year, the indicator value is set to 0.
#'
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
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#'@export

LandByGroup <- function(land, 
                        group=c('FINFISH','CLUPEIDS','GROUNDFISH','FLATFISH','GADOIDS','FORAGE',
                                'INVERTEBRATES','ALL','LARGE_PELAGIC'), years) {
  
  # make a dataframe with "ID" and "YEAR" for all spatial scales and years
  df <- NULL
  uI <- unique(land$ID)
  for(j in 1:length(uI)){
    ID.j <- rep(uI[j], times = length(years))
    df.j <- data.frame(ID.j, years)
    df <- rbind(df, df.j)
  }
  names(df) <- c("ID", "YEAR")
  
  if(group !='ALL') land <- land[which(land[group] == 1),]      # subset to species in "GROUP"
  
  ind <- aggregate(CATCH ~ ID + YEAR, data = land, FUN = sum)   # sum over years and spatial scales 
  ind <- merge(df, ind, by = c("ID", "YEAR"), all.x = T)        # merge ind with df. This makes the indicator value "NA" for any year without data

  index <- which(is.na(ind[,3]))                                # index NAs
  ind[index, 3] <- 0                                            # replace NA with 0
  
  ind.name <- paste(group, "_", "landings", sep ="")            # name indicator: metric_group
  names(ind) <- c("ID", "YEAR", ind.name)
  ind <- ind[order(ind$ID), ]                                   # order by "ID" to be consistent with other functions
  ind
  
}
