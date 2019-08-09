#'@title Calculates the landings per fished group
#'@description This function calculates the the catch of predefined speceies
#'  groups from commercial fisheries that is landed ashore for \eqn{j} areas and
#'  \eqn{i} years.
#'@details Calculates the total landings of predefined species groups.
#'
#'  Recommended data: commercial fisheries landings
#'@inheritParams resourcePotential
#'@param land A dataframe of commercial landings data with columns "YEAR", "ID",
#'  "SPECIES" and "CATCH". "YEAR" indicates the year the landing
#'  was recorded, "ID" is an area code indicating where the landing was
#'  recorded, "SPECIES" is a numeric code indicating the species landed, and
#'  "CATCH" is the corresponding landed weight.
#'@return returns a dataframe with three columns: "ID", "YEAR", and
#'  "group_Landings".
#'
#'  If there is no data for a given year, the indicator value is set to 0.
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

landings <- function(land, group, species.table = NULL, years) {
  
  # make a dataframe with "ID" and "YEAR" for all spatial scales and years
  df <- NULL
  uI <- unique(land$ID)
  for(j in 1:length(uI)){
    ID.j <- rep(uI[j], times = length(years))
    df.j <- data.frame(ID.j, years)
    df <- rbind(df, df.j)
  }
  names(df) <- c("ID", "YEAR")
  
 # if(group !='ALL') land <- land[which(land[group] == 1),]      # subset to species in "GROUP"
  land <- speciesgroups(X = land, group = group, species.table = species.table) # subset land to the species group of interest
  
  
  ind <- aggregate(CATCH ~ ID + YEAR, data = land, FUN = sum)   # sum over years and spatial scales 
  ind <- merge(df, ind, by = c("ID", "YEAR"), all.x = T)        # merge ind with df. This makes the indicator value "NA" for any year without data

  index <- which(is.na(ind[,3]))                                # index NAs
  ind[index, 3] <- 0                                            # replace NA with 0
  
  ind.name <- paste(group, "_", "landings", sep ="")            # name indicator: metric_group
  names(ind) <- c("ID", "YEAR", ind.name)
  ind <- ind[order(ind$ID), ]                                   # order by "ID" to be consistent with other functions
  ind
  
}
