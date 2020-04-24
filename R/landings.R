#'@title Calculates the Landings per fished group
#'@description This function calculates the commercial landings of predefined
#'  species groups for \eqn{j} areas and \eqn{i} years.
#'@details Calculates the Landings of predefined species groups.
#'@inheritParams resourcePotential
#'@param land A dataframe of commercial landings data with columns \code{YEAR},
#'  \code{ID}, \code{SPECIES} and \code{CATCH}. \code{YEAR} indicates the year
#'  the landing was recorded, \code{ID} is an area code indicating where the
#'  landing was recorded, \code{SPECIES} is a numeric code indicating the
#'  species landed, and \code{CATCH} is the corresponding landed weight.
#'@param species.table A table with at least one column, where the column names
#'  match the entries in \code{groups}, and the column entries are species codes
#'  from \code{land} indicating which species are included that group.
#'  \code{species.table} may also include columns for other species groups;
#'  these will be ignored. If \code{group = "ALL"}, this table is not required.
#'  Default is \code{species.table = NULL}.
#'@return Returns a dataframe with columns \code{ID} and \code{YEAR}, and a
#'  column \code{landings_group} for each entry in \code{groups}.
#'
#'  If there is no data for a given year, the indicator value is set to 0.
#'@family fishing pressure indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@examples
#'# Compile data
#'data(land)
#'data(species.table)
#'
#'# Species of interest
#'landings.groups <- c("ALL", "CLUPEIDS.L", "FLATFISH.L", "GROUNDFISH.L")
#'
#'# Calculate indicators
#'landings(land, groups = landings.groups, species.table = species.table,
#'    years = c(2014:2019))
#'@export

landings <- function(land, groups, species.table = NULL, years) {
  
  uI <- unique(land$ID)
  DF <- createDataframe(uI = uI, years = years)  # create a dataframe that matches each area ID to each year

  for(k in 1:length(groups)){                    # loop over species groups
    
    land.k <- speciesGroups(X = land, group = groups[k], species.table = species.table) # subset land to the species group of interest
    
    ind.k <- aggregate(CATCH ~ ID + YEAR, data = land.k, FUN = sum)   # sum over years and spatial scales 
    ind.k <- merge(DF, ind.k, by = c("ID", "YEAR"), all.x = T)        # merge ind with df. This makes the indicator value "NA" for any year without data
    
    index <- which(is.na(ind.k[,3]))                                # index NAs
    ind.k[index, 3] <- 0                                            # replace NA with 0
    
    ind.name <- paste("landings_", groups[k], sep ="")        # name indicator: group_landings
    names(ind.k) <- c("ID", "YEAR", ind.name)
    ind.k <- ind.k[order(ind.k$ID), ]                             # order by "ID" to be consistent with other functions
    
    if(k == 1) ind = ind.k
    ind <- merge(ind, ind.k, all.x = TRUE)
  }
  
  ind
}
