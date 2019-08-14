#'@title Calculates the resource potential of species groups
#'@description This function calculates the biomass or abundance of a
#'  pre-defined group of species for \eqn{j} areas and \eqn{i} years.
#'@details This indicator reflects temporal dynamics of species groups.
#'
#'  Recommended data: Fishery independent survey data or model output; fish and
#'  invertebrates.
#'@inheritParams biomassPerTL
#'@param group A character string indicating which species to include in the
#'  indicator calculation. If group = "ALL", all species will be included;
#'  otherwise, group should match a column name in species.table.
#'@param species.table A table with default species.table = NULL. If group =
#'  "ALL", this table is not required. If group does not equal "ALL",
#'  species.table is a table with at least one column, where the column name is
#'  the same as group, and the column  entries are the species codes indicating
#'  the species to be included in the calculation. species.table may also
#'  include columns for different species groups; these will be ignored.
#'@return Returns a dataframe with 3 columns. "ID", "YEAR", and "metric_group".
#'
#'  If there is no data for a given year, the indicator value will be "NA" for
#'  that year. If biomass of species X was not captured in the survey, species X
#'  is still likely to be present, just not detected or perhaps not recorded.
#'  Replacing with zero would have an impact on trends, whereas treating as NA
#'  does not.
#'@family resource potential indicators
#'@family ecosystem structure and functioning indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@export


resourcePotential <- function(X, group, species.table = NULL, metric = "BIOMASS", years){
  
  uI <- unique(X$ID)
  DF <- createDataframe(uI = uI, years = years)  # create a dataframe that matches each area ID to each year
  
  X <- speciesGroups(X = X, group = group, species.table = species.table) # subset X to the species of interest
  
  ind <- aggregate(X[metric], by = c(X["ID"], X["YEAR"]), FUN = sum)    # add up metric for the species group for each year + spatial scale
  ind <- merge(DF, ind, by = c("ID", "YEAR"), all.x = T)

  ind.name <- paste(metric, "_", group, sep ="")                       # name indicator: metric_group
  names(ind) = c("ID", "YEAR", ind.name)                             
  ind = ind[order(ind$ID), ]                # order by ID (to match output of other functions)
  ind                                       # return indicator values for unique(X$YEAR) 
  
  }	
			
