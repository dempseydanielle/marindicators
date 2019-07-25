#'@title Calculates fishing pressure
#'@description This function calculates fishing pressure on a fished group for
#'  \eqn{j} areas and \eqn{i} years.
#'@details Fishing pressure (FP): \deqn{FP = Y_{FG}/B_{FG}} where \eqn{B_{FG}}
#'  is the biomass of the fished group(s) and \eqn{Y_{FG}} is the landed catch
#'  of the group(s).
#'
#'  This indicator measures the level of exploitation or total fishing pressure
#'  at the ecosystem level. Change in this indicator can result from change in
#'  \eqn{B_{FG}}, \eqn{Y_{FG}} or both. If \eqn{B_{FG}} and \eqn{Y_{FG}} change
#'  in the same direction, exploitation rate may not change.
#'
#'  Recommended data: \eqn{B_{FG}}: fishery independent surveys, \eqn{Y_{FG}}:
#'  commercial fisheries landings
#'@param X dataframe of fishery independent survey data with columns "YEAR",
#'  "ID", "SPECIES", and "BIOMASS". "ID" is an area code designating where the
#'  observation was recorded (a string). "SPECIES" is a numeric code indicating
#'  the species sampled.
#'@param land dataframe of commercial landings data with columns "YEAR", "ID",
#'  "ALLCODES" and "CATCH". "ID" is an area code designating where the
#'  observation was recorded. "ALLCODES" is a numeric commercial species code
#'  indicating the species landed, and "CATCH" is the corresponding landed
#'  weight. Additional columns are required for each species group of interest.
#'  These columns have value of "1" in the rows species included in the group
#'  and "NA" in all other rows.
#'@param group string indicating the species group for which to calculate the
#'  landings. Should match one of the column names of land. If group = "ALL" the
#'  fishing pressure on the whole community will be calculated.
#'@param years vector of years for which to calculate indicator
#'@return returns a dataframe with three columns: "ID", "YEAR", and "FP_group"
#'@family fishing pressure indicators
#'@references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Shin YJ, Bundy A, Shannon LJ, Simier M, Coll M, Fulton EA, Link JS, Jouffre
#'  D, Ojaveer H, MacKinson S, Heymans JJ, Raid T (2010) Can simple be useful
#'  and reliable? Using ecological indicators to represent and compare the
#'  states of marine ecosystems. ICES J Mar Sci 67:717–731
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca} Cataline Gomez
#'@export

FishingPressure <- function(X, land, group, years){
                            
  B <- resourcePotential(X, metric = "BIOMASS", group = group, years = years)  # calculate the biomass of "group" in the community
  Y <- LandByGroup(land, group = group, years = years)                         # calculate the landings of "group"
  
  ind <- merge(Y, B, by = c('YEAR','ID'), all.x = T)

  ind$FP <- ind[,3]/ind[,4]                                         # calculate fishing pressure
  ind[,3] <- NULL
  ind[,3] <- NULL
  
  ind <- ind[order(ind$ID), ]
  ind.name <- paste("FP", "_", group, sep ="")                  # name indicator: FP_group
  names(ind) <- c("ID", "YEAR", ind.name)
  ind                                                           # return indicator dataframe
  
}
