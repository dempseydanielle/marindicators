#'@title Calculates inverse fishing pressure
#'@description This function calculates the inverse fishing pressure on a fished
#'  group for \eqn{j} areas and \eqn{i} years.(***not in Table S4)
#'@details Fishing pressure (InverseFP): \deqn{FP = B_{FG}/Y_{FG}} where
#'  \eqn{B_{FG}} is the biomass of the fished group(s) and \eqn{Y_{FG}} is the
#'  landed catch of the group(s).
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
#'  These columns have value of "1" in the rows of species included in the group
#'  and "NA" in all other rows.
#'@param group string indicating the species group for which to calculate the
#'  landings. Should match one of the column names of land. If group = "ALL" the
#'  fishing pressure on the whole community will be calculated.
#'@param years vector of years for which to calculate indicator
#'@return returns a dataframe with three columns: "ID", "YEAR", and
#'  "invFP_group"
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
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#'@export

InverseFishingPressure <- function(X, land, group, years) {
	
  FP <- FishingPressure(X = X, land = land, group = group, years = years)    # calculate fishing pressure
  FP$invFP <- 1/FP[,3]                                        # calculate inverse fishing pressure
  
  index <- which(FP$invFP == Inf)                                # index NAs
  FP[index, 'invFP'] <- 0                                            # replace NA with 0
  
  ind.name <- paste("invFP", "_", group, sep = "")            # name indicator: invFP_group
  names(FP) <- c("ID", "YEAR", "FP", ind.name)                # name FP columns 
  ind <- FP                                                   # set ind equal to FP
  ind$FP <- NULL                                             # remove FP column
  ind                                            
	
}
