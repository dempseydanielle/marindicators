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
#'  Recommended data: \eqn{B_{FG}}: fishery independent survey data,
#'  \eqn{Y_{FG}}: commercial fisheries landings
#'@inheritParams landings
#'@param X A dataframe of fishery independent survey data with columns "YEAR",
#'  "ID", "SPECIES", and "BIOMASS". "YEAR" indicates the year the observation
#'  was recorded, "ID" is an area code indicating where the observation was
#'  recorded, "SPECIES" is a numeric code indicating the species sampled, and
#'  "BIOMASS" is the corresponding biomass (stratified and corrected for
#'  catchability as required).
#'@param group.X A character string indicating which species to include in the
#'  denominator. Must match the name of a column in species.table.
#'@param group.land A character string indicating which species to include in
#'  the numerator. Must match the name of a column in species.table.
#'@param species.table A table with at least two columns, named after the
#'  strings in group.X and group.land. The entries in column group.X are all the
#'  species in the fished group. The entries in column group.land are the
#'  species of interest landed from this group. species.table may also include
#'  columns for other species groups; these will be ignored. If group.X and
#'  group.land both equal "ALL", fishing pressure on the whole community is
#'  calculated, and species.table can be NULL.
#'@return Returns a dataframe with three columns: "ID", "YEAR", and "FP_group".
#'
#'  If biomass of group is NA and landings of group are zero, fishing pressure
#'  is set to zero. Otherwise, if biomass of group is NA, fishing pressure is
#'  set to NA.
#'
#'@family fishing pressure indicators
#'@references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Shin, YJ, Shannon LJ, Bundy A, Coll M, Aydin K, Bez N, Blanchard JL, Borges,
#'  MF, Diallo I, Diaz E, Heymans JJ, Hill L, Johannesen E, Jouffre D, Kifani S,
#'  Labrosse P, Link JS, Mackinson S, Masski H, Möllmann C, Neira S, Ojaveer H,
#'  Ould Mohammed Abdallahi ., Perry I, Thiao D, Yemane D, and Cury PM. 2010.
#'  Using indicators for evaluating, comparing and communicating the ecological
#'  status of exploited marine ecosystems. Part 2: Setting the scene. ICES
#'  Journal of Marine Science, 67: 692-716
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@export

fishingPressure <- function(X, land, group.X, group.land, species.table = NULL, years){
                            
  B <- resourcePotential(X, metric = "BIOMASS", group = group.X, 
                         species.table = species.table, years = years)  # calculate the biomass of "group" in the community
  Y <- landings(land, group = group.land, species.table = species.table, years = years)                         # calculate the landings of "group"
  
  ind <- merge(Y, B, by = c("ID", "YEAR"), all.x = T)

  ind$FP <- ind[,3]/ind[,4]                            # calculate fishing pressure
  index_zero <- which(ind[,3] == 0)                    # index of where landings is zero
  ind$FP[index_zero] <- 0                              # set fishing pressure to zero wherever there are no landings (even if biomass = NA)
  ind[,3] <- NULL                                      # remove landings column
  ind[,3] <- NULL                                      # remove biomass column
  
  ind.name <- paste("FP", "_", group.X, sep ="")         # name indicator: FP_group
  names(ind) <- c("ID", "YEAR", ind.name)
  ind <- ind[order(ind$ID), ]                          # order by ID to be consistent with other functions
  ind                                                  # return indicator dataframe
  
}
