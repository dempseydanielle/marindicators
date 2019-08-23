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
#'@inheritParams resourcePotential
#'@param FP.groups A dataframe with two columns, which must be named
#'  "group.land" and "group.X". Each row holds the group names to calculate the
#'  fishing pressure on a target group, with the numerator in column
#'  "group.land" and the denominator in column "group.X".  Each entry must be a
#'  character string matching the name of a column in species.groups.
#'@param species.table A table with column names that match the entries of
#'  FP.groups. The entries in each column are the species codes for the species
#'  included in that group. Species codes should be a subset of those in the
#'  "SPECIES" column of land (for group.land) or X (for group.X). species.table
#'  may also include columns for other species groups; these will be ignored.
#'  Note that an entry in FP.groups could be "ALL". In this case, a column in
#'  species.table named "ALL" is not required; the function will automatically
#'  include all species in land and/or X.
#'@return Returns a dataframe with three columns: "ID", "YEAR", and a column for
#'  each target group, named "FP_group.X".
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
#'  Abdallahi KM, Perry I, Thiao D, Yemane D, and Cury PM. 2010. Using
#'  indicators for evaluating, comparing and communicating the ecological status
#'  of exploited marine ecosystems. Part 2: Setting the scene. ICES Journal of
#'  Marine Science, 67: 692-716
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@export

fishingPressure <- function(X, land, FP.groups, species.table = NULL, years){
  
  for (k in 1:nrow(FP.groups)){
    
    B.k <- resourcePotential(X = X, metric = "BIOMASS", groups = FP.groups[k, "group.X"], 
                         species.table = species.table, years = years)                                              # calculate the biomass of "group.X" in the community
    Y.k <- landings(land = land, groups = FP.groups[k, "group.land"], species.table = species.table, years = years) # calculate the landings of "group.land"
    
    ind.k <- merge(Y.k, B.k, by = c("ID", "YEAR"), all.x = T)
    
    ind.k$FP <- ind.k[,3]/ind.k[,4]                            # calculate fishing pressure
    index_zero <- which(ind.k[,3] == 0)                    # index of where landings is zero
    ind.k$FP[index_zero] <- 0                              # set fishing pressure to zero wherever there are no landings (even if biomass = NA)
    ind.k[,3] <- NULL                                      # remove landings column
    ind.k[,3] <- NULL                                      # remove biomass column
    
    ind.name <- paste("FP", "_", FP.groups[k, "group.X"], sep ="")         # name indicator: FP_group
    names(ind.k) <- c("ID", "YEAR", ind.name)
    ind.k <- ind.k[order(ind.k$ID), ]                          # order by ID to be consistent with other functions
    
    if(k == 1) ind = ind.k
    ind <- merge(ind, ind.k)
  }
  ind                                                  # return indicator dataframe
  
}
