#'@title Calculates all Pressure indicators
#'@description This function calculates all (or a subset) of the Pressure
#'  indicators for \eqn{j} areas and \eqn{i} years. The user can choose whether
#'  the function returns the raw indicator values, the standaradized (z-score)
#'  values, or both.
#'@details This function calculates the Pressure indicators: Diversity of the
#'  Target Species, Total Landings, Landings of Target Groups, Fishing Pressure
#'  on the Community, Fishing Pressure on Target Groups, Mean Trophic Level of
#'  the Landings, and the Marine Trophic Index. If data are not available to
#'  calculate one or more of these indicators, a subset will be returned. See
#'  the help file for the individual functions for information on how each
#'  indicator is calculated.
#'@inheritParams fishingPressure
#'@inheritParams allStructure
#'@param landings.groups A vector indicating the species groups for which to
#'  calculate the landings. Each entry must be a character string matching the
#'  name of a column in \code{species.table}. If \code{landings.groups = NULL},
#'  no Landings indicators will be calculated.
#'@param FP.groups A dataframe with two columns, which must be named
#'  \code{group.land} and \code{group.X}. Each row holds the group names to
#'  calculate the fishing pressure on a target group, with the numerator in
#'  column \code{group.land} and the denominator in column \code{group.X}. Each
#'  entry must be a character string matching the name of a column in
#'  \code{species.table}. If \code{FP.groups = NULL}, no fishing pressure
#'  indicators will be calculated.
#'@param speciesinfo.table A table with columns \code{SPECIES} and the
#'  corresponding \code{TL_LAND} (trophic level of landed species). Entries in
#'  the \code{SPECIES} column should be the unique values of species codes in
#'  land (or a subset thereof). Other columns will be ignored.
#'@param species.table A table where the column names match the entries in
#'  \code{landings.groups}. Column entries are the species codes indicating the
#'  species from \code{land} included in each group. \code{species.table} may
#'  also include columns for other species groups; these will be ignored.
#'@param minTL A vector containing minimum trophic level to include when
#'  calculating the mean trophic level of the landings. Default is \code{minTL =
#'  c(0, 3.25)}, which will return the mean trophic level of the landings and
#'  the marine trophic index.
#'@param years A vector of years for which to calculate indicators.
#'@return Returns a dataframe with columns \code{ID}, \code{YEAR}, and
#'  indicators corresponding to the arguments supplied to the function.
#'  Standardized indicators are noted with "_s" in the name.
#'@family fishing pressure indicators
#'@references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@examples
#'data(X)
#'data(land)
#'data(species.groups)
#'data(species.info)
#'
#'landings.groups <- c("ALL", "CLUPEIDS.L", "FLATFISH.L", "GROUNDFISH.L")
#' FP.groups <- data.frame(rbind(c("ALL", "ALL"),
#'    c("CLUPEIDS", "CLUPEIDS.L"),
#'    c("FLATFISH", "FLATFISH.L"),
#'    c("GROUNDFISH", "GROUNDFISH.L")))
#'names(FP.groups) <- c("group.X", "group.land")
#'
#'allPressure(X = X, land = land, species.table = species.groups,
#'    speciesinfo.table = species.info, landings.groups = landings.groups,
#'    FP.groups = FP.groups, minTL = c(0, 3.25), years = c(2014:2019), raw = FALSE, std = TRUE)
#'@export

allPressure <- function(X, land, 
                        species.table, speciesinfo.table, landings.groups, FP.groups,
                        minTL = c(0, 3.25),  years, raw = TRUE, std = TRUE){
  
  if(raw == FALSE & std == FALSE) stop("error: both raw and std are FALSE")
  
  if("CATCH" %in% colnames(land)){
    
    inds <- createDataframe(unique(land$ID), years)
    
    # Diversity of target species
    SR.L = speciesRichness(land, metric = "CATCH", group = "ALL", years = years)
    
    inds <- merge(inds, SR.L, all.x = TRUE)
    
    if(length(minTL) > 0){
      
      for(i in 1:length(minTL)){
        # Mean Trophic Level Landings 
        MTL.i = meanTLLandings(land, TL.table = speciesinfo.table, 
                               minTL = minTL[i], years = years)
        if(i == 1) MTL = MTL.i
        MTL <- merge(MTL, MTL.i, all.x = TRUE)
      }
      inds <- merge(inds, MTL, all.x = TRUE)
    }
    
    # landings by group
    if(length(landings.groups) > 0){
      land.ind <- landings(land = land, groups = landings.groups,
                           species.table = species.table, years = years)
      inds <- merge(inds, land.ind, all.x = TRUE)
    }
    
    # fishing pressure by group
    if("BIOMASS" %in% colnames(X) & nrow(FP.groups) > 0){
      
      FP = fishingPressure(X = X, land = land, FP.groups = FP.groups,
                           species.table = species.table,  years = years)
      
      inds <- merge(inds, FP, all.x = TRUE)
    }
    
    if(std == TRUE){
      inds_std <-  standardize(inds)
      
      if(raw == FALSE) inds <- inds_std
      if(raw == TRUE) inds <- merge(inds, inds_std)
    }
  }
  
  if(exists("inds")) inds 
  else print("warning: argument land = NULL. No Pressure indicators were calculated")
  
}