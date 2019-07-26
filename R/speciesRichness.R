#' @title Calculates species richness of the community or the diversity of
#'   target species
#' @description This function counts the number of species recorded in fishery
#'   independent survey data or commercial landings data for \eqn{i} years and
#'   \eqn{j} areas.
#' @details Species richness (\eqn{S_y}) is the count of the number of species
#'   recorded in all research vessel trawl surveys collected year \eqn{y}.
#'
#'   Recommended data: Fishery independent surveys, fish and invertebrates
#'
#'   The diversity of the target species for year y (\eqn{TS_y}) is the count of
#'   the number of target species recorded in all trawl catches collected in
#'   that year.
#'
#'   Recommended data: commercial fisheries landings, fish and invertebrates
#'
#' @param X dataframe of fishery independent survey data OR commercial landings
#'   data. Fishery independent survey data will have columns "YEAR", "ID",
#'   "SPECIES", and "BIOMASS" and/or "ABUNDANCE". "ID" is an area code
#'   designating where the observation was recorded. "SPECIES" is a numeric code
#'   indicating the species sampled. Commercial landings data will have columns
#'   "YEAR", "ID", "ALLCODES" and "CATCH". "ID" is an area code designating
#'   where the observation was recorded. "ALLCODES" is a numeric commercial
#'   species code indicating the species landed, and "CATCH" is the
#'   corresponding landed weight. Additional columns are required for each
#'   species group of interest. These columns have value of "1" in the rows
#'   species included in the group and "NA" in all other rows.
#' @param group character string indicating which species to include (Only
#'   applicable when X is research vessel survey data). Type ?speciesgroups for
#'   more information on species groups.
#' @param metric character string indicating whether X is fishery independent
#'   survey data or commercial landings data. "BIOMASS" and "ABUNDANCE" indicate
#'   fishery independent data. "CATCH" indicates commercial landings data. If
#'   metric = "BIOMASS" or metric = "ABUNDANCE", indicator will be named
#'   "SpeciesRichness". If metrc = "CATCH", indicator will be named
#'   "DiversityTargetSpp".
#' @param years vector of years for which to calculate indicator
#' @return Returns a dataframe with 3 columns. If metric = "BIOMASS" or metric =
#'   "ABUNDANCE", columns will be named "ID", "YEAR", "SpeciesRichness". If
#'   metrc = "CATCH", columns will be named  "ID", "YEAR", "DiversityTargetSpp".
#' @family biodiversity indicators
#' @family fishing pressure indicators
#' @references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   Greenstreet SP, Fraser HM, Rogers SI, Trenkel VM, Simpson SD, Pinnegar JK
#'   (2012) Redundancy in metrics describing the composition, structure, and
#'   functioning of the North Sea demersal fish community. ICES J Mar Sci
#'   69:8-22
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#' @export


speciesrichness <- function(X, group = c('FINFISH','ALL'), metric = c('BIOMASS','ABUNDANCE', "CATCH"),
                            years)  {

	if((metric == "BIOMASS" || metric == "ABUNDANCE") & group =='FINFISH') X <- X[X$SPECIES < 1000,]  # subset data to include only the species of interest
	if(metric == "CATCH" & group !='ALL') land <- land[which(land[group] == 1),]      # subset to species in "GROUP"
	
	uI = unique(X$ID)                   # extract the spatial scale ID's
	ind <- NULL                         # initialize dataframe for storing indicator values
	
	for (j in 1:length(uI)){            # loop over all spatal scales
	  
	  X.j = X[X$ID == uI[j], ]          # subset data to spatial scale j
	  
	  for (i in 1:length(years)){                     # loop over each year
	    
	    year.i = years[i]                             # set years.i to current year  
	    X.ij = X.j[X.j$YEAR == year.i, ]              # subset data to include only current year
	    
	    if(metric == "BIOMASS" || metric == "ABUNDANCE")  ind.i <- length(unique(X.ij$SPECIES))         # count the number of species recorded and store value
	    if(metric == "CATCH")  ind.i <- length(unique(X.ij$ALLCODES))   
	    
	    ind.i = data.frame(uI[j], year.i, ind.i)      # create a dataframe with spatial scale ID, year, and indicator value
	    ind = rbind(ind, ind.i)                       # bind ind.i to ind dataframe
	 	}
	}
	
	if(metric == "BIOMASS" || metric == "ABUNDANCE") names(ind) = c("ID", "YEAR", "SpeciesRichness")    # name the ind dataframe
	if(metric == "CATCH") names(ind) = c("ID", "YEAR", "DiversityTargetSpp")    # name the ind dataframe
	ind                                                                         # return vector of indicator values for years c(start.year:end.year) 
	
}
