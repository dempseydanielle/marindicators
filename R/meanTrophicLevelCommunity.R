#'@title Calculates the mean trophic level of the community
#'@description This function takes a dataframe with columns **** and calculates
#'  the mean trophic level of the community, weighted by biomass
#'@details Mean trophic level (TL): \deqn{TL = \Sigma TL_i*B_i)/\Sigma B_i}
#'  \eqn{TL_i} is trophic level of species \eqn{i}, and \eqn{B_i} is the biomass
#'  of species \eqn{i}.
#'
#'  This indicator is based on trophic levels of all species with available
#'  biomass time series, weighted by annual species-specific biomass, to reflect
#'  the structure of the community.
#'
#'  Recommended data: Fishery independent surveys, fish and invertebrates.
#'@param X add text here
#'@param lpred.data.table to delete?
#'@param metric set to biomass
#'@family ecosystem structure and function indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Christensen 1998 (not in tech report)
#'
#'  Shannon et al 2014
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}
#'@export


meanTrophicLevelCommunity <- function(X, TL.table = "scotianshelf",  metric= c('ABUNDANCE', 'BIOMASS'),
                                      years = c(start.year:end.year)) {
                                      #pred.data.table='indiseas_wss_tl', metric='BIOMASS') {
                                        
  if (TL.table == "indiseas") {               # for Scotian Shelf ecosystem, import stored IndiSeas data
    load("R/sysdata.rda/indiseas_TL.rda")
    TL.table <- indiseas_wss_tl
    rm(indiseas_wss_tl)
  }
  
  X <- merge(X, TL.table, by = 'SPECIES')     # Add trophic level data to RV survey data
                                              # Note that the merge function will drop species that do not have a TL
  uI = unique(X$ID)                          # extract the spatial scale ID's
  ind <- NULL                                 # initialize dataframe for storing indicator values
  
  for (j in 1:length(uI)){            # loop over all spatal scales
    
    X.j = X[X$ID == uI[j], ]          # subset data to spatial scale j
    
    for (i in 1:length(years)){                     # loop over each year
      
      year.i = years[i]                             # set years.i to current year  
      X.ij = X.j[X.j$YEAR == year.i, ]              # subset data to include only current year
      
      ind.i <- sum(X.ij[metric]*X.ij['AVG(TL)'])/sum(X.ij[metric]) # calculate mean trophic level weighted by metric
     
      ind.i = data.frame(uI[j], year.i, ind.i)     # create a dataframe with spatial scale ID, year, and indicator value
      ind = rbind(ind, ind.i)                      # bind ind.i to ind dataframe
 
		}
  }
  names(ind) = c("ID", "YEAR", "MeanTLCommunity")    # name the ind dataframe
  ind                                                # return vector of indicator values for years c(start.year:end.year) 
  
}