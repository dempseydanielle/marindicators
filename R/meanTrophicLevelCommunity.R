#'@title Calculates the mean trophic level of the community
#'@description This calculates the mean trophic level of the community weighted
#'  by biomass for \eqn{j} areas and \eqn{i} years.
#'@details Mean trophic level (TL): \deqn{TL = \Sigma TL_i*B_i)/\Sigma B_i}
#'  \eqn{TL_i} is trophic level of species \eqn{i}, and \eqn{B_i} is the biomass
#'  of species \eqn{i}.
#'
#'  This indicator is based on trophic levels of all species with available
#'  biomass time series, weighted by annual species-specific biomass, to reflect
#'  the structure of the community.
#'
#'  Recommended data: Fishery independent surveys, fish and invertebrates.
#'@param X dataframe of fishery independent survey data with columns "YEAR",
#'  "ID", "SPECIES", and "BIOMASS" and/or "ABUNDANCE". "ID" is an area code
#'  designating where the observation was recorded. "SPECIES" is a numeric code
#'  indicating the species sampled.
#'
#'  This function can also calculate the mean trophic level of the community
#'  from length-based data. To do so, X should be a dataframe with seven
#'  columns: "YEAR", "ID", and "SPECIES", are as above. "FLEN" indicates the
#'  size class, and "GROUP_NAME" and "TL" are the corresponding size-based
#'  functional group and trophic level, respectively. "BIOMASS" is the recorded
#'  biomass of the size-based functional groups. Note that one SPECIES code may
#'  be assigned more than one GROUP_NAME if ontogentic shifts occur. For
#'  example, on the Scotian Shelf small (< 33 cm) and large (>33 cm) haddock
#'  both have SPECIES code 11. Small haddock are assigend to GROUP_NAME
#'  "Haddock<33" with trophic level 3.38. Large haddock are assigned to
#'  GROUP_NAME "Haddock33+" with trophic level 3.43.
#'
#'@param TL.table dataframe with columns "SPECIES" and the corresponding "TL"
#'  (trophic level). Set to "NULL" if length.based = TRUE.
#'@param metric character string indicating whether to use "BIOMASS" or
#'  "ABUNDANCE" to calculate indicator.
#'@param years vector of years for which to calculate indicator.
#'@param length.based logical variable. length.based = TRUE indicates that X is
#'  length-based data. Default is length.based = FALSE
#'@return Returns a dataframe with 3 columns. "ID", "YEAR", and
#'  "MeanTLCommunity" (if length.based = FALSE) or "MeanTLCommunity_Length" if
#'  length.based = TRUE
#'@family ecosystem structure and function indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Christensen V. 1998. Fishery-induced changes in a marine ecosystem: insight
#'  frommodels of the Gulf of Thailand. J. Fish Bio. 53:128-142. Article No.
#'  jb980809
#'
#'  Shannon L, Coll M, Bundy A, Gascuel D, Heymans JJ, Kleisner K, Lynam CP,
#'  Piroddi C, Tam J, Travers-Trolet M, Shin Y. 2014. Trophic level-based
#'  indicators to track fishing impacts across marine ecosystems. Mar. Ecol.
#'  Prog. Ser. 512, 115–140.
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#'@export


meanTrophicLevelCommunity <- function(X,  TL.table, metric= c('ABUNDANCE', 'BIOMASS'), 
                                      length.based = FALSE, years) {
                                        
  if(length.based == FALSE) X <- merge(X, TL.table, by = 'SPECIES')     # Add trophic level data to RV survey data
                                              # Note that the merge function will drop species that do not have a TL
  if(length.based == TRUE) {
    inx <- X$FLEN==-99
    X[inx,'FLEN'] <- 10
  }
  
  uI = unique(X$ID)                           # extract the spatial scale ID's
  ind <- NULL                                 # initialize dataframe for storing indicator values
  
  for (j in 1:length(uI)){            # loop over all spatal scales
    
    X.j = X[X$ID == uI[j], ]          # subset data to spatial scale j
    
    for (i in 1:length(years)){                     # loop over each year
      
      year.i = years[i]                             # set years.i to current year  
      X.ij = X.j[X.j$YEAR == year.i, ]              # subset data to include only current year
      
      ind.i <- sum(X.ij[metric]*X.ij['TL'])/sum(X.ij[metric]) # calculate mean trophic level weighted by metric
      #ind.i <- aggregate(X.ij[metric]*X.ij['TL'],by=X.ij['ID'],FUN=sum)[,2]/aggregate(X.ij[metric],by=X.ij['ID'],FUN=sum)[,2]
      
      ind.i = data.frame(uI[j], year.i, ind.i)     # create a dataframe with spatial scale ID, year, and indicator value
      ind = rbind(ind, ind.i)                      # bind ind.i to ind dataframe
 
		}
  }
  
  if(length.based == FALSE) names(ind) = c("ID", "YEAR", "MeanTLCommunity")    # name the ind dataframe
  if(length.based == TRUE)  names(ind) = c("ID", "YEAR", "MeanTLCommunity_Length")
  ind <- ind[order(ind$ID), ]                          # order by ID to be consistent with other functions
  ind                                                # return dataframe of indicator values for years c(start.year:end.year) 
  
}