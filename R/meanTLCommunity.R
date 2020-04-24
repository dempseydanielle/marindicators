#'@title Calculates the Mean Trophic Level of the community
#'@description This function calculates the Mean Trophic Level of the community
#'  weighted by biomass for \eqn{j} areas and \eqn{i} years.
#'@details Mean trophic level (TL): \deqn{TL = \Sigma (TL_i*B_i)/\Sigma B_i}
#'  \eqn{TL_i} is trophic level of species \eqn{i}, and \eqn{B_i} is the biomass
#'  of species \eqn{i}.
#'
#'  This indicator is based on trophic levels of all species with available
#'  biomass time series, weighted by annual species-specific biomass, to reflect
#'  the structure of the community (Christensen, 1998).
#'@inheritParams biomassPerTL
#'@param TL.table A dataframe with columns \code{SPECIES} and the corresponding
#'  \code{TL} (trophic level). Entries in the \code{SPECIES} column should be
#'  the unique values of species codes in \code{X} (or a subset thereof). Other
#'  columns in \code{TL.table} are ignored.
#'@return Returns a dataframe with 3 columns. \code{ID}, \code{YEAR}, and
#'  \code{MeanTLCommunity}.
#'
#'  If there is no data for spatial scale \eqn{j} in year \eqn{i}, indicator
#'  value is assigned \code{NA}.
#'@family ecosystem structure and function indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Christensen V. 1998. Fishery-induced changes in a marine ecosystem: insight
#'  from models of the Gulf of Thailand. J. Fish Bio. 53:128-142. Article No.
#'  jb980809
#'
#'  Shannon L, Coll M, Bundy A, Gascuel D, Heymans JJ, Kleisner K, Lynam CP,
#'  Piroddi C, Tam J, Travers-Trolet M, Shin Y. 2014. Trophic level-based
#'  indicators to track fishing impacts across marine ecosystems. Mar. Ecol.
#'  Prog. Ser. 512, 115–140.
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@examples
#'# Compile data
#'data(X)
#'data(species.info)
#'
#'# Calculate indicator
#'meanTLCommunity(X, TL.table = species.info, metric = "BIOMASS", years = c(2014:2019))
#'@export


meanTLCommunity <- function(X, TL.table, metric= "BIOMASS", years) {
     
  TL.table <- na.omit(TL.table[, c("SPECIES", "TL")])                                   
  X <- merge(X, TL.table, by = 'SPECIES')     # Add trophic level data to RV survey data
                                              # Note that the merge function will drop species that do not have a TL

  uI = unique(X$ID)                           # extract the spatial scale ID's
  ind <- NULL                                 # initialize dataframe for storing indicator values
  
  for (j in 1:length(uI)){            # loop over all spatal scales
    
    X.j = X[X$ID == uI[j], ]          # subset data to spatial scale j
    
    for (i in 1:length(years)){                     # loop over each year
      
      year.i = years[i]                             # set years.i to current year  
      X.ij = X.j[X.j$YEAR == year.i, ]              # subset data to include only current year
      
      if(nrow(X.ij) > 0){                           # if there are no observations in X.ij, ind.i is set is to NA
        ind.i <- sum(X.ij[metric]*X.ij['TL'])/sum(X.ij[metric]) # calculate mean trophic level weighted by metric
      } else ind.i <- NA
      
      ind.i = data.frame(uI[j], year.i, ind.i)     # create a dataframe with spatial scale ID, year, and indicator value
      ind = rbind(ind, ind.i)                      # bind ind.i to ind dataframe
 
		}
  }
  
  names(ind) = c("ID", "YEAR", "MeanTLCommunity")    # name the ind dataframe
  ind <- ind[order(ind$ID), ]                          # order by ID to be consistent with other functions
  ind                                                  # return dataframe of indicator values for years c(start.year:end.year) 
  
}