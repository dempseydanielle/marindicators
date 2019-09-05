#'@title Calculates the intrinsic vulnerability index of the catch
#'@description This calculates the intrinsic vulnerability index (IVI) of the
#'  catch for \eqn{j} areas and \eqn{i} years.
#'@details The weighted intrinsic vulnerability index (IVI) of species in the
#'  catch was estimated as: \deqn{IVI = \Sigma(IVI_j * C_j)/\Sigma C_j} where
#'  the sum is over all species, j. The vulnerability for each species was
#'  determined by considering several traits including maximum length, age at
#'  first maturity, longevity, von Bertalanffy growth parameter, natural
#'  mortality, fecundity, spatial behaviour and geographic range (e.g. species
#'  with larger body size, higher longevity, higher age at maturity, and lower
#'  growth rates have higher vulnerability indexes and should be less able to
#'  sustain high fishing mortality; Cheung et al. 2005). The index values ranges
#'  from 1 to 100, with 100 being the most vulnerable. See Appendix 1 from
#'  Cheung et al., 2007:
#'  \url{http://www.int-res.com/articles/suppl/m333p001_app.pdf}
#'@inheritParams landings
#'@inheritParams CVBiomass
#'@param IVI.table A dataframe with columns \code{SPECIES} and the corresponding
#'  \code{IVI} (intrinsic vulnerability index). Entries in the \code{SPECIES}
#'  column should be the unique values of species codes in \code{land} (or a
#'  subset thereof). Other columns in \code{IVI.table} are ignored.
#'@return Returns a dataframe with three columns: \code{ID}, "YEAR", and
#'  \code{IVILandings}.
#'
#'  If there are no observations in land for spatial scale \eqn{j} in year
#'  \eqn{i}, indicator value is assigned \code{NA}.
#'@family stability and resistance indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Cheung WWL, Pitcher TJ, Pauly D. 2005. A fuzzy logic expert system to
#'  estimate intrinsic extinction vulnerability of marine fishes to fishing.
#'  Biol Conserv 124:97–111
#'
#'  Cheung WWL, Watson R, Morato T, Pitcher, TJ, Pauly, D. 2007. Intrinsic
#'  vulnerability in the global fish catch. Mar Ecol Prog Ser 333: 1 - 12
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy
#'@export

        
IVILandings <- function(land, IVI.table, negative = FALSE, years) {
 
  IVI.table <- na.omit(IVI.table[, c("SPECIES", "IVI")])
  land<- merge(land, IVI.table, by = "SPECIES")
  land$IV_num <- land$CATCH * land$IVI                    # multiply catch by IVI (for numerator)
  
  uI = unique(land$ID)                   # extract the spatial scale ID's
  ind <- NULL                            # initialize dataframe for storing indicator values
  
  for (j in 1:length(uI)){               # loop over all spatal scales
    
    land.j = land[land$ID == uI[j], ]    # subset data to spatial scale j
    
    for (i in 1:length(years)){          # loop over each year
      
      year.i = years[i]                             # set years.i to current year  
      land.ij = land.j[land.j$YEAR == year.i, ]     # subset data to include only current year
      
      if(nrow(land.ij) > 0){                                                     # set ind.i to NA if there are no observations in land.ij 
        ind.i <- merge(aggregate(IV_num ~ YEAR + ID, data = land.ij, FUN = sum), # calculate sum over all species of C*IVI
               aggregate(CATCH ~ YEAR + ID, data = land.ij, FUN=sum))            # calculate sum over all species of C
        
        ind.i <- ind.i[,3]/ind.i[,4]                                             # calculate IVI of landings
      } else ind.i <- NA
      
      ind.i = data.frame(uI[j], year.i, ind.i)     # create a dataframe with spatial scale ID, year, and indicator value
      ind = rbind(ind, ind.i)                      # bind ind.i to ind dataframe
    }
  }
  
  names(ind) = c("ID", "YEAR", "IVILandings")          # name the ind dataframe
  if(negative == TRUE) ind$IVILandings <- ind$IVILandings * (-1) # multiply indicator by -1
  ind <- ind[order(ind$ID), ]                          # order by ID to be consistent with other functions
  ind                                                  # return dataframe for years c(start.year:end.year) 
  
}






