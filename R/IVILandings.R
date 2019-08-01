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
#'
#'  Recommended data: Commercial fisheries landings, fish
#'@param land dataframe of commercial landings data with columns "YEAR", "ID",
#'  "ALLCODES" and "CATCH". "ID" is an area code designating where the
#'  observation was recorded. "ALLCODES" is a numeric commercial species code
#'  indicating the species landed, and "CATCH" is the corresponding landed
#'  weight.
#'@param IVI.table dataframe with two columns: "SPECIES" and "IVI". "SPECIES" is
#'  the fisheries-independent numeric species code, and "IVI" is the
#'  corresponding intrinsic vulnerability index.
#'@param propland.table dataframe with three columns: "SPECIES", "ALLCODE", and
#'  "PROPORTION_OF_LANDINGS". "SPECIES" is is the fisheries-independent numeric
#'  species code (as in IVI.table), and "ALLCODES" is the corresponding numeric
#'  commercial species code (as in land). "PROPORTION_OF_LANDINGS" is relevant
#'  to species that have different SPECIES codes, but the same ALLCODES code.
#'  For example, on the Scotian Shelf, longhorn sculpins are assigned a SPECIES
#'  code of 300, while sea ravens are assigned a species code of 320; however
#'  they are grouped together in the commercial landings data and are both
#'  assigned ALLCODE 174. The "PROPORTION_OF_LANDINGS" column estimates the
#'  proportion of each species that makes up the commercial landings. In this
#'  example, longhorn sculpins consist of about 40% of the total sculpin
#'  landings and are assigned a "PROPORTION_OF_LANDINGS" value of 0.4. Sea
#'  ravens consist of about 60% of the total sculpin landings and are assigned a
#'  "PROPORTION_OF_LANDINGS" value of 0.6.
#'@param years vector of years for which to calculate indicator
#'@return returns a dataframe with three columns: "ID", "YEAR", and
#'  "IVILandings".
#'
#'  If there are no observations in land for spatial scale \eqn{j} in year
#'  \eqn{i}, indicator value is assigned NA.
#'@family stability and resistance indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Cheung, William W. L., watson, Reg, Morato, Telmo, Pitcher, Tony J., Pauly,
#'  Daniel (2007) Intrinsic vulnerability in the global fish catch. Mar Ecol
#'  Prog Ser 333: 1 - 12
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#'@export

        
IVILandings <- function(land, IVI.table, propland.table, years) {
 
  land<- merge(land, propland.table)
  land<- merge(land, IVI.table)

  land$CATCH <- land$CATCH * land$PROPORTION_OF_LANDINGS  # multiply catch by proportion of landings
  
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
  ind <- ind[order(ind$ID), ]                          # order by ID to be consistent with other functions
  ind                                                  # return dataframe for years c(start.year:end.year) 
  
}






