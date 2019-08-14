# this script returns a dataframe with columns "ID" and "YEAR"
# where each values of uI is matched to each year

createDataframe <- function(uI, years){
  
  DF <- NULL
  for(j in 1:length(uI)){
    ID.j <- rep(uI[j], times = length(years))
    DF.j <- data.frame(ID.j, years)
    DF <- rbind(DF, DF.j)
  }
  names(DF) <- c("ID", "YEAR")
  DF
}

