# standardizes each of the indicators in x
# x is a dataframe. First column is YEAR, second column is ID (or vice-versa)
## and the remaining columns are indicators

standardize <- function(x) {
  
  n.col <- ncol(x)                 # number of columns in x
  uI <- unique(x$ID)               # unique areas codes in x
  ind.names <- names(x)[3:n.col]   # indicator names
  
  df <- NULL                       # initialize output dataframe
  
  for(j in 1:length(uI)) {         # loop over all areas
    
    xj <- x[x$ID == uI[j], ]       # subset x to area of interest
    
    xj[, 3:n.col] <- apply(xj[, 3:n.col], MARGIN = 2, FUN = scale) # standardize each indicator
    
    df <- rbind(df, xj)            #rbind standardized indicators from different areas
    
  }
  
  names(df)[3:n.col] <- paste(ind.names, "_s", sep="") # add "_s" to indicator names
  df
  
  }

