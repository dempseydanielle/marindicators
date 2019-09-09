# If species.table is defined, this function subsets X to include only the species in group

speciesGroups <- function(X, species.table = NULL, group){

  if(group == "ALL") Y = X
  else{
    inx <- which(names(species.table) == group)
    Y = X[X$SPECIES %in% species.table[,inx], ]
  }
    Y
}

