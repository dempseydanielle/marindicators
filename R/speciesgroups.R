#'@title Assigns species to species groups
#'@description I don't think I need to document this one because it is not
#'  exported for other people to use. 
#'@inheritParams shannon
#'@return text here
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'@author  Danielle Dempsey, Adam Cook \email{Adam.Cook@@dfo-mpo.gc.ca},
#'  Catalina Gomez, Alida Bundy


speciesGroups <- function(X, species.table = NULL, group){

  if(group == "ALL") Y = X
  else{
    inx <- which(names(species.table) == group)
    Y = X[X$SPECIES %in% species.table[,inx], ]
  }
    Y
}

