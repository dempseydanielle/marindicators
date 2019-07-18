#' @title Calculates fishing pressure
#' @description This function takes a dataframe with columns **** and calculates
#'   fishing pressure on a fished group
#' @details Fishing pressure (FP): \deqn{FP = Y_{FG}/B_{FG}} where \eqn{B_{FG}} is the
#'   biomass of the fished group(s) and \eqn{Y_{FG}} is the landed catch of the
#'   group(s). **Give examples of groups that can be used in function
#'
#'   This indicator measures the level of exploitation or total fishing pressure
#'   at the ecosystem level. Change in this indicator can result from change in
#'   \eqn{B_{FG}}, \eqn{Y_{FG}} or both. If \eqn{B_{FG}} and \eqn{Y_{FG}} change in the
#'   same direction, exploitation rate may not change.
#'
#'   Recommended data: \eqn{B_{FG}}: fishery independent surveys, \eqn{Y_{FG}}: commercial fisheries
#'   landings
#' @param path add text here
#' @param land add text here
#' @param group add text here
#' @param groups add text here
#' @param qadj add text here
#' @param qadjPostStrat add text here
#' @family fishing pressure indicators
#' @references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   Shin YJ, Bundy A, Shannon LJ, Simier M, Coll M, Fulton EA, Link JS, Jouffre
#'   D, Ojaveer H, MacKinson S, Heymans JJ, Raid T (2010) Can simple be useful
#'   and reliable? Using ecological indicators to represent and compare the
#'   states of marine ecosystems. ICES J Mar Sci 67:717–731
#'
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export


FishingPressure <- function(X, land, group){
                            
  B <- resourcePotential(X, metric = "BIOMASS", group = group)  # calculate the biomass of "group" in the community
  Y <- LandByGroup(land, group = group)                         # calculate the landings of "group"
  
  ind <- data.frame(cbind(B$ID, B$YEAR))                        # initialize dataframe to store indicator values
  ind$FP <- Y[,3]/B[,3]                                         # calculate fishing pressure
  
  ind.name <- paste("FP", "_", group, sep ="")                  # name indicator: FP_group
  names(ind) <- c("ID", "YEAR", ind.name)
  ind                                                           # return indicator dataframe
  
}
