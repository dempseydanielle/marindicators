% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MeanTLLandings.R
\name{meanTLLandings}
\alias{meanTLLandings}
\title{Calculates the Mean Trophic Level or Marine Trophic Index of fisheries
 landings}
\usage{
meanTLLandings(land, TL.table, minTL = 0, years)
}
\arguments{
\item{land}{A dataframe of commercial landings data with columns \code{YEAR},
\code{ID}, \code{SPECIES} and \code{CATCH}. \code{YEAR} indicates the year
the landing was recorded, \code{ID} is an area code indicating where the
landing was recorded, \code{SPECIES} is a numeric code indicating the
species landed, and \code{CATCH} is the corresponding landed weight.}

\item{TL.table}{A dataframe with columns \code{SPECIES} and the corresponding
\code{TL_LAND} (trophic level). Entries in the \code{SPECIES} column should
be the unique values of species codes in \code{land} (or a subset thereof).
Other columns in \code{TL.table} are ignored.}

\item{minTL}{The minimum trophic level of species to include. Set \code{minTL
= 0} to calculate the mean trophic level of the landings; Set \code{minTL =
3.25} to calculate the marine trophic index. Default is \code{minTL = 0}.}

\item{years}{A vector of years for which to calculate indicator.}
}
\value{
Returns a dataframe with three columns: \code{ID}, \code{YEAR}, and if
 \code{minTL = 0}: \code{MeanTL.Landings}, if \code{minTL = 3.25}:
 \code{MTI.Landings}, or if \code{minTL} is a different value:
 \code{MeanTL.Landings_minTL}.

 If there are no observations in land for spatial scale \eqn{j} in year
 \eqn{i}, indicator value is set to \code{NA}.
}
\description{
This function calculates the Mean Trophic Level or Marine Trophic
 Index of fisheries landings for \eqn{j} areas and \eqn{i} years.
}
\details{
Mean trophic level of fisheries landings (\eqn{TL_{Land}}):
 \deqn{TL_{Land} = \Sigma (TL_i*Y_i)/Y} where \eqn{TL_i} is the trophic level
 of species \eqn{i}, \eqn{Y_i} is the landings of species \eqn{i}, and
 \eqn{Y} is the total landings of all species. Trophic Level of individual
 species can be estimated either through an Ecopath model or dietary
 analysis, or taken from a global database such as Fishbase.

 This indicator captures the average trophic level of the species exploited
 in the fishery. In general, it reflects a transition from long-lived, high
 trophic level, demersal fish toward short-lived, low trophic level pelagic
 fish and invertebrates (Pauly et al., 1998).

 The marine trophic index is calculated similarly to \eqn{TL_{Land}}, but
 only includes species with trophic level greater than or equal to an
 explicitly stated trophic level minTL. For instance, Pauly and Watson 2005
 adopted a trophic level minTL of 3.25 to emphasize changes in the relative
 abundance of higher trophic level fishes, and Shannon et al. 2014 used a
 minTL of 4.0 to examine changes within the apex predator community. If used
 in this way, this indicator highlights changes in the relative abundance of
 the more threatened high-trophic level fishes (Pauly et al., 1998).
}
\examples{
# Compile data
data(land)
data(species.info)

# Calculate indicators
# Mean trophic level of landings
meanTLLandings(land, TL.table = species.info, minTL = 0, years = c(2014:2019))
# Marine trophic index
meanTLLandings(land, TL.table = species.info, minTL = 3.25, years = c(2014:2019))
}
\references{
Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
 selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
 Aquat. Sci. 3232: xii + 212 p.

 Pauly D, Christensen V, Dalsgaard J, Froese R, Torres F. 1998. Fishing Down
 Marine Food Webs. Science 279:860-863

 Pauly D, Watson R. 2005. Background and interpretation of the Marine Trophic
 Index as a measure of biodiversity. Philos Trans R Soc B Biol Sci 360:415
 423

 Shannon L, Coll M, Bundy A, Gascuel D, Heymans, JJ, Kleisner K, Lynam CP,
 Piroddi C, Tam J, Travers-Trolet M and Shin Y. 2014. Trophic level-based
 indicators to track fishing impacts across marine ecosystems. Marine Ecology
 Progress Series, 512, pp.115-140.
}
\seealso{
Other fishing pressure indicators: 
\code{\link{allPressure}()},
\code{\link{fishingPressure}()},
\code{\link{landings}()},
\code{\link{speciesRichness}()}
}
\author{
Danielle Dempsey, Adam Cook \email{Adam.Cook@dfo-mpo.gc.ca},
 Catalina Gomez, Alida Bundy
}
\concept{fishing pressure indicators}
