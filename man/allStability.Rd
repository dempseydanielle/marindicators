% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allStability.R
\name{allStability}
\alias{allStability}
\title{Calculates all Stability and Resistance indicators}
\usage{
allStability(
  X,
  land,
  maxlength.group,
  species.table,
  speciesinfo.table,
  TL.grouping = 1,
  wind = 5,
  negative = FALSE,
  years,
  raw = TRUE,
  std = TRUE,
  glob.env = TRUE,
  export.path = NULL,
  export.id = NULL
)
}
\arguments{
\item{X}{A dataframe of fishery independent data derived from research vessel
survey data or model output, with columns \code{YEAR}, \code{ID},
\code{SPECIES}, \code{BIOMASS}, and \code{ABUNDANCE}. \code{YEAR} indicates
the year the observation was recorded, \code{ID} is an area code indicating
where the observation was recorded, \code{SPECIES} is a numeric code
indicating the species sampled, and \code{BIOMASS}/\code{ABUNDANCE} is the
corresponding biomass/abundance (stratified and corrected for catchability
as required).}

\item{land}{A dataframe of commercial landings data with columns \code{YEAR},
\code{ID}, \code{SPECIES} and \code{CATCH}. \code{YEAR} indicates the year
the landing was recorded, \code{ID} is an area code indicating where the
landing was recorded, \code{SPECIES} is a numeric code indicating the
species landed, and \code{CATCH} is the corresponding landed weight.}

\item{maxlength.group}{A character string indicating the species group for
which to calculate the mean maximum length of fish in the community. Must be
set to \code{"ALL"} or match a column name in \code{species.table}. If
\code{maxlength.group = NULL}, the mean maximum length will not be
calculated.}

\item{species.table}{A table where the column names match the entries in
\code{resource.groups}, \code{ratio.groups}, and/or \code{condition.groups}.
Column entries are the species codes indicating the species from \code{X}
(or \code{X_length}) included in each group. \code{species.table} may also
include columns for other species groups; these will be ignored.}

\item{speciesinfo.table}{A table with columns \code{SPECIES} and the
corresponding \code{TL}, \code{MAXLENGTH}, \code{MAXAGE}, and \code{IVI}
(trophic level, maximum recorded age, maximum recorded length, and intrinsic
vulnerability index). Entries in the \code{SPECIES} column should be the
unique values of species codes in \code{X} (or a subset thereof). If there
are different species codes in \code{X} and \code{land}, the Intrinsic
Vulnerability Index of the Landings indicator should be calculated
separately using the function \code{IVILandings()}.}

\item{TL.grouping}{Size of the trophic level bin for which to aggregate
biomass when calculating Biomass per Trophic Level. For example, if
\code{TL.grouping = 1}, trophic levels are binned from 1.00 - 1.99, 2.00 -
2.99, etc. If TL.grouping = 0.5, trophic levels are binned from 1.00 - 1.49,
1.50 - 1.99, 2.00 - 2.49, 2.50 - 2.99, etc. Default is \code{TL.grouping =
1} so that biomass is aggregated over discrete trophic levels.}

\item{wind}{Window for the moving average used to calculate the Coefficient of
Variation of the Biomass. The first and last \code{floor(wind/2)} values of
the indicator are assigned \code{NA} to account for the moving average.
Default is \code{wind = 5} years.}

\item{negative}{If \code{negative = TRUE}, the Coefficient of Variation of the
Biomass will be multiplied by -1 so that the expected response is to
decrease with increasing fishing pressure. Default is \code{negative =
FALSE}.}

\item{years}{A vector of years for which to calculate indicators.}

\item{raw}{A logical value. If \code{raw = TRUE}, the raw indicator values are
returned by the function. If \code{raw = FALSE}, the raw indicator values
are not returned. Default is \code{raw = TRUE}. Either \code{raw} or
\code{std} must be \code{TRUE}.}

\item{std}{A logical value. If \code{std = TRUE}, the standardized indicator
values for each area \code{ID} are returned by the function. Indicators are
standardized using z-scores, i.e., by subtracting the mean and dividing by
the standard deviation (ignoring \code{NA} values). If \code{std = FALSE},
the standardized indicator values are not returned. Default is \code{std =
TRUE}. Either \code{raw} or \code{std} must be \code{TRUE}.}

\item{glob.env}{Logical value indicating whether to return output to global
environment. Default is \code{glob.env = TRUE}.}

\item{export.path}{File path indicating where to save a .csv file of
calculated indicators (named stability_export.id.csv; see below). If
\code{export.file = NULL}, the indicator dataframe will not be exported as a
.csv file. Default is \code{export.path = NULL}.}

\item{export.id}{Character string to modify the name of the .csv file (if
export.path is specified), for example an area name or date of analysis. The
exported .csv file is named stability_export.id.csv. Default is
\code{export.id = NULL}.}
}
\value{
Returns a dataframe with columns \code{ID}, \code{YEAR}, and
 indicators corresponding to the arguments supplied to the function.
 Standardized indicators are noted with \code{_s} in the name.
}
\description{
This function calculates all (or a subset) of the Stability and
 Resistance indicators for \eqn{j} areas and \eqn{i} years. The user can
 choose whether the function returns the indicator dataframe to the global
 environment, exports the dataframe to a .csv file, or both. The user can
 also choose whether the function returns the raw indicator values, the
 standardized (z-score) values, or both.
}
\details{
This function calculates the Stability and Resistance indicators:
 Biomass per Trophic Level, Intrinsic Vulnerability Index, Coefficient of
 Variation of the Biomass, Mean Lifespan, and Mean Maximum Length (weighted
 by biomass and abundance). If data are not available to calculate one or
 more of these indicators, a subset will be returned. See the help file for
 the individual functions for information on how each indicator is
 calculated.


 Notes on indicator calculations: In the individual functions, the user
 generally has control over which metric (e.g., \code{BIOMASS} or
 \code{ABUNDANCE}) is used to calculate the indicator.  Here, \code{BIOMASS}
 is used to calculate Coefficient of Variation of the Biomass, Mean Lifespan,
 Biomass per Trophic Level, and Mean Maximum Length weighted by biomass.
 \code{ABUNDANCE} is used to calculate Mean Maximum Length weighted by
 abundance. See individual functions for more flexibility.
}
\examples{
# Compile data
data(X)
data(land)
data(species.info)
data(species.table)

# Calculate raw and standardized indicators
allStability(X = X, land = land, maxlength.group = "FINFISH",
   species.table = species.table, speciesinfo.table = species.info,TL.grouping = 1,
   wind = 5, negative = FALSE, years = c(2014:2019), raw = TRUE, std = TRUE)
}
\references{
Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
 selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
 Aquat. Sci. 3232: xii + 212 p.
}
\seealso{
Other stability and resistance indicators: 
\code{\link{CVBiomass}()},
\code{\link{IVILandings}()},
\code{\link{biomassPerTL}()},
\code{\link{meanMaxAge}()},
\code{\link{meanMaxLength}()}
}
\author{
Danielle Dempsey, Adam Cook \email{Adam.Cook@dfo-mpo.gc.ca},
 Catalina Gomez, Alida Bundy
}
\concept{stability and resistance indicators}
