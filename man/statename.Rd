\name{statename}
\alias{statename}

\title{
Baby name data, by state and year.
}
\description{
Retrieve baby name data for state-year combination.
}
\usage{
statename(year, state)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{year}{year, YYYY, numeric}
  \item{state}{two character state code, uppercase, i.e. "CA"}
}
\value{
  data.frame, 200 x 4. Columns are rank (1:100), sex (male, female),
  name, and number of births.}

\author{
Greg Hirson <ghirson@ucdavis.edu>
}
\examples{
\dontrun{statename(2000, "CA")}
}
