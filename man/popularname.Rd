\name{popularname}
\alias{popularname}
\title{
Baby name ranks, given a year
}
\description{
Retrieves the ranks of male and female baby names for a given year.
}
\usage{
popularname(year, top = 20, number = "n")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{year}{
year, as YYYY.
}
  \item{top}{
how many results to return, <= 1000.
}
  \item{number}{
"n" for raw number, "p" for percent of total.
}
}
\value{
  A data frame columns rank, sex, name, value. rank and value are numeric,
  sex and name are factors.
}

\author{
Greg Hirson <ghirson@ucdavis.edu>
}


\examples{

\dontrun{popularname(2000)}

}
