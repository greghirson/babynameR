\name{babyname}
\alias{babyname}

\title{
Data for an individual baby name.
}
\description{
Retrieves the yearly rank for a baby name
}
\usage{
babyname(name, sex = "N", nyrs = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
baby name (character)
}
  \item{sex}{
N - both, M - male, or F - female (character)
}
  \item{nyrs}{
number of years backwards from last reported year.
}
}
\value{
a data.frame with two numeric columns: year and rank.  
}
\author{
Greg Hirson <ghirson@ucdavis.edu>
}
\examples{
\dontrun{babyname("Gregory")}
}
