\name{horizon}

\alias{horizon}

\title{distance to horizon}

\description{
Partly empirical function to compute the distance to the horizon from a given altitude.  The earth is assumed to be smooth, i.e. mountains and other obstacles are ignored. 
}

\usage{
horizon(h, r=6378137)
}

\arguments{
  \item{h}{altitude, numeric >= 0. Should have the same unit as r}
  \item{r}{radius of the earth; default value is 6378137 m} 
}

\value{
A distance in units of \code{h} (default is meters)
}

\references{
\url{http://williams.best.vwh.net/avform.htm#Horizon}

Bowditch, 1995. American Practical Navigator. Table 12.
}


\author{ Robert J. Hijmans }

\examples{
horizon(1.80) # me
horizon(324)  # Eiffel tower
}

\keyword{ spatial }
