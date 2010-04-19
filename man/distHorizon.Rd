\name{distHorizon}

\alias{distHorizon}

\title{distance to horizon}

\description{
Compute distance to the horizon at a given altitude. 

The earth is assumed to be smooth, i.e. mountains and other obstacles are ignored. 
}

\usage{
distHorizon(h, r=6378137)
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
distHorizon(1.80) # me
distHorizon(324)  # Eiffer tower
}

\keyword{ spatial }
