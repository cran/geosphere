\name{distRhumb}
\Rdversion{1.1}

\alias{distRhumb}

\title{
Distance along a 'rhumb line'
}

\description{
A 'rhumb line' (or loxodrome) is a path of constant bearing, which crosses all meridians at the same angle.
}

\usage{
distRhumb(p1, p2, r=6378137)
}

\arguments{
  \item{p1}{longitude/latitude of point(s), in degrees; can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a spatialPoints* object}
  \item{p2}{as above. Should be of same length of p1, or a single point (or vice versa when p1 is a single point}
  \item{r}{radius of the earth; default = 6378137 m}
}

\details{
Rhumb (from the Spanish word for course, 'rumbo') lines are straight lines on a Mercator Projection map. They were used in navigation because it is easier to follow a constant compass bearing than to continually adjust the bearing as is needed to follow a great circle, though rhumb lines are normally longer than great-circle (orthodrome) routes. Most rhumb lines will gradually spiral towards one of the poles.
}

\value{
distance value in units of r (default=meters)
}

\references{
\url{http://www.movable-type.co.uk/scripts/latlong.html}
}

\author{
Chris Veness; ported to R by Robert Hijmans
}

\seealso{
\code{\link[geosphere]{distCosine}, \link[geosphere]{distHaversine}, \link[geosphere]{distVincentySphere}, \link[geosphere]{distVincentyEllipsoid}}
}

\examples{
distRhumb(c(0,0),c(90,90))
}

\keyword{ spatial  }

