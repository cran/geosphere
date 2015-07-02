\name{bearing}
\Rdversion{1.1}

\alias{bearing}

\title{
Direction of travel
}

\description{
Get the initial bearing (direction; azimuth) to go from point \code{p1} to point \code{p2} (in longitude/latitude) following the shortest path on an ellipsoid (geodetic) or a sphere (great circle). Note that the bearing of travel changes continuously while going along the path. A route with constant bearing is a rhumb line (see  \code{\link[geosphere]{bearingRhumb}}).
}

\usage{
bearing(p1, p2, a=6378137, f=1/298.257223563, sphere=FALSE)
}

\arguments{
  \item{p1}{longitude/latitude of point(s). Can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a SpatialPoints* object}
  \item{p2}{as above}
   \item{a}{major (equatorial) radius of the ellipsoid. The default value is for WGS84 }
  \item{f}{ellipsoid flattening. The default value is for WGS84 }
  \item{sphere}{logical. If \code{TRUE}, the bearing is computed for a sphere, instead of for an ellipsoid}  
}


\value{
Bearing in degrees
}


\author{
Robert Hijmans
}


\seealso{
\code{ \link[geosphere]{bearingRhumb}  }
}

\examples{
bearing(c(10,10),c(20,20))
}

\references{
\url{http://williams.best.vwh.net/ftp/avsig/avform.txt}

\url{http://www.movable-type.co.uk/scripts/latlong.html}
}


\keyword{ spatial }

