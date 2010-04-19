\name{crossTrackDistance}
\Rdversion{1.1}

\alias{crossTrackDistance}

\title{
Cross Track Distance
}

\description{
The cross track distance (or cross track error) is the distance of a point from a great-circle path. The great circle path is defined by \code{p1} and \code{p2}, while \code{p3} is the point away from the path.
}

\usage{
crossTrackDistance(p1, p2, p3, r=6378137)
}

\arguments{
  \item{p1}{Start of great circle path. longitude/latitude of point(s). Can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a SpatialPoints* object}
  \item{p2}{End of great circle path. As above}
  \item{p3}{Point away from the great cricle path. As for p2}
  \item{r}{radius of the earth; default = 6378137}  
}


\value{
A distance in units of \code{r} (default is meters)

The sign indicates which side of the path p3 is on. Positive  means right of the course from p1 to p2, negative means left.
}


\author{
Ed Williams and Robert Hijmans
}


\seealso{
\code{ \link[geosphere]{alongTrackDistance}  }
}

\references{
\url{http://www.movable-type.co.uk/scripts/latlong.html}

\url{http://williams.best.vwh.net/ftp/avsig/avform.txt}
}


\examples{
crossTrackDistance(c(0,0),c(90,90),c(80,80))
}


\keyword{ spatial }
