\name{alongTrackDistance}
\Rdversion{1.1}

\alias{alongTrackDistance}

\title{
Along Track Distance
}

\description{
The along track distance is the distance from the start point (p1) to the closest point on the path to a third point (p3), 
following a great circle path defined by points p1 and p2
}

\usage{
alongTrackDistance(p1, p2, p3, r=6378137)
}

\arguments{
  \item{p1}{longitude/latitude of point(s), in degrees; can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a spatialPoints* object}
  \item{p2}{as above. Should have same length as p1, or a single point (or vice versa when p1 is a single point}
  \item{p3}{as above}
  \item{r}{radius of the earth; default = 6378137m}  
}


\value{
A distance in units of r (default is meters)
}

\author{
Ed Williams and Robert Hijmans
}


\seealso{
\code{ \link[geosphere]{crossTrackDistance}  }
}

\examples{
alongTrackDistance(c(0,0),c(60,60),c(50,40))
}

\keyword{ spatial }

