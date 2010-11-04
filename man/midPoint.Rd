\name{midPoint}

\Rdversion{1.1}

\alias{midPoint}

\title{Mid-point}

\description{
Find the point half-way between two points along a great circle
}

\usage{
midPoint(p1, p2)
}


\arguments{
  \item{p1}{longitude/latitude of point(s). Can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a SpatialPoints* object}
  \item{p2}{As above}
}


\value{
A pair of coordinates (longitude/latitude)
}

\references{
\url{http://mathforum.org/library/drmath/view/51822.html}

\url{http://en.wikipedia.org/wiki/Great_circle_distance}

\url{http://www.movable-type.co.uk/scripts/latlong.html}
}

\author{
Robert Hijmans, based on code by Chris Vennes
}

\examples{
midPoint(c(0,0),c(90,90))  
}

\keyword{ spatial  }

