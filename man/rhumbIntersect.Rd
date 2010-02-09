\name{rhumbIntersect}
\Rdversion{1.1}

\alias{rhumbIntersect}

\title{
Intersecting radials
}

\description{
Intersection between two rhumb lines defined by their point of origin and true bearing
}

\usage{
rhumbIntersect(p1, brng1, p2, brng2) 
}

\arguments{
  \item{p1}{longitude/latitude of point(s), in degrees; can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a spatialPoints* object}
  \item{brng1}{True bearing from p1}
  \item{p2}{As above. Should have same length as p1, or a single point (or vice versa when p1 is a single point}
  \item{brng2}{True bearing from p2}
}


\value{
a point
}

\references{
\url{http://williams.best.vwh.net/avform.htm#Rhumb}

\url{http://en.wikipedia.org/wiki/Rhumb_line}
}

\author{
Robert Hijmans based on code by Ed Williams
}


\examples{
rhumbIntersect(c(10,0), 10, c(-10,0), 10)
}

\keyword{ spatial }
