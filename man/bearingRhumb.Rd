\name{bearingRhumb}
\Rdversion{1.1}

\alias{bearingRhumb}

\title{
Rhumbline bearing
}

\description{
Bearing (direction of travel; true course) along a rhumb line (loxodrome) between two points. 
}

\usage{
bearingRhumb(p1, p2)
}

\arguments{
  \item{p1}{longitude/latitude of point(s), in degrees; can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a spatialPoints* object}
  \item{p2}{as above. Should have same length as p1, or a single point (or vice versa when p1 is a single point}
}


\value{
A bearing in degrees
}

\references{
\url{http://williams.best.vwh.net/avform.htm#Rhumb}

\url{http://en.wikipedia.org/wiki/Rhumb_line}
}

\author{
Chris Veness and Robert Hijmans, based on formulae by Ed Williams
}

\note{
Unlike most great circles, a rhumb line is a line of constant bearing, i.e. tracks of constant true course.
The meridians and the equator are both rhumb lines and great circles. Rhumb lines approaching a pole becomes a tightly wound spiral.
}

\seealso{
\code{ \link[geosphere]{bearing},  \link[geosphere]{distRhumb}  }
}

\examples{
bearingRhumb(c(0,0),c(90,90))
}

\keyword{ spatial }

