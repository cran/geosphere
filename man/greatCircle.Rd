\name{greatCircle}
\Rdversion{1.1}

\alias{greatCircle}

\title{
Great circle
}

\description{
Get points on a great circle as defined by the shortest distance between two specified points
}

\usage{
greatCircle(p1, p2, n=360) 
}

\arguments{
  \item{p1}{longitude/latitude of point(s), in degrees; can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a spatialPoints* object}
  \item{p2}{as above. Should have same length as p1, or a single point (or vice versa when p1 is a single point}
  \item{n}{The requested number of points on the Great Circle}
}


\value{
a matrix of points
}

\references{
\url{http://williams.best.vwh.net/avform.htm#Int}
}

\author{
Robert Hijmans based on a formula by Ed Williams
}


\examples{
greatCircle(c(5,52), c(-120,37), n=36)
}

\keyword{ spatial }
