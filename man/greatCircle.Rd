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
  \item{p1}{longitude/latitude of point(s). Can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a SpatialPoints* object}
  \item{p2}{as above}
  \item{n}{The requested number of points on the Great Circle}
}


\value{
A matrix of points, or a list of such matrices (e.g., if multiple bearings are supplied) 
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
