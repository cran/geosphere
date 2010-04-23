\name{gcIntermediate}
\Rdversion{1.1}

\alias{gcIntermediate}

\title{
Intermediate points on a great circle
}

\description{
Get intermediate points on a Great Circle inbetween the two points used to define the Great Circle 
}

\usage{
gcIntermediate(p1, p2, n=50) 
}

\arguments{
  \item{p1}{Longitude/latitude of a single point, in degrees; can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a SpatialPoints* object}
  \item{p2}{As above}
  \item{n}{The requested number of points on the Great Circle}
}


\value{
a matrix of points
}

\references{
\url{http://williams.best.vwh.net/avform.htm#Intermediate}
}

\author{
Robert Hijmans based on code by Ed Williams
}


\examples{
gcIntermediate(c(5,52), c(-120,37), n=10)
}

\keyword{ spatial }
