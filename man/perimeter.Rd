\name{perimeter}
\Rdversion{1.1}

\alias{perimeter}

\title{
Compute the perimeter of a polygon 
}

\description{
Compute the perimeter of a polygon (or the length of a line) on a sphere 
}

\usage{
perimeter(xy, r=6378137)
}

\arguments{
  \item{xy}{Longitude/latitude of the points forming a polygon; Must be a matrix of 2 columns (first one is longitude, second is latitude) or a SpatialPolygons* or SpatialLines* object}
 \item{r}{radius of the earth; default = 6378137 m}  
}

\value{
Numeric. The perimeter or length. Unit is the square of the unit of r.
}


\author{
Robert Hijmans 
}


\examples{
xy <- rbind(c(-180,-20), c(-140,55), c(10, 0), c(-140,-60), c(-180,-20))
perimeter(xy)
}

\keyword{ spatial }

