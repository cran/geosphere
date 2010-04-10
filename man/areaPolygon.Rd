\name{areaPolygon}
\Rdversion{1.1}

\alias{areaPolygon}

\title{
Compute the area of a polygon
}

\description{
Compute the area of a polygon on a sphere
}

\usage{
areaPolygon(xy, r=6378137)
}

\arguments{
  \item{xy}{Longitude/latitude of the points forming a polygon; Must be a matrix of 2 columns (first one is longitude, second is latitude) or a SpatialPolygons* object}
 \item{r}{radius of the earth; default = 6378137 m}  
}

\value{
Area. Unit is the square of the unit of r.
}


\examples{
xy <- rbind(c(-180,-20), c(-140,55), c(10, 0), c(-140,-60), c(-180,-20))
areaPolygon(xy)
}

\keyword{ spatial }

