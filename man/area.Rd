\name{areaPolygon}
\Rdversion{1.1}

 
\alias{areaPolygon}
\alias{areaPolygon,matrix-method}
\alias{areaPolygon,data.frame-method}
\alias{areaPolygon,SpatialPolygons-method}


\title{
Compute the areaPolygon of a polygon
}

\description{
Compute the area of a polygon on a sphere. Polygons should not self-intersect.
}

\usage{
areaPolygon(x, ...)
}

\arguments{
 \item{x}{Longitude/latitude of the points forming a polygon; Must be a matrix of 2 columns (first one is longitude, second is latitude) or a SpatialPolygons* object}
 \item{...}{Additional arguments. One implemented \code{r=6378137}, the radius of the earth; default = 6378137 m}  
}

\value{
area. Unit is the square of the unit of r (default is square meter).
}

\author{
Robert J. Hijmans. Based on code by Jason_Steven (\url{http://forum.worldwindcentral.com/showthread.php?p=69704})
}

\references{
Bevis, M. and G. Cambareri, 1987. Computing the area of a spherical polygon of arbitrary shape. Mathematical Geology 19: 335-346
}


\seealso{
\code{ \link[geosphere]{centroid}, \link[geosphere]{perimeter}  }
}


\examples{
p <- rbind(c(-180,-20), c(-140,55), c(10, 0), c(-140,-60), c(-180,-20))
areaPolygon(p)
}

\keyword{ spatial }

