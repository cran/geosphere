\name{areaPolygon}
\Rdversion{1.1}

 
\alias{areaPolygon}
\alias{areaPolygon,matrix-method}
\alias{areaPolygon,data.frame-method}
\alias{areaPolygon,SpatialPolygons-method}


\title{
Area of a longitude/latitude polygon
}

\description{
Compute the area of a polygon in longitude/latitude coordiantes on an ellipsoid
}

\usage{
\S4method{areaPolygon}{matrix}(x, a=6378137, f=1/298.257223563, ...)

\S4method{areaPolygon}{SpatialPolygons}(x, a=6378137, f=1/298.257223563, ...)
}

\arguments{
 \item{x}{longitude/latitude of the points forming a polygon; Must be a matrix of 2 columns (first one is longitude, second is latitude) or a SpatialPolygons* object}
 \item{a}{major (equatorial) radius of the ellipsoid}
 \item{f}{ellipsoid flattening. The default value is for WGS84 }
 \item{...}{Additional arguments. None implemented}  
}

\value{
area in square meters
}

\author{
This function calls GeographicLib code by C.F.F. Karney
}

\references{
C.F.F. Karney, 2013. Algorithms for geodesics, J. Geodesy 87: 43-55. \url{https://dx.doi.org/10.1007/s00190-012-0578-z}. Addenda: \url{http://geographiclib.sf.net/geod-addenda.html}. Also see \url{http://geographiclib.sourceforge.net/}
}


\seealso{
\code{ \link[geosphere]{centroid}, \link[geosphere]{perimeter}  }
}


\examples{
p <- rbind(c(-180,-20), c(-140,55), c(10, 0), c(-140,-60), c(-180,-20))
areaPolygon(p)
}

\keyword{ spatial }

