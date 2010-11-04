\name{perimeter}
\Rdversion{1.1}

\alias{perimeter}
\alias{perimeter,matrix-method}
\alias{perimeter,data.frame-method}
\alias{perimeter,SpatialPolygons-method}
\alias{perimeter,SpatialLines-method}
  


\title{
Compute the perimeter of a polygon 
}

\description{
Compute the perimeter of a polygon (or the length of a line) on a sphere 
}

\usage{
perimeter(x, ...)
}

\arguments{
 \item{x}{Longitude/latitude of the points forming a polygon; Must be a matrix of 2 columns (first one is longitude, second is latitude) or a SpatialPolygons* or SpatialLines* object}
 \item{...}{Additional arguments. One implemented \code{r=6378137}, the radius of the earth; default = 6378137 m}  
}

\value{
Numeric. The perimeter or length. Unit is that of r (default is m).
}

\seealso{
\code{ \link[geosphere]{area}, \link[geosphere]{centroid}  }
}


\author{
Robert Hijmans 
}


\examples{
xy <- rbind(c(-180,-20), c(-140,55), c(10, 0), c(-140,-60), c(-180,-20))
perimeter(xy)
}

\keyword{ spatial }

