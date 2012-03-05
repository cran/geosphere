\name{geomean}

\docType{methods}

\alias{geomean}

\title{ 
Mean location of sperhical coordinates
}

\description{
mean location for spherical (longitude/latitude) coordinates.
}

\usage{
geomean(xy, w)
}

\arguments{
  \item{xy}{matrix with two columns (longitude/latitude), or a SpatialPoints or SpatialPolygons object with a longitude/latitude CRS}
  \item{w}{weights (vector of numeric values, with a length that is equal to the number of spatial features in \code{x}}
}

\value{
Ccoordinate pair (numeric)
}

\author{Robert J. Hijmans}

\keyword{methods}
\keyword{spatial}

