\name{mercator}

\Rdversion{1.1}

\alias{mercator}

\title{
Mercator projection
}

\description{
Transform longitude/latiude points to the Mercator projection. The main purpose of this function is to illustrate rhumb lines in the vignette.
}

\usage{
mercator(p, r) 
}

\arguments{
  \item{p}{longitude/latitude of point(s). Can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a SpatialPoints* object}
  \item{r}{Numeric. Radius of the earth; default = 6378137 m} 
}

\value{
matrix
}

\author{
Robert Hijmans
}

\examples{
mercator(c(5,52))
}

\keyword{ spatial }
