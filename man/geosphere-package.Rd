\name{geosphere-package}
\alias{geosphere-package}

\alias{geosphere}

\docType{package}

\title{Geosphere}

\description{
This package implements spherical trigonometry functions for geographic applications. There are functions to compute distance and direction (bearing, azimuth, course) along Great Circles (= shortest distance on a sphere, or "as the crow flies") and along rhumb lines (lines of constant direction) as well as functions to compute intersections of great circles, and of rhumb lines. There are also functions to compute the distance between points and polylines, and to characterize spherical polygons; for random sampling on a sphere, and to compute daylength. See the vignette \code{vignette('geosphere')} for examples.

Geographic locations must be specified in latitude and longitude in degrees (NOT radians). Degrees are (obviously) in decimal notation. Thus 12 degrees, 30 minutes, 10 seconds = 12 + 30/60 + 10/3600 =  12.50278 degrees. The Southern and Western hemispheres have a negative sign.

The default unit of distance is meter; but this can be adjusted by supplying a different radius \code{r} to functions.

Directions are expressed in degrees (N = 0 and 360,  E = 90, S = 180, and W = 270 degrees).


}

\author{
Robert Hijmans, Ed Williams, Chris Veness

Maintainer: Robert J. Hijmans <r.hijmans@gmail.com>
}

\references{
\url{http://williams.best.vwh.net/avform.htm}

\url{http://www.movable-type.co.uk/scripts/latlong.html}

\url{http://en.wikipedia.org/wiki/Great_circle_distance}

\url{http://mathworld.wolfram.com/SphericalTrigonometry.html}
}

\section{Acknowledgements}{
David Purdy, Bill Monahan, and George Wang for suggestions to improve the package.
}


\keyword{ package }
\keyword{ spatial }

