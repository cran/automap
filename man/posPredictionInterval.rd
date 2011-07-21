\name{posPredictionInterval}
\alias{posPredictionInterval}
\title{Determines the position of the p\% prediction interval}
\description{This function calculates the \code{p}\% prediction interval and
determines the position of this interval relative to \code{value}. This can be
higher, lower or not distinguishable.}
\usage{posPredictionInterval(krige_object, 
                      p = 95, 
                      value = median(krige_object$krige_output$var1.pred))}
\arguments{
    \item{krige_object}{The result of from the autoKrige procedure. This is
                expected to be a autoKrige-object.}
    \item{p}{The \code{p}\% percent prediction interval is compared to value}
    \item{value}{The value to which the the \code{p}\% prediction interval compared}
}
\value{The output object is of class \code{posPredictionInterval} and contains the results
of the function in an \link[sp]{Spatial-class} object similar to the one in the input object. This means that
if the input object containes a grid, the results are also returned on that same grid.
Also included in the return object are the values for \code{p} and \code{value}. }
\author{Paul Hiemstra, \email{paul@numbertheory.nl}}
\seealso{\code{\link{autoKrige}}, \code{\link{autofitVariogram}}}
\examples{
data(meuse)
coordinates(meuse) =~ x+y
data(meuse.grid)
gridded(meuse.grid) =~ x+y

kriging_result = autoKrige(zinc~1, meuse, meuse.grid)
pos = posPredictionInterval(kriging_result, 95, 75)
plot(pos)
}
