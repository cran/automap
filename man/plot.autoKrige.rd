\name{plot.autoKrige}
\alias{plot.autoKrige}
\alias{plot.posPredictionInterval}
\title{Plot methods in automap}
\description{Defines methods to plot objects in automap.}
\usage{
\method{plot}{autoKrige}(x, sp.layout = NULL, ...)
\method{plot}{posPredictionInterval}(x, sp.layout = NULL, justPosition = TRUE, main = "Position prediction interval", ...)
}
\arguments{
    \item{x}{the object to plot (of class \code{autoKrige} or \code{posPredictionInterval})}
    \item{sp.layout}{An object that can contain lines, points and polygons
              that function as extra layout.}
	\item{justPosition}{logical, if FALSE: not only the plot with the position of the prediction interval is plotted,
			  but also plots with the upper and lower limits of the prediction interval.}
	\item{main}{Title of the plot for the position of the prediction interval.}
	\item{...}{arguments passed to lattice functions \link[lattice]{xyplot}, \link[sp]{spplot}
	          or \link[sf]{plot.sf}}
}
\details{For a detailed description of how \code{sp.layout} is constructed see \link[sp]{spplot}.}
\author{Paul Hiemstra, \email{paul@numbertheory.nl}}
\seealso{\code{\link[sp]{spplot}}, \code{\link{autoKrige}}, \code{\link{posPredictionInterval}} }
\examples{
# Ordinary kriging
library(sp)
library(sf)
data(meuse)
coordinates(meuse) =~ x+y
data(meuse.grid)
gridded(meuse.grid) =~ x+y

kriging_result = autoKrige(log(zinc)~1, meuse, meuse.grid)
# Adding the sp.layout parameter shows the locations of the measurements
plot(kriging_result, sp.layout = list(pts = list("sp.points", meuse)))

meuse = as(meuse, "sf")
meuse.grid = as(meuse.grid, "sf")

kriging_result = autoKrige(log(zinc)~1, meuse, meuse.grid)
# Adding the meuse points shows the locations of the measurements
plot(kriging_result, points = meuse)

} 
