\name{automapPlot}
\alias{automapPlot}
\title{Special plot function for automap}
\description{This function wraps around spplot and creates a blue-to-whitish colorscale instead of the standard bpy colorscale.}
\usage{
automapPlot(plot_data, zcol, col.regions, sp.layout, points,  ...)
}
\arguments{
    \item{plot_data}{A spatial object that is to be plotted, \link[sp]{sp} 
                 or \link[sf]{sf} }
    \item{zcol}{The name of the column from \code{plot_data} you want to use. Can also be a list.}
	\item{col.regions}{Choose a colors that specify the fill colours.}
	\item{sp.layout}{An sp.layout object that can be passed to \link[sp]{spplot}, 
	      to be added to the plot}
	\item{points}{Points that can be added to the plot}
 	\item{...}{other possible arguments that can be  passed on to  \link[sp]{spplot}.}
}
\details{The \code{classIntervals} function from the \code{classInt} package is 
          a good function to calculate the position of the colorbreaks.}
\author{Paul Hiemstra, \email{paul@numbertheory.nl}}
\seealso{\code{\link[sp]{spplot}}, \code{\link{plot.autoKrige}}, \code{\link{plot.posPredictionInterval}} }
\examples{
# Ordinary kriging
library(sp)
data(meuse)
coordinates(meuse) =~ x+y
data(meuse.grid)
gridded(meuse.grid) =~ x+y

kriging_result = autoKrige(zinc~1, meuse, meuse.grid)

# Adding the sp.layout parameter shows the locations of the measurements
automapPlot(kriging_result$krige_output, "var1.pred", 
	sp.layout = list("sp.points", meuse))
} 
