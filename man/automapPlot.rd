\name{automapPlot}
\alias{automapPlot}
\title{Special plot function for automap}
\description{This function wraps around spplot and creates a blue-to-whitish colorscale instead of the standard bpy colorscale.}
\usage{
automapPlot(plot_data, 
	    zcol,  
	    col.regions, 
	    ...)
}
\arguments{
    \item{plot_data}{A spatial object that is to be plotted}
    \item{zcol}{The name of the column from \code{plot_data} you want to use. Can also be a list.}
	\item{col.regions}{Choose a colors that specify the fill colours.}
 	\item{...}{arguments that are passed on to \link[sp]{spplot}. A sp.layout object for example.}
}
\details{A good function to calculate the position of the colorbreaks the \code{classIntervals} function from the \code{classInt} package.}
\author{Paul Hiemstra, \email{p.hiemstra@geo.uu.nl}}
\seealso{\code{\link[sp]{spplot}}, \code{\link{plot.autoKrige}}, \code{\link{plot.posPredictionInterval}} }
\examples{
# Ordinary kriging
data(meuse)
coordinates(meuse) =~ x+y
data(meuse.grid)
gridded(meuse.grid) =~ x+y

kriging_result = autoKrige(zinc~1, meuse, meuse.grid)

# Adding the sp.layout parameter shows the locations of the measurements
automapPlot(kriging_result$krige_output, "var1.pred", 
	sp.layout = list("sp.points", meuse))
} 
