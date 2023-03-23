\name{compare.cv}
\alias{compare.cv}
\title{Comparing the results of cross-validations}
\description{Allows comparison of the results from several outcomes of \code{\link{autoKrige.cv}} in both statistics and spatial plots 
	(bubble plots).}
\usage{
compare.cv(..., 
	   col.names, 
	   bubbleplots = FALSE, 
	   zcol = "residual", 
	   layout, 
	   key.entries, 
	   reference = 1, 
	   plot.diff = FALSE,
       digits = 4,
       ggplot = FALSE,
       addPoly = NULL) 
}
\arguments{
	\item{...}{\code{\link{autoKrige.cv}} objects that are compared to each other. Also accepts the output form krige.cv, these objects are transformed to \code{\link{autoKrige.cv}} objects.}
	\item{col.names}{Names for the different objects in \code{...}. This defaults to
		the names of the objects in \code{...}.}
	\item{bubbleplots}{logical, if \code{TRUE} then bubble plots of the objects in \code{...} are
			drawn using the same value for the color breaks.}
	\item{zcol}{Which column in the objects in \code{...} is going to be drawn in the bubbleplots. Options
			are: \code{var1.pred}, \code{var1.var}, \code{observed}, \code{residual} and \code{zscore}.}
	\item{layout}{\code{layout} of the bubbleplot, e.g. c(2,2). The argument gives the number of rows and columns
			in which the set of bubbleplots is to be drawn. Useful defaults are selected.}
	\item{key.entries}{A list of numbers telling what the key entries in the bubbleplots are. See \code{\link[sp]{bubble}} for more details.}
	\item{reference}{An integer telling which of the objects should be taken as a reference if \code{plot.diff} equals TRUE. \code{reference}
			equal to 1 means that the first object is the reference, \code{reference} equal to 2 means that the second object is the reference etc.}
    \item{plot.diff}{logical, if \code{plot.diff} is TRUE the number specified in \code{reference} defines the CV object that is taken as a reference 
            What is shown in the plot is reference data squared minus the other data squared. So the color red means that the CV is doing
            worse than the reference, vice-versa for green. This is very useful to see where the differences between the results are spatially and if 
            there is a pattern.}    
     \item{digits}{The number of significant digits in the resulting data.frame.}
     \item{ggplot}{logical, determines if spplot or ggplot2 is used to make the spatial plot of the cross-validation residuals. 
            Note that the \code{plot.diff} and \code{reference} arguments are obsolete when \code{ggplot} equals TRUE.}
     \item{addPoly}{if this object contains a \code{SpatialPolygons*} object, it is added to the plot as layout. Note that this only works
           when \code{ggplot} equals TRUE.}
}
\value{A data.frame with for each cross-validation result a number of diagnostics:
% \describe{
	\item{mean_error}{The mean of the cross-validation residual. Ideally small.}
    \item{me_mean}{mean error divided by the mean of the observed values, 
                   measure for how large the mean_error is in contrast to the mean of the dataset}
	\item{MSE}{Mean Squared error.}
	\item{MSNE}{Mean Squared Normalized Error, mean of the squared z-scores. Ideally small.}
	\item{cor_obspred}{Correlation between the observed and predicted values. Ideally 1.}
	\item{cor_predres}{Correlation between the predicted and the residual values. Ideally 0.}
	\item{RMSE}{Root Mean Squared Error of the residual. Ideally small.}
    \item{RMSE_sd}{RMSE divided by the standard deviation of the observed values. Provides a measure
                   variation of the residuals vs the variation of the observed values.}
	\item{URMSE}{Unbiased Root Mean Squared Error of the residual. Ideally small.}
	\item{iqr}{Interquartile Range of the residuals. Ideally small.}
% }
}
\author{Paul Hiemstra, \email{paul@numbertheory.nl}}
\seealso{\code{\link[gstat]{krige.cv}}, \code{\link[sp]{bubble}}, \code{\link{autofitVariogram}}, \code{\link{autoKrige.cv}},  }
\examples{
# Load the data
library(sp)
data(meuse)
coordinates(meuse) = ~x+y
data(meuse.grid)
gridded(meuse.grid) = ~x+y

# Perform cross-validation
kr.cv = autoKrige.cv(log(zinc)~1, meuse, model = c("Exp"), nfold = 10)
kr_dist.cv = autoKrige.cv(log(zinc)~sqrt(dist), meuse, 
       model = c("Exp"), nfold = 10)
kr_dist_ffreq.cv = autoKrige.cv(log(zinc)~sqrt(dist)+ffreq, 
       meuse, model = c("Exp"), nfold = 10)

# Compare the results
compare.cv(kr.cv, kr_dist.cv, kr_dist_ffreq.cv)
compare.cv(kr.cv, kr_dist.cv, kr_dist_ffreq.cv, 
           bubbleplots = TRUE)
compare.cv(kr.cv, kr_dist.cv, kr_dist_ffreq.cv, 
           bubbleplots = TRUE, col.names = c("OK","UK1","UK2"))
compare.cv(kr.cv, kr_dist.cv, kr_dist_ffreq.cv, 
           bubbleplots = TRUE, col.names = c("OK","UK1","UK2"), 
           plot.diff = TRUE)

library(ggplot2)
compare.cv(kr.cv, kr_dist.cv, kr_dist_ffreq.cv, 
           bubbleplots = TRUE, col.names = c("OK","UK1","UK2"), 
           ggplot = TRUE)

} 
