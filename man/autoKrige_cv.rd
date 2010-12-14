\name{autoKrige.cv}
\alias{autoKrige.cv}
\title{Automatic cross-validation}
\description{Uses \code{\link{autofitVariogram}} to fit a variogram model to the data and then calls
			 \code{\link[gstat]{krige.cv}} to perform cross-validation.}
\usage{
autoKrige.cv(formula, 
	     input_data, 
	     data_variogram = input_data,
	     model = c("Sph", "Exp", "Gau", "Ste"), 
	     kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10), 
	     fix.values = c(NA,NA,NA), 
	     verbose = c(FALSE,TRUE), 
	     GLS.model = NA,
         start_vals = c(NA,NA,NA),
         miscFitOptions = list(),
	     ...)
}
\arguments{
	\item{formula}{formula that defines the dependent variable as a linear model
				of independent variables; suppose the dependent variable has
				name 'z', for ordinary and simple kriging use the formula
				'z~1'; for simple kriging also define 'beta' (see below); for
				universal kriging, suppose 'z' is linearly dependent on 'x'
				and 'y', use the formula 'z~x+y'.}
    \item{input_data}{An object of the 
	            \link[sp]{SpatialPointsDataFrame-class} containing the data to be interpolated.}
    \item{data_variogram}{An optional way to provide a different dataset for
                the building of the variogram.}
	\item{model}{List of models that will be tested during automatic variogram fitting.}
     \item{kappa}{List of values for the smoothing parameter of the Matern model that will be tested during automatic variogram fitting.}
    \item{fix.values}{Can be used to fix a variogram parameter to a certain value. It 
                 consists of a list with a length of three. The items describe the
                 fixed value for the nugget, range and sill respectively. Setting
                 the value to NA means that the value is not fixed. Is passed on to autofitVariogram.}
	\item{verbose}{vector of 2 logicals. The first element sets the verbosity of autofitVariogram, see its documentation
                 for more information. The second element sets the verbosity level of krige.cv, see its documentation
                 for more information.}
	\item{GLS.model}{If a variogram model is passed on through this parameter a Generalized Least Squares 
				 sample variogram is calculated.} 
    \item{start_vals}{Can be used to give the starting values for the variogram fitting. The items describe the
                 fixed value for the nugget, range and sill respectively. They need to be given in that order.
                 Setting the value to NA means that the value will be automatically chosen.} 
    \item{miscFitOptions}{Additional options to set the behavior of \link{autofitVariogram}. For details see the 
                 documentation of \link{autofitVariogram}.}
	\item{...}{arguments passed to \code{\link[gstat]{krige.cv}}}
}
\value{\code{autoKrige.cv} returns an object of class \code{autoKrige.cv}. This is a list
		containing one object of class \code{SpatialPointsDataFrame} with the results of
		the cross-validation, see \code{\link[gstat]{krige.cv}} for more details. The
		attribute name is \code{krige.cv_output}.}
\author{Paul Hiemstra, \email{p.hiemstra@geo.uu.nl}}
\seealso{\code{\link[gstat]{krige.cv}}, \code{\link{autofitVariogram}}, \code{\link{compare.cv}} }
\examples{
data(meuse)
coordinates(meuse) = ~x+y
data(meuse.grid)
gridded(meuse.grid) = ~x+y

kr.cv = autoKrige.cv(log(zinc)~1, meuse, model = c("Exp"), nfold = 10)
kr_dist.cv = autoKrige.cv(log(zinc)~sqrt(dist), meuse, 
       model = c("Exp"), nfold = 10)
kr_dist_ffreq.cv = autoKrige.cv(log(zinc)~sqrt(dist)+ffreq, 
       meuse, model = c("Exp"), nfold = 10)
} 
