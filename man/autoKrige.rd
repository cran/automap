\name{autoKrige}
\alias{autoKrige}
\title{Performs an automatic interpolation}
\description{This function performs automatic kriging on the given dataset.
                The variogram is generated automatically using \link{autofitVariogram}.}
\usage{autoKrige(formula, 
  	  input_data, 
	  new_data, 
 	  data_variogram = input_data, 
	  block = 0, 
	  model = c("Sph", "Exp", "Gau", "Ste"), 
	  kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10), 
	  fix.values = c(NA,NA,NA), 
	  remove_duplicates = TRUE, 
	  verbose = FALSE, 
	  GLS.model = NA,
      start_vals = c(NA,NA,NA),
      miscFitOptions = list(),
	  ...)}
\arguments{
	\item{formula}{formula that defines the dependent variable as a linear model
				of independent variables; suppose the dependent variable has
				name 'z', for ordinary and simple kriging use the formula
				'z~1'; for simple kriging also define 'beta' (see below); for
				universal kriging, suppose 'z' is linearly dependent on 'x'
				and 'y', use the formula 'z~x+y'.}
    \item{input_data}{An object of the 
	            \link[sp]{SpatialPointsDataFrame-class} containing the data to be interpolated.}
    \item{new_data}{A \code{sp} object containing the prediction locations. \code{new_data} can be
				a points set, a grid or a polygon. Must not contain NA's. If this object is not provided
				a default is calculated. This is done by taking the convex hull of \code{input_data} and 
				placing around 5000 gridcells in that convex hull.}
    \item{data_variogram}{An optional way to provide a different dataset for
                the building of the variogram then for the spatial
                interpolation. }
    \item{block}{Use this parameter to pass on a specification for the 
                block size. e.g. c(1000,1000) }
	\item{model}{List of models that will be tested during automatic variogram fitting.}
     \item{kappa}{List of values for the smoothing parameter of the Matern model that will be tested during automatic variogram fitting.}
    \item{fix.values}{Can be used to fix a variogram parameter to a certain value. It 
                 consists of a list with a length of three. The items describe the
                 fixed value for the nugget, range and sill respectively. Setting
                 the value to NA means that the value is not fixed. Is passed on to autofitVariogram.}
    \item{remove_duplicates}{logical, remove duplicate points from the \code{input_data}. This can take
                   some time on large datasets. }
	\item{verbose}{logical, if TRUE autoKrige will give extra information on the fitting process}
	\item{GLS.model}{If a variogram model is passed on through this parameter a Generalized Least Squares 
				 sample variogram is calculated.} 
    \item{start_vals}{Can be used to give the starting values for the variogram fitting. The items describe the
                 fixed value for the nugget, range and sill respectively. They need to be given in that order.
                 Setting the value to NA means that the value will be automatically chosen.} 
    \item{miscFitOptions}{Additional options to set the behavior of \link{autofitVariogram}. For details see the 
                 documentation of \link{autofitVariogram}.}
	\item{...}{arguments that are passed on to the gstat function \code{\link[gstat]{krige}}.}
}
\details{
\code{autoKrige} calls the function \code{autofitVariogram} that fits a variogram model to the
given dataset. This variogram model and the data are used to make predictions on the locations
in \code{new_data}. The only compulsory argument is \code{input_data}. So the most
simple call would of the form:

\code{autoKrige(meuse)}

\code{autoKrige} now assumes that you want to perform ordinary kriging on the first column of 
\code{input_data}.

\code{autoKrige} performs some checks on the coordinate systems of \code{input_data} and \code{new_data}.
If one of both is \code{NA}, it is assigned the projection of the other. If they have different projections,
an error is raised. If one of both has a non-projected system (i.e. latitude-longitude), an error is raised.
This error is raised because 'gstat does use spherical distances when data are in geographical
coordinates, however the usual variogram models are typically not
non-negative definite on the sphere, and no appropriate models are
available' (Edzer Pebesma on r-sig-geo).

When the user specifies the power model (\code{Pow}) as the model, the initial range is set to one. Note that
when using the power model, the initial range is the initial power.
}
\value{This function returns an \code{autoKrige} object containing the results of the interpolation 
(prediction, variance and standard deviation), the sample variogram, the variogram model that
was fitted by \code{autofitVariogram} and the sums of squares between the sample variogram and the
fitted variogram model. The attribute names are \code{krige_output}, \code{exp_var}, \code{var_model} 
and \code{sserr} respectively.
} 
\author{Paul Hiemstra, \email{p.hiemstra@geo.uu.nl}}
\seealso{\code{\link{autofitVariogram}}, \code{\link[gstat]{krige}}}
\examples{
# Data preparation
data(meuse)
coordinates(meuse) =~ x+y
data(meuse.grid)
gridded(meuse.grid) =~ x+y

# Ordinary kriging, no new_data object
kriging_result = autoKrige(zinc~1, meuse)
plot(kriging_result)

# Ordinary kriging
kriging_result = autoKrige(zinc~1, meuse, meuse.grid)
plot(kriging_result)

# Fixing the nugget to 0.2
kriging_result = autoKrige(zinc~1, meuse, 
	meuse.grid, fix.values = c(0.2,NA,NA))
plot(kriging_result)

# Universal kriging
kriging_result = autoKrige(zinc~soil+ffreq+dist, meuse, meuse.grid)
plot(kriging_result)

# Block kriging
kriging_result_block = autoKrige(zinc~soil+ffreq+dist, 
	meuse, meuse.grid, block = c(400,400))
plot(kriging_result_block)

# Dealing with duplicate observations
data(meuse)
meuse.dup = rbind(meuse, meuse[1,]) # Create duplicate
coordinates(meuse.dup) = ~x+y
kr = autoKrige(zinc~dist, meuse.dup, meuse.grid)

# Extracting parts from the autoKrige object
prediction_spdf = kr$krige_output
sample_variogram = kr$exp_var
variogram_model = kr$var_model
}
