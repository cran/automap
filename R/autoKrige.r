autoKrige = function(formula, input_data, new_data, data_variogram = input_data, block = 0,
                          model = c("Sph", "Exp", "Gau", "Ste"), kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10), 
						  fix.values = c(NA,NA,NA), remove_duplicates = TRUE, verbose = FALSE, GLS.model = NA,...)
# This function performs an automatic Kriging on the data in input_data
{
	if(inherits(formula, "SpatialPointsDataFrame"))
	# Is someone just passes a spatialpointsdataframe, assume he/she wants to interpolate the first column with Ordinary Kriging
	{
		input_data = formula
		formula = as.formula(paste(names(input_data)[1], "~ 1"))
	}

    # Check if inpu_data and data_variogram are SpatialPointsDataFrame
    if(!inherits(input_data,"SpatialPointsDataFrame") | !inherits(data_variogram,"SpatialPointsDataFrame"))
    {
        stop(paste("\nInvalid input objects: input_data or data_variogram not of class 'SpatialPointsDataFrame'.\n\tClass input_data: '",
                      class(input_data),"'",
                      "\n\tClass data_variogram: '",
                      class(data_variogram),"'",sep=''))
    }
	if(as.character(formula)[3] != 1 & missing(new_data)) stop("If you want to use Universal Kriging, new_data needs to be specified \n  because the predictors are also required on the prediction locations.") 

    # Check if there are points or gridcells on the exact same coordinate and provide a more informative error message.
    # Points on the same spot causes the interpolation to crash.
    if(remove_duplicates) 
    {
        zd = zerodist(input_data)
        if(length(zd) != 0) 
        {
            warning("Removed ", length(zd) / 2, " duplicate observation(s) in input_data:", immediate. = TRUE)
			print(input_data[c(zd), ])
          	input_data = input_data[-zd[, 2], ]   

        }
    }
    
    # Fit the variogram model, first check which model is used
    variogram_object = autofitVariogram(formula,
                      data_variogram,
                      #autoselect.model = autoselect.model,
                      model = model,
                      kappa = kappa,
                      fix.values = fix.values,
					  verbose = verbose,
					  GLS.model = GLS.model)

	if(missing(new_data)) new_data = create_new_data(input_data)

    ## Perform the interpolation
    krige_result = krige(formula,
                      input_data,
                      new_data,
                      variogram_object$var_model,
                      block = block,
					  ...)

    krige_result$var1.stdev = sqrt(krige_result$var1.var)

    # Aggregate the results into an autoKrige object
    result = list(krige_output = krige_result,exp_var = variogram_object$exp_var, var_model = variogram_object$var_model)
    class(result) = c("autoKrige","list")

    return(result)

}