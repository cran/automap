autofitVariogram = function(formula, input_data, model = c("Sph", "Exp", "Gau", "Ste"),
                                kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10), fix.values = c(NA,NA,NA),
								verbose = FALSE, GLS.model = NA, start_vals = c(NA,NA,NA))
# This function automatically fits a variogram to input_data
{
    # The boundaries could also be fitted automatically. This could be done by fitting
    # a function between lag-distance and bin-width. The fitting criterium could be
    # how good it looks but it is hard to put into formal language. A more simple approach
    # is chosen here. A standard boundary set is used and is scaled for the size of the area.
    # This could give problems if the extent of the area is much smaller or larger then the corellation
    # length of the phenomenon that is studied.
    x = coordinates(input_data)[,1]
    y = coordinates(input_data)[,2]
    scale_number = (0.35*sqrt((max(x) - min(x))^2 + (max(y) - min(y))^2) / 100 )   # 0.35 times the length of the central axis through the area
    boundaries = c(2,4,6,9,12,15,25,35,50,65,80,100) * scale_number                # Boundaries for the bins in km

	# If you specifiy a variogram model in GLS.model the Generelised least squares sample variogram is constructed
	if(!is(GLS.model, "variogramModel")) {
		experimental_variogram = variogram(formula, input_data,boundaries = boundaries)
	} else {
		if(verbose) cat("Calculating GLS sample variogram\n")
		g = gstat(NULL, "bla", formula, input_data, model = GLS.model, set = list(gls=1))
		experimental_variogram = variogram(g, boundaries = boundaries)
	}

	# If there are bins with less than 5 point pairs we merge the first two bins and 
	# rebuild the variogram and check again etc etc. This stops if no bins have less than
	# 5 point pairs
	while(TRUE) {
		if(length(experimental_variogram$np[experimental_variogram$np <5]) == 0 | length(boundaries) == 1) break
		boundaries = boundaries[2:length(boundaries)]			
		if(!is(GLS.model, "variogramModel")) {
			experimental_variogram = variogram(formula, input_data,boundaries = boundaries)
		} else {
			experimental_variogram = variogram(g, boundaries = boundaries)
		}
	}	
	#experimental_variogram = experimental_variogram[experimental_variogram$np >5,] # FILTER!!!!! Clip points that have less then 5 point pairs

    # If the value in start_vals == NA:
    # Automatically choosing the initial guess for fit.variogram
    # initial_sill = mean(max(semi_var) + median(semi-var))
    # initial_range = 0.10 * central axis of the area.
    # initial_nugget = minimum semi-variance value
    if(is.na(start_vals[1])) {  # Nugget
        initial_nugget = min(experimental_variogram$gamma)
    } else {
        initial_nugget = start_vals[1]
    }
    if(is.na(start_vals[2])) { # Range
        initial_range = (0.1*sqrt((max(x) - min(x))^2 + (max(y) - min(y))^2) )   # 0.10 times the length of the central axis through the area
    } else {
        initial_range = start_vals[2]
    }
    if(is.na(start_vals[3])) { # Sill
        initial_sill = mean(c(max(experimental_variogram$gamma), median(experimental_variogram$gamma)))
    } else {
        initial_sill = start_vals[3]
    }
    
    # Determine what should be automatically fitted and what should be fixed
    # Nugget
    if(!is.na(fix.values[1]))
    {
        fit_nugget = FALSE
        initial_nugget = fix.values[1]
    } else
        fit_nugget = TRUE

    # Range
    if(!is.na(fix.values[2]))
    {
        fit_range = FALSE
        initial_range = fix.values[2]
    } else
        fit_range = TRUE

    # Partial sill
    if(!is.na(fix.values[3]))
    {
        fit_sill = FALSE
        initial_sill = fix.values[3]
    } else
        fit_sill = TRUE

    getModel = function(psill, model, range, kappa, nugget, fit_range, fit_sill, fit_nugget, verbose)
    {
		if(verbose) debug.level = 1 else debug.level = 0
        if(model == "Pow") {
            warning("Using the power model is at your own risk, read the docs of autofitVariogram for more details.")
            if(is.na(start_vals[1])) nugget = 0
            if(is.na(start_vals[2])) range = 1    # If a power mode, range == 1 is a better start value
            if(is.na(start_vals[3])) sill = 1
        }
        obj = try(fit.variogram(experimental_variogram,
                        model = vgm(psill=psill, model=model, range=range,
                                    nugget=nugget,kappa = kappa),
                        fit.ranges = c(fit_range), fit.sills = c(fit_nugget, fit_sill),
						debug.level = 0), 
				TRUE)
		if("try-error" %in% class(obj)) {
			#print(traceback())
			warning("An error has occured during variogram fitting. Used:\n", 
					"\tnugget:\t", nugget, 
					"\n\tmodel:\t", model, 
					"\n\tpsill:\t", psill,
					"\n\trange:\t", range,
					"\n\tkappa:\t",ifelse(kappa == 0, NA, kappa),
					"\n  as initial guess. This particular variogram fit is not taken into account. \nGstat error:\n", obj)
			return(NULL)
		} else return(obj)
    }

    # Automatically testing different models, the one with the smallest sums-of-squares is chosen
	test_models = model
	SSerr_list = c()
	vgm_list = list()
	counter = 1
	
	for(m in test_models) {
		if(m != "Mat" && m != "Ste") {        # If not Matern and not Stein
			model_fit = getModel(initial_sill - initial_nugget, m, initial_range, kappa = 0, initial_nugget, fit_range, fit_sill, fit_nugget, verbose = verbose)
			if(!is.null(model_fit)) {	# skip models that failed
				vgm_list[[counter]] = model_fit
				SSerr_list = c(SSerr_list, attr(model_fit, "SSErr"))}
			counter = counter + 1
		} else {                 # Else loop also over kappa values
			for(k in kappa) {
				model_fit = getModel(initial_sill - initial_nugget, m, initial_range, k, initial_nugget, fit_range, fit_sill, fit_nugget, verbose = verbose)
				if(!is.null(model_fit)) {
					vgm_list[[counter]] = model_fit
					SSerr_list = c(SSerr_list, attr(model_fit, "SSErr"))}
				counter = counter + 1
			}
		}
	}

	if(verbose) {
		cat("Selected:\n")
		print(vgm_list[[which.min(SSerr_list)]])
		cat("\nTested models, best first:\n")
		tested = data.frame("Tested models" = sapply(vgm_list, function(x) as.character(x[2,1])), 
								kappa = sapply(vgm_list, function(x) as.character(x[2,4])), 
								"SSerror" = SSerr_list)
		tested = tested[order(tested$SSerror),]
		print(tested)
	}

	result = list(exp_var = experimental_variogram, var_model = vgm_list[[which.min(SSerr_list)]], sserr = min(SSerr_list))
	class(result) = c("autofitVariogram","list")    

    return(result)
}
