autoKrige.cv = function(formula, input_data, data_variogram = input_data,
                          model = c("Sph", "Exp", "Gau", "Mat"), kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10), 
						  fix.values = c(NA,NA,NA), verbose = FALSE, GLS.model = NA,...)
# Automatically fits a variogram model to the data using autofitVariogram and performs
# crossvalidation by calling krige.cv
{
	kr.cv = krige.cv(formula, input_data, model = autofitVariogram(formula, 
														data_variogram, 
														model = model, 
														kappa = kappa,
														fix.values = fix.values,
														verbose = verbose,
														GLS.model = GLS.model)$var_model,
														...)
	kr.cv = list(krige.cv_output = kr.cv)
	class(kr.cv) = "autoKrige.cv"
	return(kr.cv)
}

summary.autoKrige.cv = function(object, ...) {
# The summary function for the autoKrige.cv object. Returns some
# statistics about the cross-validation
	obj = object$krige.cv_output
	out = list()
	# mean error, ideally 0:
	out$mean_error = mean(obj$residual)
	# mean absolute error, ideally 0, less vulnerable to outliers
	out$MAE = mean(abs(obj$residual))
	# MSE, ideally small
	out$MSE = mean(obj$residual^2)
	# Mean square normalized error, ideally close to 1
    out$MSNE = mean(obj$zscore^2)
	# correlation observed and predicted, ideally 1
    out$cor_obspred = cor(obj$observed, obj$observed - obj$residual)
	# correlation predicted and residual, ideally 0
    out$cor_predres = cor(obj$observed - obj$residual, obj$residual)
	# RMSE, ideally small
	out$RMSE = sqrt(sum(obj$residual^2) / length(obj$residual))
	# RMSE, ideally zero
	out$URMSE = sqrt((sum(obj$residual^2) / length(obj$residual)) - mean(obj$residual)^2)
	# Inter quartile range, ideally small
	out$iqr = IQR(obj$residual)

	return(t(data.frame(out)))
}

compare.cv = function(..., col.names, bubbleplots = FALSE, zcol = "residual", 
						   layout, key.entries, reference = 1, plot.diff = FALSE) 
# A function to compare cross-validations to each other in both statistics (using summary.autoKrige.cv) or
# in bubble plots (using cv.compare.bubble).
{
	dots = list(...)
	out = sapply(dots, summary)
	if(missing(col.names)) {
		col.names = LETTERS[1:length(dots)]
	}
	dimnames(out)[[2]] = col.names

	if(bubbleplots) {
		if(missing(layout)) {
			layout1 = unlist(sapply(1:10, function(x) return(rep(x, 2*x - 1))))
			layout2 = unlist(sapply(1:10, function(x) return(rep(x, 2*x))))
			layout = c(layout1[length(dots)], layout2[length(dots)])
		}
		if(missing(key.entries)) key.entries = quantile(dots[[reference]]$krige.cv_output[[zcol]])
		cv.compare.bubble(dots, zcol = zcol, col.names = col.names, 
						  layout = layout, key.entries = key.entries, 
						  reference = reference, plot.diff = plot.diff)
	}	

	return(data.frame(out))
}

cv.compare.bubble = function(objs, zcol, key.entries, layout, col.names, plot.diff, reference,...) 
# A function to create bubble plots of multiple cross validation in order to compare them.
{
  ref_data = objs[[reference]]$krige.cv_output[[zcol]]
  plot.column = zcol
  plot_list = list()
  it = 1
  for(obj in objs) {
	main = col.names[it]
    if(plot.diff) {
		obj$krige.cv_output$diff = ref_data^2 - obj$krige.cv_output[[zcol]]^2
		plot.column = "diff"
		if(it == reference) main = "Reference" else main = as.expression(parse(text = paste("Ref^2 -", col.names[it],"^2", sep = "")))
	}
	plot_list[[it]] = bubble(obj$krige.cv_output, 
    					zcol = plot.column, 
						key.entries = key.entries,
						main = main,
						sub = zcol,
						...)
    it = it + 1     	  
    }

  pos_list = cbind(rep(0:(layout[1]-1) / layout[1], layout[2]),
		   rep((layout[2]-1):0/layout[2], each = layout[1]),
		   rep(1:layout[1] / layout[1], layout[2]),
		   rep(layout[2]:1/layout[2],each = layout[1]))
  more = TRUE
  it = 1
  while(more) {
    if(it == length(plot_list)) more = FALSE
    print(plot_list[[it]], position = pos_list[it,], more = more)
    it = it + 1   
  }
}
