plot.autoKrige = function(x, sp.layout = NULL, ...)
# This function plots the results from the autoKrige procedure
# This includes the kriging prediction, the kriging results,
# the experimental variogram and the fitted variogram model
{
    shift = 0.03
    labels = as.character(x$exp_var$np)
    vario = xyplot(gamma ~ dist, data = x$exp_var, panel = autokrige.vgm.panel,
                labels = labels, shift = shift, model = x$var_model,# subscripts = TRUE,
                direction = c(x$exp_var$dir.hor[1], x$exp_var$dir.ver[1]),
                ylim = c(min(0, 1.04 * min(x$exp_var$gamma)), 1.04 * max(x$exp_var$gamma)),
                xlim = c(0, 1.04 * max(x$exp_var$dist)), xlab = "Distance", ylab = "Semi-variance",
                main = "Experimental variogram and fitted variogram model", mode = "direct",...)
                
#                		xyplot(gamma ~ dist, data = x, panel = panel, xlim = xlim, 
#			ylim = ylim, xlab = xlab, ylab = ylab, labels = labels, model = model, 
#			direction = c(x$dir.hor[1], x$dir.ver[1]), shift = shift, 
#			mode = "direct", ...)

    pred = automapPlot(x$krige_output,
          zcol = "var1.pred",
          main = "Kriging prediction",
          sp.layout = sp.layout, ...)

    stdev = automapPlot(x$krige_output,
          zcol = "var1.stdev",
          main = "Kriging standard error",
          sp.layout = sp.layout, ...)

    print(pred, position = c(0,.5,.5,1),more=T)
    print(stdev, position = c(.5,.5,1,1),more = T)
    print(vario, position = c(0,0,1,.5)) 

}


