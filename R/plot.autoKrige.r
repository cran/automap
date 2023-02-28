plot.autoKrige = function(x, sp.layout = NULL, ...)
# This function plots the results from the autoKrige procedure
# This includes the kriging prediction, the kriging results,
# the experimental variogram and the fitted variogram model
{
  askpar = par("ask")
    xx = list(exp_var = x$exp_var, var_model = x$var_model)
    class(xx) = "autofitVariogram"
    vario = plot(xx, plotit = FALSE)
                
#                		xyplot(gamma ~ dist, data = x, panel = panel, xlim = xlim, 
#			ylim = ylim, xlab = xlab, ylab = ylab, labels = labels, model = model, 
#			direction = c(x$dir.hor[1], x$dir.ver[1]), shift = shift, 
#			mode = "direct", ...)
    kout = x$krige_output
    if (inherits(kout, "sf")) {
      kout = as(kout, "Spatial")
      gridded(kout) = TRUE
    }
    pred = automapPlot(kout,
          zcol = "var1.pred",
          main = "Kriging prediction",
          sp.layout = sp.layout, ...)

    stdev = automapPlot(kout,
          zcol = "var1.stdev",
          main = "Kriging standard error",
          sp.layout = sp.layout, ...)

      
      print(pred, position = c(0,.5,.5,1),more=T)
      print(stdev, position = c(.5,.5,1,1),more = T)
      print(vario, position = c(0,0,1,.5)) 
}


