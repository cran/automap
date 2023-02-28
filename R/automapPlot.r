automapPlot = function(plot_data, zcol, col.regions, sp.layout, points, ...)
# A function to plot the results from autoKrige. Its purpose is to provide
# a custom colorscale useful for the kriging results. 
{
  
	if(missing(zcol)) zcol = names(plot_data)
	if(missing(col.regions)) col.regions = c("#EDF8B1","#C7E9B4","#7FCDBB","#41B6C4","#1D91C0","#225EA8","#0C2C84","#5A005A")
	dots = list(...)
	if (inherits(plot_data, "Spatial")) {
	  if (missing(sp.layout)) {
	    if (missing(points)) {
	      bb = bbox(plot_data)
	      points = SpatialPoints(data.frame(x = bb[1,2]*2, y = bb[2,2]*2))
	    }
	    sp.layout = list("sp.points", points)
	  }
	    
	  p = spplot(plot_data,
		zcol = zcol,
		col.regions=col.regions,
		cuts = length(col.regions) - 1,
		sp.layout = sp.layout, 
		...)					
	  return(p)
	} else if (inherits(plot_data, "sf")) {
    plot(plot_data[zcol], nbreaks = length(col.regions), breaks = "equal", pal = col.regions)
    if (!missing(points)) plot(st_geometry(points), add = TRUE, pch = 17)
  }

}