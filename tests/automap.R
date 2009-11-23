library(automap)

# Data preparation
data(meuse)
coordinates(meuse) =~ x+y
data(meuse.grid)
gridded(meuse.grid) =~ x+y

# Ordinary kriging
kriging_result = autoKrige(zinc~1, meuse, meuse.grid)
summary(kriging_result)

kriging_result = autoKrige(zinc~1, meuse, meuse.grid, fix.values = c(0.2,NA,NA))
summary(kriging_result)

# Universal kriging
kriging_result = autoKrige(zinc~soil+ffreq+dist, meuse, meuse.grid)
summary(kriging_result)

# Block kriging
kriging_result = autoKrige(zinc~soil+ffreq+dist, meuse, meuse.grid, block = c(400,400))
summary(kriging_result)

# Kriging with power model
kriging_result = autoKrige(zinc~soil+ffreq+dist, meuse, meuse.grid, model = "Pow")
