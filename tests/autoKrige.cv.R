library(automap)
library(sp)
library(sf)

data(meuse)
coordinates(meuse) = ~x+y
data(meuse.grid)
gridded(meuse.grid) = ~x+y

kr.cv = autoKrige.cv(log(zinc)~1, meuse, model = c("Exp"))
kr_dist.cv = autoKrige.cv(log(zinc)~sqrt(dist), meuse, 
       model = c("Exp"))
kr_dist_ffreq.cv = autoKrige.cv(log(zinc)~sqrt(dist)+ffreq, 
       meuse, model = c("Exp"))

summary(kr.cv)
summary(kr_dist.cv)
summary(kr_dist_ffreq.cv)

compare.cv(kr.cv, kr_dist.cv, kr_dist_ffreq.cv)


meuse = as(meuse, "sf")
meuse.grid = as(meuse.grid, "sf")
kr.cv.sf = autoKrige.cv(log(zinc)~1, meuse, model = c("Exp"))
kr_dist.cv.sf = autoKrige.cv(log(zinc)~sqrt(dist), meuse, 
                          model = c("Exp"))
kr_dist_ffreq.cv.sf = autoKrige.cv(log(zinc)~sqrt(dist)+ffreq, 
                                meuse, model = c("Exp"))

summary(kr.cv.sf)
summary(kr_dist.cv.sf)
summary(kr_dist_ffreq.cv.sf)

compare.cv(kr.cv, kr_dist.cv, kr_dist_ffreq.cv, kr.cv.sf, kr_dist.cv.sf, kr_dist_ffreq.cv.sf)
