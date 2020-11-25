library(raster)

# Create data
set.seed(123)
r <- raster(ncols=10, nrows=10)
r[] <- sample(10:360, ncell(r),replace = T)
crs(r) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
projection(r)
plot(r, maxpixels=100)

install.packages("animation")
library(animation)
ani.options(interval=1)

saveGIF({
  for (i in 1:365){
    m <- c(1, i, 0,  i, 365, 1)
    rclmat <- matrix(m, ncol=3, byrow=TRUE)
    rc <- reclassify(r, rclmat)
    
    plot(rc, col=c("green3", "white"), legend=FALSE, main = paste("Day", i))
  }
}) 
