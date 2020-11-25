library(ncdf4) # package for netcdf manipulation
library(sf)
library(tidyverse)
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(dplyr)
library(stringr)
library(spData)
library(oce)

setwd("C:/Users/martisl/OneDrive - NIWA/NIWA Research Project/R_files")

# READING ONE FILE IN

nc_data <- nc_open('CHL/MO/A20021822002212_MO_CHL_coastal_2018.0.nc')
print(nc_data)#metadata

# naming the variables
x<- ncvar_get(nc_data, "x")
y<- ncvar_get(nc_data, "y")
chl <- ncvar_get(nc_data, "CHL") # store the data in a 2-dimensional x,y array

fillvalue <- ncatt_get(nc_data, "CHL", "missing_value");fillvalue

nc_close(nc_data) 

chl[chl == fillvalue$value] <- NA #replace -9999 with NA

# SAVE NETCDF TO RASTER
r <- raster(t(chl), 
            xmn=min(x), xmx=max(x), 
            ymn=min(y), ymx=max(y), 
            crs=CRS("+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
                      #CRS string obtained from https://spatialreference.org/ref/?search=new+zealand)

r <- flip(r, direction='y') # flipping y

#plot(r)

# CODE TO CONVERT RASTER TO DATAFRAME
chl.df <- raster::as.data.frame(r, xy = TRUE)
names(chl.df)[3] <- "CHL"

# # removing missing values
# rm <- which(!is.na(chl.df[,3]))
# chl.df <- chl.df[rm,]
# nrow(chl.df)
# #number complete values : 2214399
# 
# # plotting to see outliers
# windows()
# plot(sort(chl.df$CHL))

# PLOTTING ALL POINTS
library(scales)
ggplot() +
  geom_raster(data = chl.df, aes(x = x, y = y, fill = CHL)) +
  #scale_y_reverse() +
  coord_equal() +
  labs(title = 'Chlorophyll level', fill = 'CHL') +
  scale_fill_gradient(low = "yellow", high = "red", 
                       limits = c(min(chl.df$CHL), 2), oob = squish)


##########################

# CODE TO WRITE RASTER AS NETCDF

# writeRaster(x = r,
#   filename = paste0("C:\\Users\\Inda\\Google Drive\\Victoria University 2\\NIWA Research Project\\R_files\\results\\CHL.nc"),
#   overwrite = TRUE,
#   format = 'CDF')

