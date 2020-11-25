
setwd("C:/Users/martisl/OneDrive - NIWA/NIWA Research Project/R_files")

rm(list=ls())

library(ncdf4) # package for netcdf manipulation
library(tidyverse)
library(raster) # package for raster manipulation
# library(rgdal) # package for geospatial analysis
# library(ggplot2) # package for plotting
# library(dplyr)
# library(stringr)
# library(spData)

# EXTRACT MONTHLY FILES
fls<-list()
for(i in 2003:2019){
f <- list.files(path = 'data\\CHL\\MO', pattern=paste0('A',i), full.names=TRUE)
fls[[i-2002]]<-f
}


MTH <- str_pad(1:12, 2, pad = "0")
ls_chl <- list()
for(i in 1:length(fls)){ #1 to 17
  YR <- 2002+i
  stack_chl <- stack()
for(j in 1:length(fls[[i]])){ #1 to 12
  nc_data <- nc_open(fls[[i]][j])
  lon <- ncvar_get(nc_data, "x")
  lat <- ncvar_get(nc_data, "y")
  chl_layer <- ncvar_get(nc_data, "CHL") # store the data in a 2-dimensional array
  fillvalue <- ncatt_get(nc_data, "CHL", "missing_value")
  nc_close(nc_data)
  chl_layer[chl_layer == fillvalue$value] <- NA # turn -9999s into NAs
  chl_layer <- raster(t(chl_layer), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat), crs = CRS("+init=EPSG:2193")) 
  stack_chl <- stack(stack_chl, chl_layer)
}
  for (k in 1:12){names(stack_chl)[k] <- paste0("CHL",YR,MTH[k])}
  ls_chl[[i]] <- stack_chl
  }
ls_chl

# save(ls_chl,file="results\\chl.stack.list.Rda") #SAVE OBJECT TO RDA

# EXTRACT YEARLY NETCDF FILES
fls<-list()
for(i in 2003:2019){
  f <- list.files(path = 'data/CHL/yr', pattern=paste0('A',i), full.names=TRUE)
  fls[[i-2002]]<-f
}
ls_chl_yr <- list()
for(i in 1:length(fls)){ #1 to 17 
    YR <- 2002+i
    stack_chl <- stack()
    nc_data <- nc_open(fls[[i]])
    lon <- ncvar_get(nc_data, "x")
    lat <- ncvar_get(nc_data, "y")
    chl_layer <- ncvar_get(nc_data, "CHL") # store the data in a 2-dimensional array
    fillvalue <- ncatt_get(nc_data, "CHL", "missing_value")
    nc_close(nc_data)
    chl_layer[chl_layer == fillvalue$value] <- NA # turn -9999s into NAs
    chl_layer <- raster(t(chl_layer), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat), crs = CRS("+init=EPSG:2193")) 
    stack_chl <- stack(stack_chl, chl_layer)
    names(stack_chl) <- paste0("CHL",YR)
    ls_chl_yr[[i]] <- stack_chl}
save(ls_chl_yr,file="results/chl.stack.list.yearly.Rda") #SAVE OBJECT TO RDA

ls_chl_yr


###############OLD CODE########################

# # CONVERTING netCDF to RASTER BY YEAR 2003-2019
# # ls <- list()
# # s <- stack()
# # MTH <- str_pad(1:12, 2, pad = "0")
# # l<-1
# for(i in 1:length(f)){
#   r <- raster(f[[i]], varname = "CHL")
#   names(r) <- paste0("CHL",i)
#   s <- stack(s, r)
#   # ls[[l]] <- s
#   # l <- l+1
# }




# # CONVERTING netCDF to RASTER BY YEAR 2003-2019
# ls <- list()
# MTH <- str_pad(1:12, 2, pad = "0")
# l<-1
# for(YR in 2003:2019){
#   fday<-c("001","032","060","091","121","152","182","213","244","274","305","335")
#   lday<-c("031","059","090","120","151","181","212","243","273","304","334","365")
#   fday.leap<-c("001","032","061","092","122","153","183","214","245","275","306","336")
#   lday.leap<-c("031","060","091","121","152","182","213","244","274","305","335","366")
#   if(YR%%4==0){fday<-fday.leap;lday<-lday.leap}
#   s <- stack(
#     raster(paste0("CHL/MO/A",YR,fday[1],YR,lday[1],"_MO_CHL_coastal_2018.0.nc"), varname = "CHL"),
#     raster(paste0("CHL/MO/A",YR,fday[2],YR,lday[2],"_MO_CHL_coastal_2018.0.nc"), varname = "CHL"),
#     raster(paste0("CHL/MO/A",YR,fday[3],YR,lday[3],"_MO_CHL_coastal_2018.0.nc"), varname = "CHL"),
#     raster(paste0("CHL/MO/A",YR,fday[4],YR,lday[4],"_MO_CHL_coastal_2018.0.nc"), varname = "CHL"),
#     raster(paste0("CHL/MO/A",YR,fday[5],YR,lday[5],"_MO_CHL_coastal_2018.0.nc"), varname = "CHL"),
#     raster(paste0("CHL/MO/A",YR,fday[6],YR,lday[6],"_MO_CHL_coastal_2018.0.nc"), varname = "CHL"),
#     raster(paste0("CHL/MO/A",YR,fday[7],YR,lday[7],"_MO_CHL_coastal_2018.0.nc"), varname = "CHL"),
#     raster(paste0("CHL/MO/A",YR,fday[8],YR,lday[8],"_MO_CHL_coastal_2018.0.nc"), varname = "CHL"),
#     raster(paste0("CHL/MO/A",YR,fday[9],YR,lday[9],"_MO_CHL_coastal_2018.0.nc"), varname = "CHL"),
#     raster(paste0("CHL/MO/A",YR,fday[10],YR,lday[10],"_MO_CHL_coastal_2018.0.nc"), varname = "CHL"),
#     raster(paste0("CHL/MO/A",YR,fday[11],YR,lday[11],"_MO_CHL_coastal_2018.0.nc"), varname = "CHL"),
#     raster(paste0("CHL/MO/A",YR,fday[12],YR,lday[12],"_MO_CHL_coastal_2018.0.nc"), varname = "CHL"))
#   for (i in 1:12){names(s)[i] <- paste0("CHL",YR,MTH[i])}
#   # df <- raster::as.data.frame(s, xy = TRUE) # code to convert to DF
#   # ls[[l]] <- brick(s) #brick seems to be leading to source problem (don't know why)
#   ls[[l]] <- s
#   l <- l+1
# }
# # save(ls,file="results/chl.stack.list.Rda") #SAVE OBJECT TO RDA

####################################################################

# # CODE TO MERGE/JOIN MULTIPLE DATAFRAMES IN A LIST

# x <- data.frame(i = c("a","b","c"), ii = c("aa","bb","cc"), j = 1:3, stringsAsFactors=FALSE)
# y <- data.frame(i = c("a","b","c"), ii = c("aa","bb","cc"), k = 4:6, stringsAsFactors=FALSE)
# z <- data.frame(i = c("a","b","c"), ii = c("aa","bb","cc"), l = 7:9, stringsAsFactors=FALSE)
# xyz <- list(x, y, z)
# xyz <- xyz %>% reduce(left_join, by = c("i","ii"))
# xyz
# chl.df <- ls %>% reduce(left_join, by = c("x","y"))

