setwd("C:/Users/martisl/OneDrive - NIWA/NIWA Research Project/R_files")
rm(list = ls()) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
load("results\\chl.stack.list.yearly.Rda")

# library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(dplyr)
library(stringr)
library(spData)
library(spData)
library(tidyr)
# detach("package:tidyverse", unload = TRUE)

get_xy_vals <- function(data_list,x,y){ #x=c(start=coord,end=coord)
  if(length(x)!=1){
    x <- seq(from=range(x)[1],to=range(x)[2], by=500)}
  if(length(y)!=1){  
    y <- seq(from=range(y)[1],to=range(y)[2], by=500)}
  xy <- data.frame(expand.grid(x=x,y=y))
  year <- 2003:2019
  expanded <- xy[rep(row.names(xy), 17), ] #fails here
  mydf <- data.frame(expanded, year=sort(rep(year,nrow(xy))), chl=NA) #pre-set df
  location <- xy #DF
  coordinates(location) <- ~ x+ y #Convert to spatial points df format
  for(i in 1 :length(data_list)){ #1-17
    vals <- raster::extract(data_list[[i]], location) #Extract values
    mydf[((i-1)*nrow(xy)+1):(i*nrow(xy)),"chl"] <- vals}
  mydf <- mydf
}
dat <- get_xy_vals(data_list=ls_chl_yr, 
                    x=(1088228+(500*143)):(1088228+(500*(143+9))), 
                    y=(6260250-(500*207)):(6260250-(500*(207+9))))

#################OLD CODE#############
# getting x,y values - Inefficient code
get_xy_vals <- function(data_list,x,y){ #x=c(start=coord,end=coord)
  if(length(x)!=1){
    x <- seq(from=range(x)[1],to=range(x)[2], by=500)}
  if(length(y)!=1){  
    y <- seq(from=range(y)[1],to=range(y)[2], by=500)}
  xy <- as.data.frame(expand.grid(x=x,y=y))  
  points <- seq(1,nrow(xy))
  location <- data.frame(points=points,x=xy[1],y=xy[2]) #DF
  coordinates(location) <- ~ x+ y #Convert to spatial points df format
  vals <- list() #empty list
  cbind.vals <- matrix(0, nrow = nrow(xy), ncol = 0) #empty matrix
  for(i in 1 :length(data_list)){
    vals[[i]] <- raster::extract(data_list[[i]], location) #Extract values
    cbind.vals <- cbind(cbind.vals,vals[[i]])} #combine data into matrix
  valuesSP <- cbind(location,cbind.vals)
  valuesDF <- as.data.frame(valuesSP) #convert to dataframe
  # CONVERT df from wide format to long format
  data <- tidyr::gather(valuesDF, key=file, value=CHL, CHL200301:CHL201912)
  data$time <- substring(data$file,4,9)
  data$year <- substring(data$file,4,7)
  data$month <- substring(data$file,8,9)
  data$season <- if_else(data$month %in% c("09","10","11"), "Spring",
                 if_else(data$month %in% c("12","01","02"), "Summer",
                 if_else(data$month %in% c("03","04","05"), "Autumn", 
                 if_else(data$month %in% c("06","07","08"), "Winter", "NA"))))
  data$season <- as.factor(data$season)
  data <- data[, c("points", "file",  "time", "season", "year", "month","x", "y", "CHL")]
}

