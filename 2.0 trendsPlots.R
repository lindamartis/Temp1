setwd("C:/Users/martisl/OneDrive - NIWA/NIWA Research Project/R_files")
# rm(list = ls()) #will clear all objects includes hidden objects.
# gc() #free up memrory and report the memory usage.
# load("results\\chl.stack.list.Rda")

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

# plotting x,y values - Monthly
trends_monthly <- function(data, valname, year) {
  j = which(names(data) == valname) # index corresponding to valname
  if (length(j) == 0) stop("parameter not present in data frame")
    if(all(year %in% data$year)) { # check year exists in data frame
      newdata <- NULL
      for (i in 1: length(year)){cut <- data[data$year == year[i], ] #trim
        newdata <- rbind(newdata, cut)
      }
    } else {
      stop("Year is not present in the dataset")
    }
  data <- newdata
  h = ggplot(data[!is.na(data[, j]), ], 
             aes_string(x = "time", y = names(data)[j])) +
    theme(axis.text.x = element_text(angle = 90)) +
    geom_point() +
    geom_smooth(formula = y ~ x, method = "loess", span = 0.5, se = FALSE, aes(group = "" )) +
    labs(x = "Months", y = names(data)[j]) +
    scale_y_continuous(limits = range(data[!is.na(data[, j]), j])) +
    labs(title = paste0(names(data)[j], ' over time in months'))
  h
}

# plotting x,y values - Yearly
trends_yearly<- function(data, valname, year) {
  j <- which(names(data) == valname) # index corresponding to valname
  if (length(j) == 0) stop("parameter not present in data frame")
  if(all(year %in% data$year)) { # check year exists in data frame
    newdata <- NULL
    for (i in 1: length(year)){cut <- data[data$year == year[i], ] #trim
    newdata <- rbind(newdata, cut)
    }
  } else {
    stop("Year not present in the dataset")
  }
  data <- newdata %>% group_by(year) %>% summarize(val = mean(CHL, na.rm = TRUE))
  h = ggplot(data[!is.na(data[, "val"]), ], 
             aes_string(x = "year", y = "val")) +
    theme(axis.text.x = element_text(angle = 90)) +
    geom_point() +
    geom_smooth(formula = y ~ x, method = "loess", span = 0.5, se = FALSE, aes(group = "" )) +
    labs(x = "Years", y = paste0("mean ",valname)) +
    scale_y_continuous(limits = range(data[ , "val"])) +
    labs(title = paste0(valname, ' over time in years'))
  h
}

# plotting x,y values - Seasonal
trends_season<- function(data, valname, year) {
  j <- which(names(data) == valname) # index corresponding to valname
  if (length(j) == 0) stop("parameter not present in data frame")
  if(all(year %in% data$year)) { # check year exists in data frame
    newdata <- NULL
    for (i in 1: length(year)){cut <- data[data$year == year[i], ] #trim
    newdata <- rbind(newdata, cut)
    }
  } else {
    stop("Year not present in the dataset")
  }
  data <- newdata %>% group_by(year, season) %>% summarize(val = mean(CHL, na.rm = TRUE))
  h = ggplot(data[!is.na(data[, "val"]), ], 
             aes_string(x = "year", y = "val", colour="season")) +
            geom_point() +
    theme(axis.text.x = element_text(angle = 90)) +
    geom_smooth(formula = y ~ x, method = "loess", span = 0.6, se = FALSE, aes(group = "" )) +
    labs(x = "Years", y = paste0("mean ",valname), color = 'Season') +
    scale_y_continuous(limits = range(data[ , "val"])) +
    labs(title = paste0(valname, ' over time in years')) +
    facet_wrap(~season)
  h
}


dat <- get_xy_vals(data=ls_chl, x=c(1082730:1083730), y=c(6260250))
trends_monthly(dat,valname="CHL", year=c(2003,2004))
trends_yearly(dat,valname="CHL", year=2003:2019)
trends_season(dat,valname="CHL", year=c(2003:2019))
