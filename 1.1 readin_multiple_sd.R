setwd("C:/Users/martisl/OneDrive - NIWA/NIWA Research Project/R_files")
rm(list = ls())

#library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(raster) # package for raster manipulation
library(ggplot2) # package for raster manipulation
library(scales) # package for raster manipulation

f <- list.files(path = 'data/CHL/YR', pattern='CHL', full.names=TRUE)

all_chl <- stack()
for(year in 1:length(f)){
  nc_data <- nc_open(f[[year]])
  lon <- ncvar_get(nc_data, "x")
  lat <- ncvar_get(nc_data, "y")
  chl_layer <- ncvar_get(nc_data, "CHL") # store the data in a 2-dimensional array
  fillvalue <- ncatt_get(nc_data, "CHL", "missing_value")
  nc_close(nc_data) 
  chl_layer[chl_layer == fillvalue$value] <- NA # turn -9999s into NAs
  chl_layer <- raster(t(chl_layer), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat), crs = CRS("+init=EPSG:2193")) 
  all_chl <- stack(all_chl, chl_layer)
}

# Check rasters were read in correctly

r2 = raster::as.data.frame(all_chl[[16]], xy = T) # pick first year
names(r2)[3] = 'CHL'
head(r2)
ggplot() +
  geom_raster(data = r2, aes(x = x, y = y, fill = CHL)) +
  scale_y_reverse() +
  coord_equal() +
  labs(title = 'Chlorophyll', fill = 'Chl. A') +
  scale_fill_gradient(low = "yellow", high = "red", limits = c(min(r2$CHL), 1.5), oob = squish)
  #scale_fill_gradient(low = "yellow", high = "red", na.value = "grey")

# Now try and get trend through time

get_xy_vals <- function(data, x, y){
  if(length(x)!=1){
    x <- seq(from=x[1],to=x[2], by=500)}
  if(length(y)!=1){  
    y <- seq(from=y[1],to=y[2], by=500)}
  xy <- as.data.frame(expand.grid(x=x,y=y))  
  points <- seq(1,nrow(xy))
  location <- data.frame(points=points,x=xy[1],y=xy[2]) #DF
  coordinates(location) <- ~ x+ y #Convert to spatial points df format
  vals <- list() #empty list
  cbind.vals <- matrix(0, nrow = nrow(xy), ncol = 0) #empty matrix
  for(i in 1:length(data@layers)) {
    vals[[i]] <- raster::extract(data[[i]], location) #Extract values
    cbind.vals <- cbind(cbind.vals,vals[[i]])} #combine data into matrix
  valuesSP <- cbind(location,cbind.vals)
  valuesDF <- as.data.frame(valuesSP) #convert to dataframe
  names(valuesDF)[2:(ncol(valuesDF) - 2)] = c(2003:2019)
  # CONVERT df from wide format to long format
  data <- pivot_longer(valuesDF, cols = 2:(ncol(valuesDF)-2), names_to = 'year', values_to = 'CHL')
}

dat <- get_xy_vals(data = all_chl, x = 1072735 + c(0, 500), y = 6260250 + c(0, 500))

head(dat)
dim(dat)
unique(dat$year)

trends_plot_Yearly <- function(data, valname, year) {
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

trends_plot_Yearly(dat,valname="CHL", year=2003:2019)


