setwd("C:/Users/martisl/OneDrive - NIWA/NIWA Research Project/R_files")
#rm(list = ls())

#library(ncdf4) # package for netcdf manipulation
library(raster)
library(scales) # package for raster manipulation
library(ggplot2)
library(gganimate)
theme_set(theme_bw())
library(animation)
library(tidyr)
library(tidyverse)


# creating GIF with 100by100 raster
dat <- get_xy_vals(data_list=ls_chl_yr, # ls_chl_yr comes from chl.stack.list.yearly.Rda
                   x=(1088228+(500*143)):(1088228+(500*(143+9))), 
                   y=(6260250-(500*207)):(6260250-(500*(207+9))))
head(dat)
pp <- ggplot() +
  geom_raster(data = dat, aes(x = x, y = y, fill = chl)) +
  scale_y_reverse() +
  coord_equal() +
  labs(title = "100 by 100 raster", fill = 'Chl. A') +
  scale_fill_gradient(low = "yellow", high = "red", limits = c(min(dat$chl), 1.5), oob = squish) +
  transition_time(year)+
  labs(title = "Year: {frame_time}")
pp
#anim_save("test-animaiton.gif", pp)

r2 = raster::as.data.frame(all_chl, xy = T) # # all_chl comes frm all_chl_sd.Rda
names(r2)[3:19] <- 2003:2019
head(r2)
class(r2)

r2.long <- gather(r2, key="year", value="chl",names(r2)[3:19])
r2.long$year <- as.numeric(r2.long$year)
str(r2.long)

a <- r2.long[1:13889264,]


# Creating GIF for whole NZ chl
chl_gif <- ggplot() +
  geom_raster(data = r2.long, aes(x = x, y = y, fill = chl)) +
  scale_y_reverse() +
  coord_equal() +
  labs(title = "Chlorophyll", fill = 'Chlorophyll level') +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "grey", 
                      limits = c(min(r2.long$chl, na.rm = T), 1), oob = squish)+
  transition_time(year)+
  labs(title = "Year: {frame_time}")
chl_gif

#ATTEMPT 2

a <- brick(all_chl)

animate(a, pause=0.25, n=1)



# creating one year image for whole NZ
r2 = raster::as.data.frame(all_chl[[1]], xy = T) # all_chl comes frm all_chl_sd.Rda
names(r2)[3] = 'CHL'
ggplot() +
  geom_raster(data = r2, aes(x = x, y = y, fill = CHL)) +
  scale_y_reverse() +
  coord_equal() +
  labs(title = "One year", fill = 'Chl. A') +
  scale_fill_gradient(low = "yellow", high = "red", limits = c(min(r2$CHL), 1.5), oob = squish)

    
# Creating facet wrap plots
ggplot() +
  geom_raster(data = r2.long, aes(x = x, y = y, fill = chl)) +
  facet_wrap(~year) +
  scale_y_reverse() +
  coord_equal() +
  labs(title = 'Chlorophyll', fill = 'Chl. A') +
  scale_fill_gradient(low = "yellow", high = "red", limits = c(min(r2.long$chl, na.rm = T), 1.5), oob = squish)
  #scale_fill_gradient(low = "yellow", high = "red", na.value = "grey")



######################################## GAPMINDER EXAMPLE ###########

library(gapminder)
head(gapminder)
p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy") +
  transition_time(year) +
  labs(title = "Year: {frame_time}")
#anim_save("gapminder.gif", p)
