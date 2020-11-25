## Correlations in the data

rm(list = ls())
library(GGally)

# Get data 

source('read_in_data.R')

# functions for plots

trends_plot = function(data, k, spec_area = NULL) {
  j = which(names(data) == k) # index corresponding to col name
  if (length(j) == 0) stop("parameter name not present in data frame")
  if (!is.null(spec_area)) { # if area specified
    if ('area' %in% names(data)) { # check area is a variable in the data frame
      if (spec_area %in% data$area) { # if area exists in data frame
        data = data[data$area == spec_area, ] # trim
      } else {
        stop("Variable area not present in area component of data")
      }
    } else {
      stop("No area component of data")
    }
  }
  h = ggplot(data[!is.na(data[, j]), ], aes_string(x = names(data)[1], y = names(data)[j], colour = names(data)[2])) +
    geom_point() +
    geom_smooth(span = 0.3, se = F, method = loess) +
    geom_hline(yintercept = 0, color = 'black', linetype = 'dashed') +
    labs(x = 'Year', y = names(data)[j], color = 'Season') +
    scale_y_continuous(limits = range(data[!is.na(data[, j]), j]))
  if (is.null(spec_area)) {
    h = h + labs(title = paste0(names(data)[j], ' across time'))
  } else {
    h = h + labs(title = paste0(names(data)[j], ' across time in ', spec_area))
  }
  h
}

trends_season = function(data, k, spec_area = NULL) {
  j = which(names(data) == k) # index corresponding ot name
  if (length(j) == 0) stop("parameter name not present in data frame")
  if (!is.null(spec_area)) { # if area specified
    if ('area' %in% names(data)) { # check area is a variable in the data frame
      if (spec_area %in% data$area) { # if area exists in data frame
        data = data[data$area == spec_area, ] # trim
      } else {
        stop("Variable area not present in area component of data")
      }
    } else {
      stop("No area component of data")
    }
  }
  h = ggplot(data[!is.na(data[, j]), ], aes_string(x = names(data)[1], y = names(data)[j])) +
    geom_point(aes(color = season)) +
    geom_hline(yintercept = 0, color = 'black', linetype = 'dashed') +
    geom_smooth(span = 0.3) +
    labs(x = 'Year', y = names(data)[j], color = 'Season') +
    scale_y_continuous(limits = range(data[!is.na(data[, j]), j])) +
    facet_wrap(~season)
  if (is.null(spec_area)) {
    h = h + labs(title = paste0(names(data)[j], ' across time'))
  } else {
    h = h + labs(title = paste0(names(data)[j], ' across time in ', spec_area))
  }
  h
}

panel.cor <- function(x, y, digits = 2, cex.cor, ...) {
  # options
  low_col = 'orangered'
  high_col = 'deepskyblue3'
  
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient, p-value, confidence intervals
  r <- cor(x, y, use="complete.obs")
  p <- cor.test(x, y)$p.value
  q <- cor.test(x, y)$conf.int

  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r: ", txt, sep = "")
  
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p: ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  
  txt3 <- format(c(q, 0.123456789), digits = digits)
  txt3 <- paste("CI: ",txt3[1]," - ", txt3[2], sep = "")
  
  if (q[2] < 0) {
    text(0.5, 0.7, txt, col = low_col, font = 2)
    text(0.5, 0.5, txt2, col = low_col, font = 2)
    text(0.5, 0.3, txt3, col = low_col, font = 2)
  } else if (q[1] > 0) {
    text(0.5, 0.7, txt, col = high_col, font = 2)
    text(0.5, 0.5, txt2, col = high_col, font = 2)
    text(0.5, 0.3, txt3, col = high_col, font = 2)
  } else {
    text(0.5, 0.7, txt)
    text(0.5, 0.5, txt2)
    text(0.5, 0.3, txt3)
  }
}

pull_out_data = function(data_by_area = area_data, global_data = env_data, area_pick = 'STFW', season_pick = 'Spring') {
  if (area_pick %in% data_by_area$area == F) stop("Variable area_pick not in dataset data_by_area.")
  if (season_pick %in% data_by_area$season == F) stop("Variable season_pick not in dataset data_by_area.")
  dummy = data_by_area[data_by_area$area == area_pick & data_by_area$season == season_pick, ]
  dummy = merge(dummy, global_data) # combine with environmental data
  return(dummy)
}


# plotting pairwise correlations

windows(width = 600, height = 600)
ggpairs(env_data, columns = 3:12, title = 'All seasons') # with correlation values

# by season
ggpairs(env_data[env_data$season == 'Spring', 3:9], title = 'Spring') # spring
ggpairs(env_data[env_data$season == 'Summer', 3:9], title = 'Summer') # spring
ggpairs(env_data[env_data$season == 'Autumn', 3:9], title = 'Autumn') # spring
ggpairs(env_data[env_data$season == 'Winter', 3:9], title = 'Winter') # spring
dev.off()

# To save
# pdf("Plots/correlations_all_seasons.pdf", width = 16, height = 16)
# ggpairs(env_data, columns = 3:9) # with correlation values
# dev.off()



# go through different variables

# SAM
trends_plot(env_data, 'SAM')
trends_season(env_data, 'SAM')

# NINO
trends_plot(env_data, 'NINO')
trends_season(env_data, 'NINO')

# IPO_SMOOTH
trends_plot(env_data, 'IPO_SMOOTH')
trends_season(env_data, 'IPO_SMOOTH')

# TPI
trends_plot(env_data, 'TPI')
trends_season(env_data, 'TPI')

# M1
trends_plot(env_data, 'M1')
trends_season(env_data, 'M1')

# M3
trends_plot(env_data, 'M3')
trends_season(env_data, 'M3')

# Z2
trends_plot(env_data, 'Z2')
trends_season(env_data, 'Z2')

# KID_W
trends_plot(env_data, 'KID_W')
trends_season(env_data, 'KID_W')

# Save environmental seasonal data

for (j in names(env_data)[3:11]) {
  pdf(paste0("Plots/", j, ".pdf"), width = 8, height = 8)
  print(trends_season(env_data, j))
  dev.off()
}



# Look at data by area
names(area_data)

# SSH in STZ
trends_plot(area_data, 'SSH', spec_area = 'STZ')
trends_season(area_data, 'SSH', spec_area = 'STZ')

# SST in STFW
trends_plot(area_data, 'SST', spec_area = 'STFW')
trends_season(area_data, 'SST', spec_area = 'STFW')

# Chl. A in STFE
trends_plot(area_data, 'Chl_A', spec_area = 'STFE')
trends_season(area_data, 'Chl_A', spec_area = 'STFE')

# Wind speed in SAZ
trends_plot(area_data, 'Wind_speed', spec_area = 'SAZ')
trends_season(area_data, 'Wind_speed', spec_area = 'SAZ')


area_pick = 'STZ'
season_pick = 'Spring'
bla = pull_out_data(area_pick = area_pick, season_pick = season_pick)

# Plots - individual variables across time

# Plots - by season and area

windows(width = 600, height = 600)

# Sub-tropical zone - include Kidson W and SW

area_pick = 'STZ'

season_pick = 'Spring'
bla = pull_out_data(area_pick = area_pick, season_pick = season_pick)
pairs(bla[, c(11, 4:8, 12:14, 17, 19:21)], pch = 19, cex.labels = 1.5,
      main = paste0('Chl. A, ', area_pick, ' in ', season_pick)) # all
pairs(bla[, c(11, 4:8, 12:14, 17, 19:21)], upper.panel = panel.cor, pch = 19, cex.labels = 1.5,
      main = paste0('Chl. A, ', area_pick, ' in ', season_pick)) # all
pairs(bla[, c(11, 4:8, 12:14, 17, 19:22)], upper.panel = panel.cor, pch = 19, cex.labels = 1.5,
      main = paste0('Chl. A, ', area_pick, ' in ', season_pick)) # all including Kidson
pairs(bla[, c(11, 5, 7, 14, 19)], upper.panel = panel.cor, pch = 19, cex.labels = 1.5,
      main = paste0('Chl. A, ', area_pick, ' in ', season_pick)) # trimmed down

cor(bla[, c(11, 4:10, 12:34)], use = "pairwise.complete.obs") # correlation matrix for spring

season_pick = 'Summer'
bla = pull_out_data(area_pick = area_pick, season_pick = season_pick)
pairs(bla[, c(11, 4:8, 12:14, 17, 19:21)], upper.panel = panel.cor, pch = 19, cex.labels = 1.5,
      main = paste0('Chl. A, ', area_pick, ' in ', season_pick)) # all
pairs(bla[, c(11, 4:8, 12:14, 17, 19:25)], upper.panel = panel.cor, pch = 19, cex.labels = 1.5,
      main = paste0('Chl. A, ', area_pick, ' in ', season_pick)) # all

cor(bla[, c(11, 4:10, 12:34)], use = "pairwise.complete.obs") # correlation matrix for spring

season_pick = 'Autumn'
bla = pull_out_data(area_pick = area_pick, season_pick = season_pick)
pairs(bla[, c(11, 4:8, 12:14, 17, 19:22)], upper.panel = panel.cor, pch = 19, cex.labels = 1.5,
      main = paste0('Chl. A, ', area_pick, ' in ', season_pick)) # all


season_pick = 'Winter'
bla = pull_out_data(area_pick = area_pick, season_pick = season_pick)
pairs(bla[, c(11, 4:8, 12:14, 17, 19:22)], upper.panel = panel.cor, pch = 19, cex.labels = 1.5,
      main = paste0('Chl. A, ', area_pick, ' in ', season_pick)) # all

# Sub-tropical front West - include Kidson W and SW

area_pick = 'STFW'

season_pick = 'Spring'
bla = pull_out_data(area_pick = area_pick, season_pick = season_pick)
pairs(bla[, c(11, 4:8, 12:14, 17, 19:21)], upper.panel = panel.cor, pch = 19, cex.labels = 1.5,
      main = paste0('Chl. A, ', area_pick, ' in ', season_pick)) # all

season_pick = 'Summer'
bla = pull_out_data(area_pick = area_pick, season_pick = season_pick)
pairs(bla[, c(11, 4:8, 12:14, 17, 19:21)], upper.panel = panel.cor, pch = 19, cex.labels = 1.5,
      main = paste0('Chl. A, ', area_pick, ' in ', season_pick)) # all

season_pick = 'Autumn'
bla = pull_out_data(area_pick = area_pick, season_pick = season_pick)
pairs(bla[, c(11, 4:8, 12:14, 17, 19:21)], upper.panel = panel.cor, pch = 19, cex.labels = 1.5,
      main = paste0('Chl. A, ', area_pick, ' in ', season_pick)) # all


season_pick = 'Winter'
bla = pull_out_data(area_pick = area_pick, season_pick = season_pick)
pairs(bla[, c(11, 4:8, 12:14, 17, 19:21)], upper.panel = panel.cor, pch = 19, cex.labels = 1.5,
      main = paste0('Chl. A, ', area_pick, ' in ', season_pick)) # all

# Sub-tropical front East - no Kidson?

area_pick = 'STFE'

season_pick = 'Spring'
bla = pull_out_data(area_pick = area_pick, season_pick = season_pick)
pairs(bla[, c(11, 4:8, 12:14, 17, 19:21)], upper.panel = panel.cor, pch = 19, cex.labels = 1.5,
      main = paste0('Chl. A, ', area_pick, ' in ', season_pick)) # all

season_pick = 'Summer'
bla = pull_out_data(area_pick = area_pick, season_pick = season_pick)
pairs(bla[, c(11, 4:8, 12:14, 17, 19:21)], upper.panel = panel.cor, pch = 19, cex.labels = 1.5,
      main = paste0('Chl. A, ', area_pick, ' in ', season_pick)) # all

season_pick = 'Autumn'
bla = pull_out_data(area_pick = area_pick, season_pick = season_pick)
pairs(bla[, c(11, 4:8, 12:14, 17, 19:21)], upper.panel = panel.cor, pch = 19, cex.labels = 1.5,
      main = paste0('Chl. A, ', area_pick, ' in ', season_pick)) # all


season_pick = 'Winter'
bla = pull_out_data(area_pick = area_pick, season_pick = season_pick)
pairs(bla[, c(11, 4:8, 12:14, 17, 19:21)], upper.panel = panel.cor, pch = 19, cex.labels = 1.5,
      main = paste0('Chl. A, ', area_pick, ' in ', season_pick)) # all

# Sub-Antarctic Zone - no Kidson?

area_pick = 'SAZ'

season_pick = 'Spring'
bla = pull_out_data(area_pick = area_pick, season_pick = season_pick)
pairs(bla[, c(11, 4:8, 12:14, 17, 19)], upper.panel = panel.cor, pch = 19, cex.labels = 1.5,
      main = paste0('Chl. A, ', area_pick, ' in ', season_pick)) # all

season_pick = 'Summer'
bla = pull_out_data(area_pick = area_pick, season_pick = season_pick)
pairs(bla[, c(11, 4:8, 12:14, 17, 19)], upper.panel = panel.cor, pch = 19, cex.labels = 1.5,
      main = paste0('Chl. A, ', area_pick, ' in ', season_pick)) # all

season_pick = 'Autumn'
bla = pull_out_data(area_pick = area_pick, season_pick = season_pick)
pairs(bla[, c(11, 4:8, 12:14, 17, 19)], upper.panel = panel.cor, pch = 19, cex.labels = 1.5,
      main = paste0('Chl. A, ', area_pick, ' in ', season_pick)) # all


season_pick = 'Winter'
bla = pull_out_data(area_pick = area_pick, season_pick = season_pick)
pairs(bla[, c(11, 4:8, 12:14, 17, 19)], upper.panel = panel.cor, pch = 19, cex.labels = 1.5,
      main = paste0('Chl. A, ', area_pick, ' in ', season_pick)) # all


# Save plots

# for (j in unique(area_data$area)) {
#   area_pick = j
#   for (k in unique(area_data$season)) {
#     season_pick = k
#     bla = pull_out_data(area_pick = area_pick, season_pick = season_pick)
#     
#     pdf(paste0("Plots/By area/", area_pick, ", ", season_pick, ".pdf"), width = 16, height = 16)
#     print(ggpairs(bla[, 4:22], title = paste0(area_pick, ", ", season_pick))) # spring
#     dev.off()
#     
#     pdf(paste0("Plots/", area_pick, ", ", season_pick, ", Kidson.pdf"), width = 16, height = 16)
#     print(ggpairs(bla[, c(18, 20:32)], title = 'STZ, Spring, Kidson')) # spring
#     dev.off()
#   }
# }
