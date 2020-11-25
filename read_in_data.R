# Read in all data

# Global data 

env_data = read.csv('data/climate_variables.csv')
for (j in c(3:ncol(env_data))) env_data[, j] = as.numeric(as.character(env_data[, j]))
names(env_data) = c('year', 'season', 'SAM', 'NINO', 'SOI', 'IPO_SMOOTH', 'IPO_UNSMOOTH', 'TPI', 'M1', 'M3', 'Z2',
                    'KID_W', 'KID_SW', 'KID_TSW', 'KID_T', 'KID_NE', 'KID_R', 'KID_HW', 'KID_HE', 'KID_HNW',
                    'KID_TNW', 'KID_HSE', 'KID_H', 'KID_TROUGH', 'KID_ZONAL', 'KID_BLOCKING')
env_data$season <- factor(env_data$season, levels = c('son', 'djf', 'mam', 'jja'),
                          labels = c('Spring', 'Summer', 'Autumn', 'Winter'))

# Area-specific data
area_data = read.csv('data/multiarea_climate_variables.csv')
for (j in c(4:ncol(area_data))) area_data[, j] = as.numeric(as.character(area_data[, j]))
names(area_data) = c('year', 'season', 'area', 'SSH', 'SST', 'Wind_speed', 'U10', 'V10', 'Precipitation',
                     'Sea_level_pressure', 'mld_30', 'mld_125', 'Chl_A', 'POC', 'VGPM')
area_data = area_data[, 1:(ncol(area_data)-1)]
area_data$season <- factor(area_data$season, levels = c('son', 'djf', 'mam', 'jja'), 
                           labels = c('Spring', 'Summer', 'Autumn', 'Winter'))
area_data$area <- factor(area_data$area, levels = c("saz", "stfe", "stfw", "stz"), 
                         labels = c('SAZ', 'STFE', 'STFW', 'STZ'))
area_data_all_chl = area_data # save with old Chl. A data
area_data$Chl_A[area_data$year < 1990] = NA # IMPORTANT - removing old Chl.A data due to unreliability

# Add in prior Chl. A as a column
area_data$Chl_A_prev = NA

for (j in unique(area_data$area)) {
  first_year = min(area_data$year)
  for (k in unique(area_data$year)) {
    area_data$Chl_A_prev[area_data$area == j & area_data$year == k & area_data$season == 'Autumn'] = 
      area_data$Chl_A[area_data$area == j & area_data$year == k & area_data$season == 'Summer']
    area_data$Chl_A_prev[area_data$area == j & area_data$year == k & area_data$season == 'Winter'] = 
      area_data$Chl_A[area_data$area == j & area_data$year == k & area_data$season == 'Autumn']
    area_data$Chl_A_prev[area_data$area == j & area_data$year == k & area_data$season == 'Spring'] = 
      area_data$Chl_A[area_data$area == j & area_data$year == k & area_data$season == 'Winter']
    if (k != first_year) { # for summer
      area_data$Chl_A_prev[area_data$area == j & area_data$year == k & area_data$season == 'Summer'] = 
        area_data$Chl_A[area_data$area == j & area_data$year == k-1 & area_data$season == 'Spring']
      
    }
  }  
}

