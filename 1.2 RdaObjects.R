setwd("C:/Users/martisl/OneDrive - NIWA/NIWA Research Project/R_files")

load("results/all_chl_sd.Rda") # 1 stack with 17 layers
load("results\\dat.Rda") # all data in long format
load("results\\dat.clean.Rda") # all data in long format without NAs
# dat.clean <- dat[!is.na(dat$chl),]
load("results\\dat.spread.Rda") # dat clean data in wide format - contains NAs inside rows
# library(tidyr)
# dat.spread <- spread(dat.clean, key=year, value=chl)
load("results\\dat.spread.clean.Rda") # data spread clean with no NAs anywhere
# vector <- NA
# for (i in 1:nrow(dat.spread)){
# if (all(!is.na(dat.spread[i,3:19])))
# {vector <- c(vector, i)}  
# }
# dat.spread.clean <- dat.spread[vector,]
load("results/chl.stack.list.yearly.Rda") 

all_chl
