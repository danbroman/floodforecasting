#######DESCRIPTION###############################
#processes area, prcp, et, qavg table for use as weights in 
#rating curve fits
#outputs rdata object and csv
#################################################
##load libraries
library(dplyr)
library(data.table)
library(stringr)
library(tidyr)
library(readr)

##user inputs
#processed data location
dir_dat = '/Volumes/gonggong/flood forecasting/data/'
#reference table location
dir_ref = '/Volumes/gonggong/flood forecasting/data/reference tables/'

##read in data
stage_weight = fread(file.path(dir_ref, 'qavg_normalizing_coef.csv'))
stage_topology = readRDS(file.path(dir_ref, 'stage_topology_tbl.rda'))

##process data
stage_weight = stage_weight %>% dplyr::rename(sta_id = gage_names) %>% dplyr::select(sta_id, area, prcp, et, Qavg)
stage_weight_us = stage_weight %>% dplyr::rename(sta_id_us = sta_id, area_us = area, prcp_us = prcp, et_us = et, Qavg_us = Qavg)
stage_weight = stage_topology %>% left_join(stage_weight)
stage_weight = stage_weight %>% left_join(stage_weight_us)
stage_weight = stage_weight %>% dplyr::mutate(scale = Qavg_us / Qavg)

##save processed tables
saveRDS(stage_weight, paste0(dir_ref, 'stage_weight_tbl.rda'))
write.csv(stage_weight, paste0(dir_ref, 'stage_weight_tbl.csv'), row.names = F)
