#######DESCRIPTION###############################
#processes stage topology table from GIS analysis
#stage stations upstream of a given station
#################################################
##load libraries
library(dplyr)
library(data.table)
library(stringr)
library(tidyr)
library(readr)

##user inputs
dir_ref = '/Volumes/gonggong/flood forecasting/data/reference tables/'
file_name = 'Upstream_Gages_Stage_Station_Wsheds.txt'

##read in raw data
stage_meta = fread(paste0(dir_ref, 'cwc_honumber_match_edit_thm_v3.csv')) 
stage_meta = stage_meta %>% dplyr::mutate(ho_number = as.numeric(ho_number))
stage_topology_raw = readLines(file.path(dir_ref, file_name))

##process data
stage_topology = NULL
for(i in 2:length(stage_topology_raw)){
	str_temp = unlist(strsplit(stage_topology_raw[i], split = ','))
	str_temp = str_replace_all(str_temp, '[[:punct:]]', '')
	str_temp = str_trim(str_temp)
	nele = length(str_temp)
	stage_topology_temp = data.table(sta_name = str_temp[2], ho_number = str_temp[1], ho_number_us = str_temp[3:nele])
	stage_topology = bind_rows(stage_topology, stage_topology_temp)
}

stage_topology = stage_topology %>% dplyr::mutate(ho_number = as.numeric(ho_number), ho_number_us = as.numeric(str_replace_all(ho_number_us, 'u', '')))
stage_topology = stage_topology %>% left_join(stage_meta) 

##save processed tables
saveRDS(stage_topology, paste0(dir_ref, 'stage_topology_tbl.rda'))
write.csv(stage_topology, paste0(dir_ref, 'stage_topology_tbl.csv'), row.names = F)
