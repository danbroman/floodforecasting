#######DESCRIPTION###############################
#processes raw stage data from Indian CWC records
#outputs rdata object and csv
#################################################
#load libraries
library(dplyr)
library(data.table)
library(readr)
library(tidyr)
library(ncdf4)
library(tools)
##user inputs
#raw file location
dir_raw = '/Volumes/gonggong/flood forecasting/data/raw/stage/'
#output location
dir_dat = '/Volumes/gonggong/flood forecasting/data/'
#quality control flag - see qc_reference for a description of the flag values and methods
qc_thresh = 90
##process raw files
stage_list = list.files(paste0(dir_raw), pattern = '.nc')
date_ref = as.POSIXct('2000-01-01 00:00:00', "%Y-%m-%d %H:%M:%S", tz = 'GMT')
date_ref = data.table(date = seq(from = as.POSIXct('2000-01-01 00:00:00'), to = as.POSIXct(Sys.Date()), by = 'min'))
date_ref$date_ind = 0:(nrow(date_ref) - 1)
stage_dat = NULL
for(i in 1:length(stage_list)){
	sta_id_temp = file_path_sans_ext(stage_list[i])
	nc_temp = try(nc_open(paste0(dir_raw, stage_list[i])))
	if(length(nc_temp) > 1){
		stage_temp = ncvar_get(nc_temp, 'level')
		qc_temp = ncvar_get(nc_temp, 'QC')
		date_temp = ncvar_get(nc_temp, 'date')

		stage_dat_temp = data.table(sta_id = sta_id_temp, date_ind = date_temp, flag = qc_temp, stage = stage_temp)
		stage_dat = rbind_list(stage_dat, stage_dat_temp)
	}
}
stage_dat = stage_dat %>% left_join(date_ref) %>% dplyr::select(-date_ind) %>% dplyr::filter(flag >= qc_thresh)
#output processed files
write.csv(stage_dat, paste0(dir_dat, 'stage.csv'), row.names = F)
saveRDS(stage_dat, paste0(dir_dat, 'stage.csv'))
