#######DESCRIPTION###############################
#processes raw river width data from DFO
#outputs rdata object and csv
#################################################
##load libraries
library(dplyr)
library(data.table)
library(readr)
library(tidyr)
library(ncdf4)
#date reference
idl_jd_base = 2451545 #1/1/2000
##user inputs
dir_raw = '/Volumes/gonggong/flood forecasting/data/raw/river width/'
dir_dat = '/Volumes/gonggong/flood forecasting/data/'
dir_ref = '/Volumes/gonggong/flood forecasting/data/reference tables/'
run_name = 'width_jan16'

#reference id table
width_meta = fread(paste0(dir_ref, 'GFDS_site_area_ID.txt'))

#process raw files
nc_temp = nc_open(file.path(dir_raw, 'Brahma_all.nc'))
width_dt_brahm = ncvar_get(nc_temp, 'river_data')
width_dt_brahm_date = ncvar_get(nc_temp, 'jul_date')
width_dt_brahm_id = ncvar_get(nc_temp, 'station')
nc_close(nc_temp)
width_dt_brahm_qc = ncvar_get(nc_temp, 'river_data_QC')
nc_close(nc_temp)

width_dt_brahm_date = width_dt_brahm_date - idl_jd_base
width_dt_brahm_date = as.Date(width_dt_brahm_date, origin = as.Date('2000-01-01'))
width_dt_brahm_dt = data.table(val = as.numeric(width_dt_brahm), AreaID = rep(width_dt_brahm_id, each = length(width_dt_brahm_date)), date = width_dt_brahm_date, qc = as.numeric(width_dt_brahm_qc)) %>% dplyr::filter(!is.na(val))

nc_temp = nc_open(file.path(dir_raw, 'Ganges_all.nc'))
width_dt_ganges = ncvar_get(nc_temp, 'river_data')
width_dt_ganges_date = ncvar_get(nc_temp, 'jul_date')
width_dt_ganges_id = ncvar_get(nc_temp, 'station')
width_dt_ganges_qc = ncvar_get(nc_temp, 'river_data_QC')
nc_close(nc_temp)

width_dt_ganges_date = width_dt_ganges_date - idl_jd_base
width_dt_ganges_date = as.Date(width_dt_ganges_date, origin = as.Date('2000-01-01'))
width_dt_ganges_dt = data.table(val = as.numeric(width_dt_ganges), AreaID = rep(width_dt_ganges_id, each = length(width_dt_ganges_date)), date = width_dt_ganges_date, qc = as.numeric(width_dt_ganges_qc)) %>% dplyr::filter(!is.na(val))

width_dt = rbind_list(width_dt_brahm_dt, width_dt_ganges_dt)
width_dt = width_dt %>% left_join(width_meta) %>% dplyr::rename(sta_id_width = GFDSsiteID, basin = Basin) %>% dplyr::select(date, sta_id_width, basin, qc, val)

#output processed files
write.csv(width_dt, paste0(dir_dat, run_name, '.csv'), row.names = F)
saveRDS(width_dt, paste0(dir_dat, run_name, '.rda'))
