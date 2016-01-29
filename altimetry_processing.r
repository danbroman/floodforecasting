#######DESCRIPTION###############################
#processes raw altimetry data from Charon Birkett UMd
#outputs rdata object and csv
#################################################
##load libraries
library(dplyr)
library(data.table)
library(readr)
library(tidyr)
nasa_base_date = as.Date('1958-01-01')
##user inputs
#raw file location
dir_raw = '/Volumes/gonggong/flood forecasting/data/raw/altimetry/'
#output location
dir_dat = '/Volumes/gonggong/flood forecasting/data/'
run_name = 'alt_jan15a'
#process raw files
alt_list = list.files(paste0(dir_raw), pattern = '.txt')
alt_names = substr(do.call(rbind, strsplit(alt_list, '.try'))[, 2], 1, 3)
alt_sta = as.numeric(substr(alt_names, 1, 2))
alt_stavar = substr(alt_names, 3, 3)
alt_dt = NULL
for(i in 1:length(alt_list)){
	alt_temp = data.table(read.table(paste0(dir_raw, alt_list[i]), 
		skip = 6, header = T))
	fileName = paste0(dir_raw, alt_list[i])
	alt_meta = readChar(fileName, file.info(fileName)$size)
	alt_meta = substr(alt_meta, 0, 331)
	refalt = as.numeric(unlist(strsplit(alt_meta, '='))[12])
	alt_name = alt_names[i]
	alt_temp = alt_temp %>% dplyr::mutate(sta = alt_sta[i], stavar = alt_stavar[i], refalt = refalt)
	alt_dt = rbind_list(alt_dt, alt_temp)
}
alt_dt = alt_dt %>% data.table() %>% dplyr::mutate(date = nasa_base_date + timday, alt = avhgt + refalt, sta = as.numeric(sta)) %>% dplyr::select(date, sta, stavar, avhgt, error, totbias, alt)
#output processed files
write.csv(alt_dt, paste0(dir_dat, run_name, '.csv'), row.names = F)
saveRDS(alt_dt, paste0(dir_dat, run_name, '.rda'))
