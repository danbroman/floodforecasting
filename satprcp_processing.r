#######DESCRIPTION###############################
#processes raw merged satellite precipitation from NCAR
#TRMM + CMORPH + GsMAP
#outputs rdta object and csv
#################################################
##load libraries
library(dplyr)
library(data.table)
library(readr)
library(tidyr)
library(ncdf4)
list.dirs = function(path=".", pattern=NULL, all.dirs=FALSE,
  full.names=FALSE, ignore.case=FALSE) {
  # use full.names=TRUE to pass to file.info
  all <- list.files(path, pattern, all.dirs,
           full.names=TRUE, recursive=FALSE, ignore.case)
  dirs <- all[file.info(all)$isdir]
  # determine whether to return full names or just dir names
  if(isTRUE(full.names))
    return(dirs)
  else
    return(basename(dirs))
}
##user inputs
#raw file location
dir_raw = '/Volumes/gonggong/flood forecasting/data/raw/satprcp/'
#output location
dir_dat = '/Volumes/gonggong/flood forecasting/data/'

##process raw files
stadir_list = list.dirs(dir_raw)
prcp_dt = NULL
for(i in 1:length(stadir_list)){
	stadir_temp = stadir_list[i]
	file_list = list.files(paste0(dir_raw, stadir_temp), pattern = 'Q_satellite.*.dat')
	for(j in 1:length(file_list)){
		file_temp = file_list[j]
		filepath_temp = paste0(dir_raw, stadir_temp, '/', file_temp)
		to_read = file(filepath_temp, 'rb')
		header_temp = readBin(to_read, integer(), n = 4, size = 2, signed = T)
		nday_temp = header_temp[1]
		year_temp = header_temp[4]
		month_temp = header_temp[3]
		day_temp = header_temp[2]
		date_temp = seq(from = as.Date(paste(year_temp, month_temp, day_temp, sep = '-')), by = 'day', length.out = nday_temp)
		prcp_temp = readBin(to_read, numeric(), size = 4, n = nday_temp)
		close(to_read)
		prcp_temp_dt = data.table(sta_id = stadir_temp, date = date_temp, prcp = dat_temp)
		prcp_dt = rbind_list(prcp_dt, prcp_temp_dt)
	}
}
#output processed files
write.csv(prcp_dt, paste0(dir_dat, 'sat_prcp.csv'), row.names = F)
saveRDS(prcp_dt, paste0(dir_dat, 'sat_prcp.rda'))
