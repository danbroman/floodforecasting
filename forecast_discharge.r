#######DESCRIPTION###############################
#uses stage-discharge fitted rating curves to produce 0-15 day lead
#forecasts of stage
#requires fitted rating curves from stage_ratingcurvefit.r 
#observed stage values
#################################################
##load libraries
library(tools)
library(ncdf4)
library(dplyr)
library(data.table)
library(ggplot2)
library(minpack.lm)
library(hydroGOF)
library(R.utils)
library(scales)
library(RColorBrewer)

##user inputs
fcst_start_date = '2015-06-15'
fcst_end_date = '2015-10-31'
fcst_leads = 0:15
fcst_sta_ids = c('007-mgd4ptn', '002-MGD4PTN', '006-mgd4ptn', '005-mgd4ptn', '004-MGD4PTN', '029-mgd5ptn', '024-MGD4PTN', '023-mgd4ptn', '022-mgd4ptn', '025-mgd4ptn', '019-mgd4ptn', '001-MGD4PTN', '015-mgd4ptn')
dir_dat = '/Volumes/gonggong/flood forecasting/data/'
dir_ref = '/Volumes/gonggong/flood forecasting/data/reference tables/'
dir_fcst = '/Volumes/gonggong/flood forecasting/data/'
run_name = 'bagmati-kosi'
npts_thresh = 20
nse_thresh = 0.1

##read in data
stage_dat = readRDS(paste0(dir_dat, 'stage.rda'))
rating_curve_tbl = readRDS(paste0(dir_dat, 'rating_curve_all-bagmati-kosi.rda'))
stage_weight_tbl = readRDS(file.path(dir_ref, 'stage_weight_tbl.rda')) 

##process data 
stage_dat_day = stage_dat %>% dplyr::filter(hour %in% c(6:18)) %>% dplyr::mutate(hour_min = hour - 12) %>% group_by(year, month, day, sta_id) %>% dplyr::slice(which.min(hour_min)) %>% ungroup() %>% dplyr::select(date, sta_id, basin, stage) %>% dplyr::mutate(date = as.Date(date)) %>% data.table() 
rating_curve_tbl = rating_curve_tbl %>% dplyr::filter(nse >= nse_thresh, npts > npts_thresh)
rating_curve_opt_tbl = rating_curve_tbl %>% group_by(sta_id) %>% dplyr::slice(which.max(nse)) %>% data.table()

fcst_dates = seq(from = as.Date(fcst_start_date), to = as.Date(fcst_end_date), by = 'day')
fcst_tbl = data.table(sta_id = rep(fcst_sta_ids, each = length(fcst_dates) * length(fcst_leads)), fcst_date = fcst_dates, fcst_lead = rep(fcst_leads, each = length(fcst_dates)))

##forecast
Q_fcst_tbl = NULL
for(j in 1:nrow(fcst_tbl)){
	fcst_lead_temp = fcst_tbl$fcst_lead[j]
	fcst_date_temp = fcst_tbl$fcst_date[j]
	fcst_sta_id_temp = fcst_tbl$sta_id[j]
	rating_curve_fcst_tbl = rating_curve_tbl %>% dplyr::filter(lag >= fcst_lead_temp, sta_id_ds == fcst_sta_id_temp) %>% group_by(sta_id) %>% data.table()
	sta_id_set = stage_weight_tbl %>% dplyr::filter(sta_id == fcst_sta_id_temp) %>% dplyr::select(sta_id_us)

	Q_fcst = NULL
	for(i in fcst_lead_temp:15){
		Q_fcst_temp = stage_dat_day %>% dplyr::filter(sta_id %in% sta_id_set$sta_id_us, date == (fcst_date_temp - as.difftime(i, unit = 'days'))) %>% dplyr::mutate(lag = i) %>% data.table()
		Q_fcst_temp = Q_fcst_temp %>% left_join(rating_curve_fcst_tbl)
		Q_fcst = bind_rows(Q_fcst, Q_fcst_temp)
	}

	Q_fcst = Q_fcst %>% dplyr::mutate(Q_fcst_uw = C * (stage + a)^n, wt = 1 / err) %>% dplyr::filter(!is.na(Q_fcst_uw)) %>% dplyr::mutate(Q_fcst = Q_fcst_uw * wt)
	Q_fcst = Q_fcst %>% group_by(sta_id) %>% dplyr::slice(which.max(nse)) %>% ungroup()
	Q_fcst_best = sum(Q_fcst$Q_fcst) / sum(Q_fcst$wt)
	Q_fcst_temp = data.table(sta_id = fcst_sta_id_temp, Q_fcst = Q_fcst_best, fcst_lead = fcst_lead_temp, fcst_date = fcst_date_temp)

	Q_fcst_tbl = bind_rows(Q_fcst_tbl, Q_fcst_temp)
}

Q_stage_fcst_tbl = Q_fcst_tbl %>% left_join(rating_curve_opt_tbl) %>% dplyr::mutate(stage_fcst = (Q_fcst / C) ^ (1 / n) - a)
#save data
saveRDS(Q_stage_fcst_tbl, paste0(dir_fcst, run_name, '_Q_stage_fcst.rda'))
