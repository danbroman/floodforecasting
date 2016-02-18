#######DESCRIPTION###############################
#uses stage-stage fitted rating curves to produce 0-15 day lead
#forecasts of stage
#requires fitted rating curves from stage_ratingcurve_stage_fit.r 
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
dir_fcst = ''
run_name = ''
##process data 
fcst_dates = seq(from = as.Date(fcst_start_date), to = as.Date(fcst_end_date), by = 'day')
fcst_tbl = data.table(sta_id = rep(fcst_sta_ids, each = length(fcst_dates) * length(fcst_leads)), fcst_date = fcst_dates, fcst_lead = rep(fcst_leads, each = length(fcst_dates)))

##forecast
stage_fcst_tbl = NULL
for(j in 1:nrow(fcst_tbl)){
	fcst_lead_temp = fcst_tbl$fcst_lead[j]
	fcst_date_temp = fcst_tbl$fcst_date[j]
	fcst_sta_id_temp = fcst_tbl$sta_id[j]
	rating_curve_fcst_tbl = rating_curve_tbl %>% dplyr::filter(lag >= fcst_lead_temp, sta_id_ds == fcst_sta_id_temp) %>% group_by(sta_id) %>% data.table()

	stage_fcst = NULL
	for(i in fcst_lead_temp:15){
		stage_fcst_temp = stage_dat_day %>% dplyr::filter(sta_id %in% sta_id_set$sta_id_us, date == (fcst_date_temp - as.difftime(i, unit = 'days'))) %>% dplyr::mutate(lag = i) %>% data.table()
		stage_fcst_temp = stage_fcst_temp %>% left_join(rating_curve_fcst_tbl)
		stage_fcst = bind_rows(stage_fcst, stage_fcst_temp)
	}

	stage_fcst = stage_fcst %>% dplyr::mutate(stage_fcst_uw = C * (stage + a)^n, wt = 1 / err) %>% dplyr::filter(!is.na(stage_fcst_uw)) %>% dplyr::mutate(stage_fcst = stage_fcst_uw * wt)

	stage_fcst = stage_fcst %>% group_by(sta_id) %>% dplyr::slice(which.max(nse)) %>% ungroup()
	# stage_fcst$fcst_date = fcst_date_temp
	# stage_fcst$fcst_lead = fcst_lead_temp
	# stage_fcst_diag = bind_rows(stage_fcst_diag, stage_fcst)
	stage_fcst_best = sum(stage_fcst$stage_fcst) / sum(stage_fcst$wt)
	stage_fcst_temp = data.table(sta_id = fcst_sta_id_temp, stage_fcst = stage_fcst_best, fcst_lead = fcst_lead_temp, fcst_date = fcst_date_temp)

	stage_fcst_tbl = bind_rows(stage_fcst_tbl, stage_fcst_temp)
}

#save data
saveRDS(stage_fcst_tblpaste0(dir_fcst, run_name, '_fcst.rda'))
