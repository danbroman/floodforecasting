#######DESCRIPTION###############################
#fits 'rating curves' between oserved downstream discharge and upstream stage
#fits 'rating curves' between calcuated discharge and upstream stage
#saves as rdata object for use in flood forecasting code
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
nlc = nls.control(maxiter = 1000)

##user inputs
#processed data location
dir_dat = '/Volumes/gonggong/flood forecasting/data/'
#reference table location
dir_ref = '/Volumes/gonggong/flood forecasting/data/reference tables/'
#plot location (if desired)
dir_plots = '/Users/danbroman/Google Drive/Graduate School/Flood Forecasting/'
npts_thresh = 20
nse_thresh = 0.1

##read in data
stage_dat = readRDS(file.path(dir_dat, 'stage.rda'))
stage_weight_tbl = readRDS(file.path(dir_ref, 'stage_weight_tbl.rda'))

##mutate data
stage_dat_day = stage_dat %>% dplyr::mutate(date = as.Date(date)) %>% dplyr::filter(hour %in% c(6:18)) %>% dplyr::mutate(hour_min = hour - 12) %>% group_by(date, sta_id) %>% dplyr::slice(which.min(hour_min)) %>% ungroup() %>% dplyr::select(date, sta_id, basin, stage) %>% data.table() 

##fit rating curves
rating_curve_set = data.table(stage_weight_tbl, lag = rep(0:20, each = nrow(stage_weight_tbl))) 

rating_curve_tbl = NULL
for(i in 1:nrow(rating_curve_set)){
	sta_id_temp = rating_curve_set$sta_id[i]
	sta_id_us_temp = rating_curve_set$sta_id_us[i]
	lag_temp = rating_curve_set$lag[i]
	stage_fcst_ds_temp = stage_dat_day %>% dplyr::filter(sta_id == sta_id_temp) %>% dplyr::select(date, stage) %>% data.table()
	stage_fcst_us_temp = stage_dat_day %>% dplyr::filter(sta_id == sta_id_us_temp) %>% dplyr::mutate(date = date - lag_temp) %>% dplyr::rename(stage_us = stage) %>% dplyr::select(date, stage_us)
	stage_stage = stage_fcst_ds_temp %>% left_join(stage_fcst_us_temp) %>% dplyr::filter(!is.na(stage), !is.na(stage_us))
	
	rating_curve_temp = data.table(sta_id = sta_id_us_temp, lag = lag_temp, C = NA, a = NA, n = NA, nse = NA, scale = scale_temp, sta_id_ds = sta_id_temp, npts = NA, err = NA) 
	fit_dat = stage_stage %>% dplyr::select(stage, stage_us)
	if(nrow(fit_dat) > 0){
		power.nls = try(nlsLM(stage~C*(stage_us+a)^n, data = fit_dat, start = list(C=1, a=0, n=1), control = nlc)) 
		if(class(power.nls) != 'try-error'){
			fit_pred = predict(power.nls)
			nse_skill = round(NSE(fit_pred, fit_dat$stage), 4)
			npts = nrow(fit_dat)
			err = (1 / npts) * sum((fit_dat$stage - fit_pred)^2)
			rating_curve_temp = data.table(sta_id = sta_id_us_temp, lag = lag_temp, C = coef(power.nls)["C"], a = coef(power.nls)["a"], n = coef(power.nls)["n"], nse = nse_skill, npts = nrow(fit_dat), scale = scale_temp, sta_id_ds = sta_id_temp, npts = npts, err = err)  #stores current station rating curve values
		}	
	}
	rating_curve_tbl = bind_rows(rating_curve_tbl, rating_curve_temp)
}

#save data
saveRDS(rating_curve_tbl, paste0(dir_dat, 'rating_curve_stage.rda'))
rating_curve_tbl = rating_curve_tbl %>% dplyr::filter(nse >= nse_thresh, npts >= npts_thresh, !is.na(C))
saveRDS(rating_curve_tbl, paste0(dir_dat, 'rating_curve_stage_fl.rda'))




