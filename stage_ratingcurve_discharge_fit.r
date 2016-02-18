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
nse_thresh = 0.3
#subset list...fit only for set of stations
subset_list = c('001-MGD4PTN', '015-mgd4ptn', '007-mgd4ptn', '002-MGD4PTN', '006-mgd4ptn', '005-mgd4ptn', '004-MGD4PTN', '029-mgd5ptn', '024-MGD4PTN', '023-mgd4ptn', '022-mgd4ptn', '025-mgd4ptn', '019-mgd4ptn')

##read in data
discharge_dat = data.table(readRDS(file.path(dir_dat, 'discharge_obs.rda')))
stage_dat = readRDS(file.path(dir_dat, 'stage.rda'))

stage_weight_tbl = readRDS(file.path(dir_ref, 'stage_weight_tbl.rda'))

##mutate data
#grabs stage data closest to noon
stage_dat_6hr = stage_dat %>% dplyr::filter(hour %in% c(6:18)) %>% dplyr::mutate(hour_min = hour - 12) %>% group_by(year, month, day, sta_id) %>% dplyr::slice(which.min(hour_min)) %>% ungroup() %>% dplyr::select(date, sta_id, basin, stage) %>% dplyr::mutate(date = as.Date(date)) %>% data.table() 

discharge_dat = discharge_dat %>% dplyr::mutate(date = as.Date(date)) %>% dplyr::filter(flow >= 0)

#fit rating curves...use downstream discharge only
sta_id_set = stage_dat_6hr %>% distinct(sta_id) %>% dplyr::select(sta_id, basin)
rating_curve_set = data.table(sta_id_set, lag = rep(0:20, each = nrow(sta_id_set))) 

rating_curve_tbl = NULL
for(i in 1:nrow(rating_curve_set)){
	rating_curve_set_temp = rating_curve_set[i]
	sta_id_temp = rating_curve_set_temp$sta_id
	basin_temp = rating_curve_set_temp$basin
	sta_id_ds_temp = paste(basin_temp, 'discharge', sep = '_')
	lag_temp = rating_curve_set_temp$lag
	# scale_temp = stage_weight_tbl %>% dplyr::filter(sta_id == sta_id_ds_temp, sta_id_us == sta_id_temp) %>% dplyr::select(scale) %>% as.numeric()
	scale_temp = 1
	stage_dat_temp = stage_dat_6hr %>% dplyr::filter(sta_id == sta_id_temp) %>% dplyr::mutate(date = date - lag_temp)
	discharge_dat_temp = discharge_dat %>% dplyr::filter(basin == basin_temp)
	stage_discharge_dat_temp = stage_dat_temp %>% left_join(discharge_dat_temp) %>% dplyr::filter(!is.na(stage), !is.na(flow))
	rating_curve_temp = data.table(sta_id = sta_id_us_temp, lag = lag_temp, C = NA, a = NA, n = NA, nse = NA, scale = scale_temp, sta_id_ds = sta_id, npts = NA, err = NA) 
	if(scale_temp <= 1 & !is.na(scale_temp) & nrow(stage_discharge_dat_temp) > 0){		
		fit_dat = data.frame(dplyr::select(stage_discharge_dat_temp, flow, stage)) %>% dplyr::mutate(flow = flow / 1000) #pulls out just stage and flow; divides flow by 1000
		power.nls = try(nlsLM(flow~C*(stage+a)^n, data = fit_dat, start = list(C=1, a=0, n=1), control = nlc)) #fits function; optimize using nonlinear least squares
		if(class(power.nls) != 'try-error'){
			fit_pred = predict(power.nls)
			nse_skill = round(NSE(fit_pred, fit_dat$flow), 4)
			npts = nrow(fit_dat)
			err = (1 / npts) * sum((fit_dat$flow - fit_pred)^2)
			rating_curve_temp = data.table(sta_id = sta_id_temp, lag = lag_temp, C = coef(power.nls)["C"], a = coef(power.nls)["a"], n = coef(power.nls)["n"], nse = nse_skill, npts = nrow(fit_dat), scale = scale_temp, sta_id_ds = paste(basin_temp, 'discharge', sep = '_'), npts = npts, err = err)  #stores current station rating curve values
		}		
	
	}
	rating_curve_tbl = bind_rows(rating_curve_tbl, rating_curve_temp)	#stores all rating curve values
	}
}

#calculate optimal lag using dischage only...filter using min npts and nse
rating_curve_opt_tbl = rating_curve_tbl %>% group_by(sta_id) %>% dplyr::filter(npts >= npts_thresh, nse >= nse_thresh) %>% dplyr::slice(which.max(nse)) %>% data.table()

saveRDS(rating_curve_tbl, paste0(dir_dat, 'rating_curve_discharge.rda'))
saveRDS(rating_curve_opt_tbl, paste0(dir_dat, 'rating_curve_opt_discharge.rda'))

# rating_curve_opt_tbl = readRDS(paste0(dir_dat, 'rating_curve_opt_discharge.rda'))
#calculate discharge using optimal rating curve
rating_curve_tbl = readRDS(paste0(dir_dat, 'rating_curve_discharge.rda'))
discharge_calc_dat = stage_dat_6hr %>% left_join(rating_curve_opt_tbl) %>% dplyr::filter(!is.na(C)) %>% dplyr::mutate(flow = C*(stage+a)^n * 1000 * scale) %>% dplyr::select(sta_id, sta_id_ds, lag, date, basin, flow, nse)

saveRDS(rating_curve_opt_tbl, paste0(dir_dat, 'rating_curve_opt_discharge.rda'))

#fit rating curves...daisy chain upstream
stage_weight_tbl = stage_weight_tbl %>% dplyr::filter(!sta_id %in% c('ganges_discharge', 'brahmaputra_discharge', 'meghna_discharge'))
rating_curve_us_set = data.table(stage_weight_tbl, lag = rep(0:20, each = nrow(stage_weight_tbl)))
rating_curve_us_set = rating_curve_us_set %>% dplyr::filter(sta_id %in% subset_list)

rating_curve_us_tbl = NULL
for(i in 1:nrow(rating_curve_us_set)){
	rating_curve_set_temp = rating_curve_us_set[i]
	sta_id_temp = rating_curve_set_temp$sta_id
	sta_id_us_temp = rating_curve_set_temp$sta_id_us
	lag_temp = rating_curve_set_temp$lag
	# scale_temp = rating_curve_set_temp$scale
	scale_temp = 1
	stage_dat_temp = stage_dat_6hr %>% dplyr::filter(sta_id == sta_id_us_temp) %>% dplyr::mutate(date = date - lag_temp) %>% dplyr::select(date, stage, basin)

	discharge_calc_sta_id_ds = discharge_calc_dat %>% dplyr::filter(sta_id == sta_id_temp) %>% dplyr::select(sta_id_ds, nse) %>% dplyr::slice(which.max(nse)) %>% dplyr::select(sta_id_ds) %>% as.character()
	discharge_calc_dat_temp = discharge_calc_dat %>% dplyr::filter(sta_id == sta_id_temp, sta_id_ds == discharge_calc_sta_id_ds) %>% dplyr::select(sta_id, date, flow) %>% data.table()
	
	stage_discharge_dat_temp = stage_dat_temp %>% left_join(discharge_calc_dat_temp) %>% dplyr::filter(!is.na(stage), !is.na(flow))

	rating_curve_temp = data.table(sta_id = sta_id_us_temp, lag = lag_temp, C = NA, a = NA, n = NA, nse = NA, scale = scale_temp, sta_id_ds = sta_id, npts = NA, err = NA) 

	if(scale_temp <= 1 & !is.na(scale_temp) & nrow(stage_discharge_dat_temp) > 0){		
		fit_dat = data.frame(dplyr::select(stage_discharge_dat_temp, flow, stage, basin, date)) %>% dplyr::mutate(flow = flow / 1000) #pulls out just stage and flow; divides flow by 1000
		power.nls = try(nlsLM(flow~C*(stage+a)^n, data = fit_dat, start = list(C=1, a=0, n=1), control = nlc)) #fits function; optimize using nonlinear least squares
		if(class(power.nls) != 'try-error'){
			fit_pred = predict(power.nls)
			nse_skill = round(NSE(fit_pred, fit_dat$flow), 4)
			npts = nrow(fit_dat)
			err = (1 / npts) * sum((fit_dat$flow - fit_pred)^2)
			rating_curve_temp = data.table(sta_id = sta_id_us_temp, lag = lag_temp, C = coef(power.nls)["C"], a = coef(power.nls)["a"], n = coef(power.nls)["n"], nse = nse_skill, npts = nrow(fit_dat), scale = scale_temp, sta_id_ds = sta_id_temp, npts = npts, err = err)  #stores current station rating curve values
			discharge_calc_temp = data.table(fit_dat, rating_curve_temp) %>% dplyr::mutate(flow = C*(stage+a)^n * 1000 * scale_temp) %>% dplyr::select(sta_id, sta_id_ds, lag, date, basin, flow, nse)
			discharge_calc_dat = discharge_calc_dat %>% bind_rows(discharge_calc_temp)
		}
	}
	rating_curve_us_tbl = rbind_list(rating_curve_us_tbl, rating_curve_temp)	#stores all rating curve values

}

saveRDS(rating_curve_us_tbl, paste0(dir_dat, 'rating_curve_upstream-bagmati-kosi.rda'))
saveRDS(discharge_calc_dat, paste0(dir_dat, 'discharge_calculated-bagmati-kosi.rda'))

rating_curve_mg_tbl = rating_curve_us_tbl %>% bind_rows(rating_curve_tbl)
saveRDS(rating_curve_mg_tbl, paste0(dir_dat, 'rating_curve_all-bagmati-kosi.rda'))

# test = discharge_calc_dat %>% dplyr::filter(sta_id_ds == 'ganges_discharge', sta_id == '001-lgd3beh')
# test2 = discharge_dat %>% dplyr::filter(basin == 'ganges', date >= as.Date('2015-05-15'))
# ggplot() + geom_point(data = test, aes(x = date, y = flow)) + geom_point(data = test2, aes(x = date, y = flow), colour = 'red')
