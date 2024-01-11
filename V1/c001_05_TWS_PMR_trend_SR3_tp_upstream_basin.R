
normalized_var <- function(x){
  var_nor = ((x-mean(x)))/sd(x)
  return(var_nor)
}

linear_reg_cof <- function(var,times){
  fit = summary(lm(var~times))
  cof = fit$coefficients[2,1]
  return(cof)
}

linear_reg_pvalue <- function(var,times){
  fit = summary(lm(var~times))
  cof = fit$coefficients[2,4]
  return(cof)
}


path_out = 'G:/Nature_comments/path_out/'

for (itype in 1:2) {
  if(itype==1){
    
    result_P = load( paste0(path_out,'c001_02_ERA5_precipitation_tp_upstream_basin.Rdata'))
    
    dim(prec_region_ave) = c(12,nyears)
    prec_yearly = apply(prec_region_ave,c(2),mean,na.rm=T)
    
    result_e = load(paste0(path_out,'c001_02_ERA5_evapipitation_tp_upstream_basin.Rdata'))
    evap_region_ave[which(evap_region_ave>0)]=0
    dim(evap_region_ave) = c(12,nyears)
    evap_yearly = apply(evap_region_ave,c(2),mean,na.rm=T)
    
    result_runoff = load( paste0(path_out,'c001_02_ERA5_runoff_tp_upstream_basin.Rdata'))
    dim(sro_region_ave) = c(12,nyears)
    runoff_yearly = apply(sro_region_ave,c(2),mean,na.rm=T)
    
    
    result_tws = load(paste0(path_out,'c001_02_c001_02_CSR_GRACE_GRACE-FO_RL06_Mascons_tibet_riverbasin.Rdata'))
    tws_used = tws_SR_annaul[,3]
    dim(tws_used)  =c(12,nyears)
    TWS_yearly = apply(tws_used,c(2),mean,na.rm=T)
    
  }
  if(itype==2){
    result_P_E_SR  = load(paste0(path_out,'c001_08_Land_SR_P_minus_E_R_ERA5.Rdata'))
    prec_yearly = P_annual_SR3
    evap_yearly = E_annual_SR3
    runoff_yearly = R_annual_SR3
    ###
    result = load(paste0(path_out,'c001_02_CSR_GRACE_GRACE-FO_RL06_Mascons_SR.Rdata'))
    dim(TWS_SR3_trend)  =c(12,nyears)
    TWS_yearly = apply(TWS_SR3_trend,c(2),mean,na.rm=T)
  }

  ###
  PMEMR = (prec_yearly+evap_yearly-runoff_yearly)
  PME = (prec_yearly+evap_yearly)
  
  PME_ano = normalized_var(PME)
  PMEMR_ano = normalized_var(PMEMR)
  TWS_ano = TWS_yearly/sd(TWS_yearly)
  
  years = seq(1,nyears,1)
  PME_trend = linear_reg_cof(PME_ano,years)
  PMEMR_trend = linear_reg_cof(PMEMR_ano,years)
  TWS_trend = linear_reg_cof(TWS_ano,years)
  
  PME_pvalue = linear_reg_pvalue(PME_ano,years)
  PMEMR_pvalue = linear_reg_pvalue(PMEMR_ano,years)
  TWS_pvalue = linear_reg_pvalue(TWS_ano,years)
  
  if(itype==1){
    TP_PME_trend = PME_trend
    TP_PMEMR_trend = PMEMR_trend
    TP_TWS_trend = TWS_trend
    TP_PME_pvalue = PME_pvalue
    TP_PMEMR_pvalue = PMEMR_pvalue
    TP_TWS_pvalue = TWS_pvalue
    
  }
  if(itype==2){
    SR3_PME_trend = PME_trend
    SR3_PMEMR_trend = PMEMR_trend
    SR3_TWS_trend = TWS_trend
    SR3_PME_pvalue = PME_pvalue
    SR3_PMEMR_pvalue = PMEMR_pvalue
    SR3_TWS_pvalue = TWS_pvalue
  }
  
  
}

save(TP_PME_trend,TP_PMEMR_trend,TP_TWS_trend,TP_PME_pvalue,TP_PMEMR_pvalue,TP_TWS_pvalue,
     SR3_PME_trend,SR3_PMEMR_trend,SR3_TWS_trend,SR3_PME_pvalue,SR3_PMEMR_pvalue,SR3_TWS_pvalue,
     file = paste0(path_out,'c001_05_TWS_PMR_trend_SR3_tp_upstream_basin.Rdata'))


c006_03_TWS_PMR_trend_SR3_TP_upstream_basin.Rdata
