rm(list=ls())
fit_lm <- function(data){
  nlons = dim(data)[1]
  nlats = dim(data)[2]
  nyears = dim(data)[3]
  
  years = seq(1,nyears,1)
  

  data_trend = array(NaN,dim = c(nlons,nlats))
  for (ilon in 1:nlons) {
    for (ilat in 1:nlats) {
      fit = summary(lm(E_minus_P_annual[ilon,ilat,]~years)) 
      data_trend[ilon,ilat] = fit$coefficients[2,1]
    }
  }
  return(data_trend)
}

path_out = 'E:/Nature_comments/path_out/'
result_prec = load(paste0(path_out,'000_00_era5_evap_2003_2017.Rdata'))

result_evap = load(paste0(path_out,'000_00_era5_prec_2003_2017.Rdata'))

evap_era5[which(evap_era5>0)]=0
evap_era5_used = evap_era5*(-1)
E_minus_P = evap_era5_used - prec_era5

# save(E_minus_P,lon,lat,
#      file = paste0(path_out,'000_03_North_hemisphere_EMP.Rdata'))
####################################################################
nlons = length(lon)
nlats = length(lat)
nyears = 2017-2003+1
dim(E_minus_P) = c(nlons,nlats,12,nyears)
E_minus_P_annual = apply(E_minus_P,c(1,2,4),mean,na.rm=T)
E_minus_P_trend = fit_lm(E_minus_P_annual)


E_minus_P_JJAS = apply(E_minus_P[,,6:9,],c(1,2,4),mean,na.rm=T)
E_minus_P_JJAS_trend = fit_lm(E_minus_P_JJAS)


save(E_minus_P_trend,
     E_minus_P_JJAS_trend,
     lon,lat,
     file = paste0(path_out,'c001_01_North_hemisphere_EMP_trend.Rdata'))