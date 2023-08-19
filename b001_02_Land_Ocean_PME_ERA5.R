
rm(list=ls())

path = 'G:/data/prec/GPCP/'

library(ncdf4)
path = 'G:/data/prec/GPCP/'
year0 = 1979;syear = 2003;eyear = 2017
stime = (syear-year0)*12+1;etime = (eyear-year0)*12+12
file = nc_open(paste0(path,'gpcc_nature_region.nc'))
lon = ncvar_get(file,'lon')
lat = ncvar_get(file,'lat')
prec_gpcp = ncvar_get(file,'precip')[,,stime:etime]
nc_close(file)
path_out = 'G:/Nature_comments/path_out/'
save(lon,lat,prec_gpcp,
     file = paste0(path_out,'000_00_gpcp_2003_2017.Rdata'))




path = 'G:/data/ERA5/'
file = nc_open(paste0(path,'EVAP_nature_region_1979_2019.nc'))
lon = ncvar_get(file,'longitude')
lat = ncvar_get(file,'latitude')
evap_era5 = ncvar_get(file,'e')[,,stime:etime]*1000#uint m to mm
nc_close(file)
path_out = 'G:/Nature_comments/path_out/'
save(lon,lat,evap_era5,
     file = paste0(path_out,'000_00_era5_evap_2003_2017.Rdata'))

path = 'G:/data/ERA5/'
file = nc_open(paste0(path,'Prec_nature_region_1979_2018.nc'))
lon = ncvar_get(file,'longitude')
lat = ncvar_get(file,'latitude')
prec_era5 = ncvar_get(file,'tp')[,,stime:etime]*1000#uint m to mm
nc_close(file)
path_out = 'G:/Nature_comments/path_out/'
save(lon,lat,prec_era5,
     file = paste0(path_out,'000_00_era5_prec_2003_2017.Rdata'))
###########################################
path = 'G:/Nature_comments/'
file = nc_open(paste0(path,'Nature_lsm_region.nc') )
lsm = ncvar_get(file,'LO_val')[,341:1]
lon_lsm = ncvar_get(file,'lon')
lat_lsm = ncvar_get(file,'lat')[341:1]
nc_close(file)
save(lsm,lat_lsm,lon_lsm,
     file = paste0(path_out,'000_00_land_ocean_mask.nc'))
#####################
rm(list=ls())
source('E:/R_function/find_location.R')
library(ncdf4)
path_out = 'I:/Nature_comments/path_out/'
nlons = 1440;nlats = 341;nyears = 2017-2003+1
syear = 2003

{
  ###read files
  ###V1 
  # result = load(paste0(path_out,'c000_00_ERA5_2003_2017_allmonth_E_minus_P.Rdata'))
  # latitude = lat_used
  # longitude = lon_used
  # evap_used[which(evap_used>0)]=0
  # P_minus_E = (prec_used + evap_used)
  ###V2
  reslt_prec = load(paste0(path_out,'000_00_era5_prec_2003_2017.Rdata'))
  
  result_evap = load(paste0(path_out,'000_00_era5_evap_2003_2017.Rdata'))
  evap_era5[which(evap_era5>0)]=0
  P_minus_E = (prec_era5 + evap_era5)
  latitude = lat
  longitude = lon
}



reult_lm = load(paste0(path_out,'000_00_land_ocean_mask.nc'))
lsm[which(lsm!=0)]=NaN
lsm[which(lsm==0)]=1

P_minus_E_mask = array(NaN,dim = dim(P_minus_E))
for (itime  in 1:(12*nyears)) {
  P_minus_E_mask[,,itime] = P_minus_E[,,itime]*lsm
}
# save(P_minus_E_mask,latitude,longitude,
#      file = paste0(path_out,'c000_00_era5_P_minus_E_landmask.nc'))




for (iregion in 1:11) {
  ###The Atlantic
  if(iregion==1){
    lon1 = -100;lon2 = -50
    lat1 = 3;lat2 = 41
    P_minus_E_used = P_minus_E_mask
    
  }
  if(iregion==2){
    lon1 = -120;lon2 = -50
    lat1 = 43;lat2 = 82
    P_minus_E_used = P_minus_E_mask
    
  }
  if(iregion==3){
    lon1 = -50;lon2 = 0
    lat1 = 0;lat2 = 43
    P_minus_E_used = P_minus_E_mask
    
  }
  if(iregion==4){
    lon1 = -50;lon2 = 25
    lat1 = 43;lat2 = 75
    P_minus_E_used = P_minus_E_mask
    
  }
  ###The Indian Ocean
  if(iregion==5){
    lon1 = 40;lon2 = 75
    lat1 = 0;lat2 = 15
    P_minus_E_used = P_minus_E_mask
    
  }
  if(iregion==6){
    lon1 = 40;lon2 = 75
    lat1 = 15;lat2 = 33
    P_minus_E_used = P_minus_E_mask
    
  }
  if(iregion==7){
    lon1 = 75;lon2 = 107
    lat1 = 0;lat2 = 15
    P_minus_E_used = P_minus_E_mask
    
  }
  if(iregion==8){
    lon1 = 75;lon2 = 107
    lat1 = 13;lat2 = 32
    P_minus_E_used = P_minus_E_mask
    
  }
  ####the land
  if(iregion==9){
    lon1 = -20;lon2 = 28
    lat1 = 20;lat2 = 50
    P_minus_E_used = P_minus_E
    
  }
  if(iregion==10){
    lon1 = 28;lon2 = 80
    lat1 = 20;lat2 = 50
    P_minus_E_used = P_minus_E
    
  }
  if(iregion==11){
    lon1 = 80;lon2 = 105
    lat1 = 20;lat2 = 50
    P_minus_E_used = P_minus_E
  }
  #############
  lat_region_used = lat_region(latitude ,lat1=lat1,lat2=lat2)
  indexlat1 = lat_region_used[[1]]
  indexlat2 = lat_region_used[[2]]
  lat_box = latitude[indexlat1:indexlat2]
  ###
  lon_region_used = lon_region(longitude ,lon1=lon1,lon2=lon2)
  indexlon1 = lon_region_used[[1]]
  indexlon2 = lon_region_used[[2]]
  lon_box = longitude[indexlon1:indexlon2]
  
  a = P_minus_E_used[indexlon1:indexlon2,indexlat1:indexlat2,]
  P_minus_E_box = apply(P_minus_E_used[indexlon1:indexlon2,indexlat1:indexlat2,], c(3), mean,na.rm=T) 
  ###
  P_minus_E_box_used = P_minus_E_box
  dim(P_minus_E_box_used) = c(12,nyears)
  P_minus_E_annual = apply(P_minus_E_box_used,c(2),mean,na.rm=T)
  ###
  #P_minus_E_box_ano = (P_minus_E_box-mean(P_minus_E_box))/sd(P_minus_E_box)
  PME_demo = ts(P_minus_E_box[1:174],
                start = c(syear,1),
                frequency = 12)#$trend  
  PME_demo_trend = decompose(PME_demo)$trend
  
  
  #############
  if(iregion==1){
    P_minus_E_NATO1 = P_minus_E_box
    PME_demo = PME_demo
    P_minus_E_annual_NATO1 = P_minus_E_annual
    lat_NATO1 = lat_box
    lon_NATO1 = lon_box
    PME_demo_trend_NATO1 = PME_demo_trend
  }
  if(iregion==2){
    P_minus_E_NATO2 = P_minus_E_box
    P_minus_E_annual_NATO2 = P_minus_E_annual
    PME_demo = PME_demo
    
    lat_NATO2 = lat_box
    lon_NATO2 = lon_box
    PME_demo_trend_NATO2 = PME_demo_trend
  }
  if(iregion==3){
    P_minus_E_NATO3 = P_minus_E_box
    P_minus_E_annual_NATO3 = P_minus_E_annual
    PME_demo = PME_demo
    
    lat_NATO3 = lat_box
    lon_NATO3 = lon_box
    PME_demo_trend_NATO3 = PME_demo_trend
  }
  if(iregion==4){
    P_minus_E_NATO4 = P_minus_E_box
    P_minus_E_annual_NATO4 = P_minus_E_annual
    PME_demo = PME_demo
    
    lat_NATO4 = lat_box
    lon_NATO4 = lon_box
    PME_demo_trend_NATO4 = PME_demo_trend
  }
  if(iregion==5){
    P_minus_E_IO1 = P_minus_E_box
    P_minus_E_annual_IO1 = P_minus_E_annual
    PME_demo = PME_demo
    
    lat_IO1 = lat_box
    lon_IO1 = lon_box
    PME_demo_trend_IO1 = PME_demo_trend
  }
  if(iregion==6){
    P_minus_E_IO2 = P_minus_E_box
    P_minus_E_annual_IO2 = P_minus_E_annual
    PME_demo = PME_demo
    
    lat_IO2 = lat_box
    lon_IO2 = lon_box
    PME_demo_trend_IO2 = PME_demo_trend
  }
  if(iregion==7){
    P_minus_E_IO3= P_minus_E_box
    P_minus_E_annual_IO3 = P_minus_E_annual
    PME_demo = PME_demo
    
    lat_IO3 = lat_box
    lon_IO3 = lon_box
    PME_demo_trend_IO3 = PME_demo_trend
  }
  if(iregion==8){
    P_minus_E_IO4= P_minus_E_box
    P_minus_E_annual_IO4 = P_minus_E_annual
    PME_demo = PME_demo
    
    lat_IO4 = lat_box
    lon_IO4 = lon_box
    PME_demo_trend_IO4 = PME_demo_trend
  }
  if(iregion==9){
    P_minus_E_SR1= P_minus_E_box
    P_minus_E_annual_SR1 = P_minus_E_annual
    PME_demo_SR1 = PME_demo
    
    lat_SR1 = lat_box
    lon_SR1 = lon_box
    PME_demo_trend_SR1 = PME_demo_trend
  }
  if(iregion==10){
    P_minus_E_SR2= P_minus_E_box
    P_minus_E_annual_SR2 = P_minus_E_annual
    PME_demo_SR2 = PME_demo
    
    lat_SR2 = lat_box
    lon_SR2 = lon_box
    PME_demo_trend_SR2 = PME_demo_trend
  }
  if(iregion==11){
    P_minus_E_SR3= P_minus_E_box
    P_minus_E_annual_SR3 = P_minus_E_annual
    PME_demo_SR3 = PME_demo
    
    lat_SR3 = lat_box
    lon_SR3 = lon_box
    PME_demo_trend_SR3 = PME_demo_trend
  }
}
save(PME_demo_trend_NATO1,PME_demo_trend_NATO2,PME_demo_trend_NATO3,PME_demo_trend_NATO4,
     PME_demo_trend_IO1,PME_demo_trend_IO2,PME_demo_trend_IO3,PME_demo_trend_IO4,
     P_minus_E_annual_NATO1,P_minus_E_annual_NATO2,P_minus_E_annual_NATO3,P_minus_E_annual_NATO4,
     P_minus_E_annual_IO1,P_minus_E_annual_IO2,P_minus_E_annual_IO3,P_minus_E_annual_IO4,
     file = paste0(path_out,'b001_02_Ocean_PME_ERA5.Rdata'))
save(P_minus_E_SR1,P_minus_E_annual_SR1,PME_demo_SR1,lat_SR1,lon_SR1,PME_demo_trend_SR1,
     P_minus_E_SR2,P_minus_E_annual_SR2,PME_demo_SR2,lat_SR2,lon_SR2,PME_demo_trend_SR2,
     P_minus_E_SR3,P_minus_E_annual_SR3,PME_demo_SR3,lat_SR3,lon_SR3,PME_demo_trend_SR3,
     file = paste0(path_out,'b001_02_Land_SR_PME_ERA5.Rdata'))