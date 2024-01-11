source('E:/R_function/find_location.R')
source('E:/R_function/area_mask.R')
source('E:/R_function/area_average.R')
library(raster)
path_out = 'G:/Nature_comments/path_out/'
result_land = load(paste0(path_out,'b001_02_Land_SR_PME_ERA5.Rdata')) 
result_ocean = load(paste0(path_out,'b001_02_Ocean_PME.Rdata')) 

for (iregion in 1:3) {
  print(iregion)
  if(iregion==1){
    lon1 = -50;lon2 = 0
    lat1 = 0;lat2 = 43
    EMP_used = P_minus_E_ocean
  }
  if(iregion==2){
    lon1 = 50;lon2 = 90
    lat1 = 2;lat2 = 30
    EMP_used = P_minus_E_ocean
  }
  if(iregion==3){
    lat1 = 42; lat2 = 25
    lon1 = 68; lon2 = 107
    EMP_used = P_minus_E_land
  }
  lat_region_used = lat_region(lat ,lat1=lat1,lat2=lat2)
  indexlat1 = lat_region_used[[1]]
  indexlat2 = lat_region_used[[2]]
  lat_box = lat[indexlat1:indexlat2]
  ###
  lon_region_used = lon_region(lon ,lon1=lon1,lon2=lon2)
  indexlon1 = lon_region_used[[1]]
  indexlon2 = lon_region_used[[2]]
  lon_box = lon[indexlon1:indexlon2]
  
  EMP_box = EMP_used[indexlon1:indexlon2,indexlat1:indexlat2,]
  if(iregion==3){
    path_gis = "E:/GIS/TP_runoff_basin/"
    used_shp = rgdal::readOGR(paste(path_gis,"TibetanPlateau.shp",sep=""))
    
    tibet_ave = area_average(lon_box,lat_box,used_shp)
    region_mask = array(NaN,dim = dim(EMP_box))
    ntimes = dim(EMP_box)[3]
    EMP_box_tp = array(NaN,dim = dim(EMP_box))
    for (itime in 1:ntimes) {
      EMP_box_tp[,,itime] = EMP_box[,,itime]*tibet_ave
    }
    
  }
  ####
  if(iregion==1){
    EMP_NA = EMP_box
  }
  if(iregion==2){
    EMP_IO = EMP_box
  }
  if(iregion==3){
    EMP_tp = EMP_box_tp
  }
}

save(EMP_NA,EMP_IO,EMP_tp,
     file = paste0(path_out,'c001_03_TP_Indianociean_northatlantic_EMP.Rdata'))

