rm(list=ls())
library(sf)
library(raster)
library(ncdf4)
source('.../Downloads/R_function/area_average.R')
path = '.../Desktop/data/'

files = list.files(path,'nc')
nfiles = length(files)


path_gis = '.../Downloads/GIS/TP_runoff_basin/'
upper_shp = st_read(paste(path_gis,"TibetanPlateau.shp",sep=""))
ave_mean <- function(data,shp_mask){
  nlons = dim(data)[1]
  nlats = dim(data)[2]
  ntimes = dim(data)[3]
  ####
  data_shp = array(NaN,dim = dim(data))
  for (itime in 1:ntimes) {
    data_shp[,,itime] = data[,,itime]*shp_mask
  }
  nyears = ntimes/12
  region_ave_annual = apply(data_shp,c(3),mean,na.rm=T)
  dim(region_ave_annual) = c(12,nyears)
  region_ave = sum(apply(region_ave_annual,c(1),mean,na.rm=T))
  return(region_ave)
}

mositure_resource_abs = array(NaN,dim = c(3,2))
mositure_resource_rel = array(NaN,dim = c(3,2))

for (ifile in 1:nfiles) {
  if(ifile==1|ifile==2){
    year0 = 1979;
  }
  if(ifile==3){
    year0 = 1980;
  }
  stime = (2003-year0)*12+1
  etime = (2015-year0)*12+12
  file = nc_open(paste0(path,files[ifile]))
  lon = ncvar_get(file,'longitude') 
  lat = ncvar_get(file,'latitude') 
  upper_mask = area_average(lon,lat,upper_shp)
  WO_abs = ncvar_get(file,'western_oceans_contribution_absolute')[,,stime:etime]
  IO_abs = ncvar_get(file,'indian_ocean_contribution_absolute')[,,stime:etime]
  WO_rel = ncvar_get(file,'western_oceans_contribution_relative')[,,stime:etime]
  IO_rel = ncvar_get(file,'indian_ocean_contribution_relative')[,,stime:etime]
  
  
  mositure_resource_abs[ifile,2] = ave_mean(WO_abs,upper_mask)
  mositure_resource_abs[ifile,1] = ave_mean(IO_abs,upper_mask)
  mositure_resource_rel[ifile,2] = ave_mean(WO_rel,upper_mask)
  mositure_resource_rel[ifile,1] = ave_mean(IO_rel,upper_mask)
  nc_close(file)
  
}

MME = apply(mositure_resource_abs, c(2), mean,na.rm=T)
all_data = rbind(mositure_resource_abs,MME)
write.csv(all_data,
          file = paste0(path,'shp_con_V.csv'))
