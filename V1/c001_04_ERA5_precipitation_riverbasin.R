rm(list=ls())
library(ncdf4)
source('E:/R_function/find_location.R')
source('E:/R_function/area_mask.R')
source('E:/R_function/area_average.R')


year0 = 1979
syear = 2003;eyear = 2017;years = seq(syear,eyear,1);nyears = length(years)
smonth = 1;emonth = 12;nmonth = emonth - smonth+1
stime = (syear-year0)*12+1;etime =  (eyear-year0+1)*12;
lat1_used = 42; lat2_used = 25
lon1_used = 68; lon2_used = 107
#
#
#
#

path = 'I:/data/ERA5/'
file = nc_open(paste0(path,'Prec_nature_region_1979_2018.nc'))
lat = ncvar_get(file,'latitude')
lat_region_used = lat_region(lat ,lat1=lat1_used,lat2=lat2_used)
indexlat1 = lat_region_used[[1]]
indexlat2 = lat_region_used[[2]]
latitude = lat[indexlat1:indexlat2];nlats = length(latitude)
lon = ncvar_get(file,'longitude')
lon_region_used = lon_region(lon,lon1=lon1_used,lon2=lon2_used)
indexlon1 = lon_region_used[[1]]
indexlon2 = lon_region_used[[2]]
longitude = lon[indexlon1:indexlon2];nlons = length(longitude)
prec_all_en = ncvar_get(file,'tp')[indexlon1:indexlon2,indexlat1:indexlat2,stime:etime]*1000#m to mm
nc_close(file)
###

###
path_gis = "E:/GIS/TibetanPlateau/TP_basin_boundary/"
path_gis = "E:/GIS/TibetanPlateau/TP_basin_boundary/"
riverbasin = c('TibetanPlateau','TP_Endorheic','tp_upstream_basin',
               'Yellow','Yangtze','Mekong','Salween',
               'Ganges','Brahmaputra','Indus')
nregions = length(riverbasin)

for (iregion in 1:nregions) {
  
  used_shp = rgdal::readOGR(paste(path_gis,riverbasin[iregion],".shp",sep=""))
  
  prec_tibet_ave = area_mask(longitude,latitude,used_shp)
  prec_region_mask = array(NaN,dim = dim(prec_all_en))
  ntimes = dim(prec_all_en)[3]
  for (iyear in 1:ntimes) {
    prec_region_mask[,,iyear] = prec_all_en[,,iyear]*prec_tibet_ave
  }
  prec_region_ave = apply(prec_region_mask,c(3),mean,na.rm=T)
  path_out = 'I:/Nature_comments/path_out/'
  save(prec_region_ave,
            file = paste0(path_out,'c001_02_ERA5_precipitation_',riverbasin[iregion],'.Rdata'))
  
  
}
