rm(list = ls())
library(ncdf4)
source('E:/Tibet_mositure/function/find_location.R')
source('E:/Tibet_mositure/function/area_average.R')


#path_out = 'E:/Tibet_mositure/path_out/'
path_out = 'G:/Nature_comments/path_out/'

syear = 2003;eyear = 2017;years = seq(syear,eyear,1);nyears = length(years)
smonth = 1;emonth = 12;nmonth = emonth - smonth+1
#path_gis = "E:/GIS/TibetanPlateau/TP_basin_boundary/"

path_gis = "E:/GIS/TP_runoff_basin/"
region_shp = rgdal::readOGR(paste(path_gis,"TibetanPlateau.shp",sep=""))
lat1_used = 42; lat2_used = 25
lon1_used = 68; lon2_used = 107

{
  
  path = 'G:/data/prec/GPCP/'
  year0 = 1979
  stime = (syear-year0)*12+1;etime = (eyear-year0)*12+12
  file_gpcp = nc_open(paste0(path,'precip.mon.mean.nc'))
  latitude = ncvar_get(file_gpcp,'lat');
  longitude = ncvar_get(file_gpcp,'lon');
  lat_region_used = lat_region(latitude,lat1=lat1_used,lat2=lat2_used)
  lon_region_used = lon_region(longitude,lon1=lon1_used,lon2=lon2_used)
  lat_gpcp = latitude[lat_region_used[[1]]:lat_region_used[[2]]];nlats = length(lat_gpcp) 
  lon_gpcp = longitude[lon_region_used[[1]]:lon_region_used[[2]]];nlons = length(lon_gpcp)
  gpcp_pre = ncvar_get(file_gpcp,'precip')[lon_region_used[[1]]:lon_region_used[[2]],lat_region_used[[1]]:lat_region_used[[2]],stime:etime];
  nc_close(file_gpcp)
 
  gpcp_tibet_weights = area_average(lon_gpcp,lat_gpcp,region_shp)
  gpcp_yearly_weights = array(NaN,dim = c(nlons,nlats,(nyears*12)))
  for (itime in 1:(nyears*12)) {
    gpcp_yearly_weights[,,itime] = gpcp_pre[,,itime]*gpcp_tibet_weights*30 #day to month
  }
  dim(gpcp_yearly_weights) = c(nlons,nlats,12,nyears)
  ave_tp = apply(gpcp_yearly_weights,c(3,4),mean,na.rm=T)
  rownames(ave_tp) = month.name
  colnames(ave_tp) = seq(syear,eyear,1)
  
  monthly_cli =  apply(ave_tp,c(1),mean,na.rm=T)
  yearly_cli = apply(ave_tp,c(2),sum,na.rm=T)
  names(monthly_cli) = month.abb
  years_cli = sum(monthly_cli)
}
save(ave_tp,monthly_cli,ave_tp,years_cli,
     file = paste0(path_out,'a002_01_gpcp_2003_2017_new_contribution.Rdata'))

result = load(paste(path_out,'a002_01_gpcp_2003_2017_new_contribution.Rdata'))
path =  'I:/Nature_comments/data_fromZhang/'
nyears = 2017-2003+1

name_file = c('SupplementFig2','SupplementFig3')
for (ifile in 1:2) {
  print(name_file[ifile])
  if(ifile==1){
    region_name = c('Asia (AS)', 'Indian Ocean (IO)', 'North Africa (NAF)', 'North Atlantic (NATO)',
                    'Arctic Ocean (AO)', 'Black Sea (BS)', 'Caspian Sea (CS)', 'Europe (EU)',  'Mediterranean Sea (MS)',
                    'North America (NAM)',  'Pacific Ocean (PO)', 'Red Sea (RS)', 'Tibet Plateau (TP)')
    index_number = c(9,5,12,2,
                     1,3,4,11,6,
                     13,7,8,10)
    file = read.csv(paste0(path,'Figure1_contr_in_source_variance_new.csv'))  #supplement fig.2
  }
  if(ifile==2){
    region_name = c('Asia (AS)','North Atlantic (NATO)',  'North Africa (NAF)', 'Indian Ocean (IO)',
                    'Arctic Ocean (AO)', 'Black Sea (BS)', 'Caspian Sea (CS)', 'Europe (EU)',  'Mediterranean Sea (MS)',
                    'North America (NAM)',  'Pacific Ocean (PO)', 'Red Sea (RS)', 'Tibet Plateau (TP)')
    
    index_number = c(9,12,2,5,
                     1,3,4,11,6,
                     13,7,8,10)
    file = read.csv(paste0(path,'Figure1_contr_rate_noloss.csv')) #supplement fig.3
  }

  
  abs_allregion = array(NaN,dim = c(13,(12*nyears)))
  con_allregion_yearly = array(NaN,dim = c(13,(12*nyears)))
  for (iregion in 1:13) {
    index = index_number[iregion]
    resource_con = round(as.numeric(file[index,2:181]) ,2) 
    dim(resource_con) = c(12,nyears)
    abs_resourece_monthly = array(NaN,dim = c(12,nyears))
    con_resourece_yearly = array(NaN,dim = c(12,nyears))
    for (iyear in 1:nyears) {
      for (imonth in 1:12) {
        #abs_resourece_monthly[imonth,iyear] = resource_con[imonth,iyear]*ave_tp[imonth,iyear]/100 # % to mm
        abs_resourece_monthly[imonth,iyear] = resource_con[imonth,iyear]*monthly_cli[imonth]/100 # % to mm
        con_resourece_yearly[imonth,iyear] = abs_resourece_monthly[imonth,iyear]/yearly_cli[iyear]*100 #devide the yearly cli
      }
    }
    dim(abs_resourece_monthly) = c(12*nyears)
    dim(con_resourece_yearly) = c(12*nyears)
    abs_allregion[iregion,] = abs_resourece_monthly
    con_allregion_yearly[iregion,] = con_resourece_yearly
  }
  
  
  ###
  month <- seq(1,12,1)
  time <- array(NaN,dim = c(length(month),nyears))
  for (iyear in syear:eyear) {
    for (imonth in 1:12) {
      if(imonth < 10){imonth_used = paste("0",imonth,sep="")}
      else{
        imonth_used = imonth
      }
      time[imonth,(iyear-syear+1)] = as.numeric(paste(iyear,imonth_used,sep = ""))
    }
  }
  dim(time) = c(12*nyears)
  
  
  
  rownames(abs_allregion) = region_name
  colnames(abs_allregion) = time

  
  rownames(con_allregion_yearly) = region_name
  colnames(con_allregion_yearly) = time
  
  if(ifile==1){
    write.csv(abs_allregion,
              file = paste0(path_out,'a002_01_rewrite_supplementfig2_abs_Monthly_rates_accounting_for_on-route_changes.csv'))
    
    
    write.csv(con_allregion_yearly,
              file = paste0(path_out,'a002_01_rewrite_supplementfig2_contribution_yearly_rates_accounting_for_on-route_changes.csv'))
              
  }
  if(ifile==2){
    write.csv(abs_allregion,
              file = paste0(path_out,'a002_01_rewrite_supplementfig3_abs_Monthly_rates_accounting_for_disregarding_on-route_changes.csv'))
    
    
    write.csv(con_allregion_yearly,
              file = paste0(path_out,'a002_01_rewrite_supplementfig3_contribution_yearly_rates_accounting_for_disregarding_on-route_changes.csv'))
    
  }
}





