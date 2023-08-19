# rm(list=ls())
# library(ncdf4)
# source('E:/R_function/find_location.R')
# 
# lat1 = 85;lat2 = 0.1
# 
# path = 'G:/data/Grace/GRACE-FO_RL06/'
# file = nc_open(paste0(path,'CSR_GRACE_GRACE-FO_RL06_Mascons_all-corrections_v02.nc'))
# 
# lat = ncvar_get(file,'lat')
# lat_region_used = lat_region(lat ,lat1=lat1,lat2=lat2)
# indexlat1 = lat_region_used[[1]]
# indexlat2 = lat_region_used[[2]]
# latitude = lat[indexlat1:indexlat2]
# #lon_E = ncvar_get(file,'lon')
# tws_all = ncvar_get(file,'lwe_thickness')[,indexlat1:indexlat2,8:163]
# nc_close(file)
# 
# longitude = seq(-180,179.75,0.25)
# tws_180 = array(NaN,dim = dim(tws_all))
# tws_180[1:720,,] = tws_all[721:1440,,]
# tws_180[721:1440,,] = tws_all[1:720,,]
# 
# for (iregion in 1:3) {
#   
#   if(iregion==1){
#     lon1 = -20;lon2 = 28
#     lat1 = 20;lat2 = 50
#     
#   }
#   if(iregion==2){
#     lon1 = 28;lon2 = 80
#     lat1 = 20;lat2 = 50
#     
#   }
#   if(iregion==3){
#     lon1 = 80;lon2 = 105
#     lat1 = 20;lat2 = 50
#   }
#   lat_region_used = lat_region(latitude ,lat1=lat1,lat2=lat2)
#   indexlat1 = lat_region_used[[1]]
#   indexlat2 = lat_region_used[[2]]
#   lat_box = latitude[indexlat1:indexlat2]
#   ###
#   lon_region_used = lon_region(longitude ,lon1=lon1,lon2=lon2)
#   indexlon1 = lon_region_used[[1]]
#   indexlon2 = lon_region_used[[2]]
#   lon_box = longitude[indexlon1:indexlon2]
#   
#   tws_SR =round(apply(tws_180[indexlon1:indexlon2,indexlat1:indexlat2,], c(3), mean,na.rm=T) ,2) 
#   if(iregion==1){
# 
#     tws_SR1 = tws_SR
#   }
#   if(iregion==2){
#     tws_SR2 = tws_SR
#     
#   }
#   if(iregion==3){
#     tws_SR3 = tws_SR
#   }
#   
# }
# path_out = 'G:/Nature_comments/path_out/'
# write.csv(tws_SR1,
#           file = paste0(path_out,'c001_02_CSR_GRACE_GRACE-FO_RL06_Mascons_SR1.csv'))
# 
# write.csv(tws_SR2,
#           file = paste0(path_out,'c001_02_CSR_GRACE_GRACE-FO_RL06_Mascons_SR2.csv'))
# 
# write.csv(tws_SR3,
#           file = paste0(path_out,'c001_02_CSR_GRACE_GRACE-FO_RL06_Mascons_SR3.csv'))


# dim(tws_SR1_used) = c(12,nyears)
# colnames(tws_SR1_used) = seq(2003,2017,1)
# rownames(tws_SR1_used) = seq(1,12,1)
# 
# tws_SR2_used = read.csv(paste0(path_out,'c001_02_CSR_GRACE_GRACE-FO_RL06_Mascons_SR2_used.csv'))
# tws_SR2_used = as.matrix(tws_SR2_used)
# nyears = 2017-2003+1
# dim(tws_SR2_used) = c(12,nyears)
# colnames(tws_SR2_used) = seq(2003,2017,1)
# rownames(tws_SR2_used) = seq(1,12,1)
rm(list=ls())
path_out = 'I:/Nature_comments/path_out/'
nyears = 2017-2003+1

for (itype in 1:3) {
  print(itype)
  if(itype==1){
    tws_SR1_used = read.csv(paste0(path_out,'c001_02_CSR_GRACE_GRACE-FO_RL06_Mascons_SR1_used.csv'))
    tws_SR1_used = as.matrix(tws_SR1_used)
    tws_SR_all = tws_SR1_used
    
  }
  if(itype==2){
    tws_SR2_used = read.csv(paste0(path_out,'c001_02_CSR_GRACE_GRACE-FO_RL06_Mascons_SR2_used.csv'))
    tws_SR2_used = as.matrix(tws_SR2_used)
    tws_SR_all = tws_SR2_used
  }
  if(itype==3){
    tws_SR3_used = read.csv(paste0(path_out,'c001_02_CSR_GRACE_GRACE-FO_RL06_Mascons_SR3_used.csv'))
    tws_SR3_used = as.matrix(tws_SR3_used)
    tws_SR_all = tws_SR3_used
  }
  dim(tws_SR_all) = c(12,nyears)
  colnames(tws_SR_all) = seq(2003,2017,1)
  rownames(tws_SR_all) = seq(1,12,1)
  tws_SR_new = tws_SR_all
  for (imonth in 1:12) {
    index = which(tws_SR_all[imonth,]==0)
    if(length(index)>0){
      for (iii in 1:length(index)) {
        if(index[iii]!=1&index[iii]!=nyears){
          #print(paste0(imonth,'_',index[iii]))
          tws_SR_new[imonth,index[iii]]= (tws_SR_all[imonth,(index[iii]-1)]+tws_SR_all[imonth,(index[iii]+1)])/2
        }
        if(index[iii]==1){
          tws_SR_new[imonth,index[iii]]= mean(tws_SR_all[imonth,2:5])
        }
        if(index[iii]==nyears){
          tws_SR_new[imonth,index[iii]]= mean(tws_SR_all[imonth,(nyears-5):(nyears-1)])
        }
      }
    }
  }
  tws_SR_annaul = apply(tws_SR_new,c(2),mean,na.rm=T)
  dim(tws_SR_new) = c(12*nyears)
  TWS_SR_demo = ts(tws_SR_new,#[1:144],
                    start = c(2003,1),
                    frequency = 12)#$trend
  TWS_SR_trend = decompose(TWS_SR_demo)$trend
  
  if(itype==1){
    tws_SR1_annual = round(tws_SR_annaul,2)
    TWS_SR1_trend = TWS_SR_trend
  }
  if(itype==2){
    tws_SR2_annual = round(tws_SR_annaul,2)
    TWS_SR2_trend = TWS_SR_trend
  }
  if(itype==3){
    tws_SR3_annual = round(tws_SR_annaul,2)
    TWS_SR3_trend = TWS_SR_trend
  }
}

save(tws_SR1_annual,tws_SR2_annual,tws_SR3_annual,
     TWS_SR1_trend,TWS_SR2_trend,TWS_SR3_trend,
     file = paste0(path_out,'b001_01_CSR_GRACE_GRACE-FO_RL06_Mascons_SR.Rdata'))


result = load(paste0(path_out,'c001_01_CSR_GRACE_GRACE-FO_RL06_Mascons_SR.Rdata'))
##########################################
