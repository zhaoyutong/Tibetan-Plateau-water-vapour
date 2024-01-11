rm(list=ls())


path_out = 'G:/Nature_comments/path_out/'

result_hus = load(paste0(path_out,'c001_02_era5_huss_2003_2017_north_hemisphere_level_mask.Rdata'))
result_u = load(paste0(path_out,'c001_02_era5_uwind_2003_2017_north_hemisphere_level_mask.Rdata'))
nlevels = length(level_used)
nlons = length(longitude);nlats=length(latitude);nlevel = 37;nyears = 15
ntimes = nyears*12;
Q_qu_l = array(NaN,dim=dim(hus_level))
for (ilevel in 2:(nlevels-1)) {
  Q_qu_l[,,,ilevel] = hus_level[,,,ilevel]*uwind_level[,,,ilevel]*(level_used[ilevel]-level_used[ilevel-1])/9.8*(-1)
}
save(Q_qu_l,level_used,longitude,latitude,
     file = paste0(path_out,'c001_02_era5_uwind_2003_2017_north_hemisphere_U_Vertical_Integrated_Water_Vapor_Transport.Rdata'))

rm(list = ls())
path_out = 'G:/Nature_comments/path_out/'
result_hus = load(paste0(path_out,'c001_02_era5_huss_2003_2017_north_hemisphere_level_mask.Rdata'))
result_v = load(paste0(path_out,'c001_02_era5_vwind_2003_2017_north_hemisphere_level_mask.Rdata'))
nlevels = length(level_used)
Q_qv_l = array(NaN,dim=dim(hus_level))
for (ilevel in 2:(nlevels-1)) {
  Q_qv_l[,,,ilevel] = hus_level[,,,ilevel]*vwind_level[,,,ilevel]*(level_used[ilevel]-level_used[ilevel-1])/9.8*(-1)
}
save(Q_qv_l,level_used,longitude,latitude,
     file = paste0(path_out,'c001_02_era5_uwind_2003_2017_north_hemisphere_V_Vertical_Integrated_Water_Vapor_Transport.Rdata'))


######################################
path_out = 'G:/Nature_comments/path_out/'
result_U = load(paste0(path_out,'c001_02_era5_uwind_2003_2017_north_hemisphere_U_Vertical_Integrated_Water_Vapor_Transport.Rdata'))
result_V = load(paste0(path_out,'c001_02_era5_uwind_2003_2017_north_hemisphere_V_Vertical_Integrated_Water_Vapor_Transport.Rdata'))
nyears = 15;nlons = length(longitude);nlats = length(latitude)
nlevels = length(level_used)
dim(Q_qu_l) = c(nlons,nlats,nlevels,12,nyears)
dim(Q_qv_l) = c(nlons,nlats,nlevels,12,nyears)


for (itype in 1:4) {
  if(itype==1){
    smonth = 1;emonth=12
    slevel = 1;elevel = 37;
  }
  if(itype==2){
    smonth = 6;emonth=9
    slevel = 1;elevel = 37;
  }
  if(itype==3){
    smonth = 1;emonth=12
    slevel = 18;elevel = 35;
  }
  if(itype==4){
    smonth = 6;emonth=9
    slevel = 18;elevel = 35;
  }
  print(itype)
  Q_qu_annual = apply(Q_qu_l[,,slevel:elevel,smonth:emonth,],c(1,2,3,5),mean,na.rm=T)
  Q_qv_annual = apply(Q_qv_l[,,slevel:elevel,smonth:emonth,],c(1,2,3,5),mean,na.rm=T)
  #
  Q_qu_annual_sum = apply(Q_qu_annual, c(1,2,4), sum,na.rm=T)
  Q_qv_annual_sum = apply(Q_qv_annual, c(1,2,4), sum,na.rm=T)

  Q_qu_annual_sum_180  = array(NaN,dim = dim(Q_qu_annual_sum))
  Q_qv_annual_sum_180  = array(NaN,dim = dim(Q_qv_annual_sum))
  Q_qu_annual_sum_180[1:360,,] = Q_qu_annual_sum[361:720,,]
  Q_qu_annual_sum_180[361:720,,] = Q_qu_annual_sum[1:360,,]
  Q_qv_annual_sum_180[1:360,,] = Q_qv_annual_sum[361:720,,]
  Q_qv_annual_sum_180[361:720,,] = Q_qv_annual_sum[1:360,,]

  #
  times = seq(1,15,1)

  Q_qu_trend = array(NaN,dim = c(nlons,nlats))
  Q_qv_trend = array(NaN,dim = c(nlons,nlats))
  for (ilon in 1:nlons) {
    for (ilat in 1:nlats) {
      fit_u = lm(Q_qu_annual_sum_180[ilon,ilat,]~times)
      fit_v = lm(Q_qv_annual_sum_180[ilon,ilat,]~times)
      Q_qu_trend[ilon,ilat] = summary(fit_u)$coefficients[2,1]
      Q_qv_trend[ilon,ilat] = summary(fit_v)$coefficients[2,1]
    }
  }

  slevel_hpa = level_used[slevel]/100
  elevel_hpa = level_used[elevel]/100

  save(Q_qu_trend,Q_qv_trend,longitude,latitude,
       file = paste0(path_out,'c001_02_era5_QU_QV_2003_2017_',smonth,'_',emonth,'month_',slevel_hpa,'_',elevel_hpa,'hpa_north_hemisphere_vertically_integrated_water_vapor_fluxes.Rdata'))

}
#######################################################################################
path_out = 'G:/Nature_comments/path_out/'

smonth = 6;emonth=9
slevel = 18;elevel = 35;
slevel_hpa = 300;
elevel_hpa = 950
result = load(paste0(path_out,'c001_02_era5_QU_QV_2003_2017_',smonth,'_',emonth,'month_',slevel_hpa,'_',elevel_hpa,'hpa_north_hemisphere_vertically_integrated_water_vapor_fluxes.Rdata'))

nlongitudes_regrid = 50
nlatitudes_regrid = 30

library(raster)
uwind_raster1 <- raster(xmn=min(longitude),xmx=max(longitude),ymn=min(latitude),ymx=max(latitude),
                        ncol=length(longitude),nrow=length(latitude))
uwind_raster1[]=t(Q_qu_trend)
uwind_raster2 <- raster(xmn=min(longitude),xmx=max(longitude),ymn=min(latitude),ymx=max(latitude),
                        ncol=nlongitudes_regrid,nrow=nlatitudes_regrid)
uwind_regrid <- resample(uwind_raster1, uwind_raster2, method='ngb')
Q_qu_trend_regrid = t(as.matrix(uwind_regrid))


vwind_raster1 <- raster(xmn=min(longitude),xmx=max(longitude),ymn=min(latitude),ymx=max(latitude),
                        ncol=length(longitude),nrow=length(latitude))
vwind_raster1[]=t(Q_qv_trend)
vwind_raster2 <- raster(xmn=min(longitude),xmx=max(longitude),ymn=min(latitude),ymx=max(latitude),
                        ncol=nlongitudes_regrid,nrow=nlatitudes_regrid)
vwind_regrid <- resample(vwind_raster1, vwind_raster2, method='ngb')
Q_qv_trend_regrid = t(as.matrix(vwind_regrid))

latitude_used = seq(latitude[1],latitude[length(latitude)], ((latitude[1]-latitude[length(latitude)])/nlatitudes_regrid)*(-1))
longitude_used = seq(-179,180, ((max(longitude)-min(longitude))/nlongitudes_regrid))

save(latitude_used,longitude_used,
     Q_qv_trend_regrid,Q_qu_trend_regrid,
     file = paste0(path_out,'c001_02_Q_qv_trend_regrid_annual.Rdata'))