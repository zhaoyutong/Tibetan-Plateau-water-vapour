rm(list=ls())
library(ncdf4)
path = '/Users/lezi/Desktop/data/'#'D:/Nature_comments_20240303/data/Ocean_moisture/'
files = list.files(path,'nc')
nfiles = length(files)

for (ifile in 1:3) {
  if(ifile==1|ifile==2){
    year0 = 1979;
  }
  if(ifile==3){
    year0 = 1980;
  }
  stime = (2003-year0)*12+1
  etime = (2015-year0)*12+12
  nyears = 2015-2003+1
  file = nc_open(paste0(path,files[ifile]))
  Wo_mm_annual = apply(ncvar_get(file,'western_oceans_contribution_absolute')[,,stime:stime],c(3),mean,na.rm=T)
  IO_mm_annual = apply(ncvar_get(file,'indian_ocean_contribution_absolute')[,,stime:stime],c(3),mean,na.rm=T)
  Wo_percent_annual = apply(ncvar_get(file,'western_oceans_contribution_relative')[,,stime:stime],c(3),mean,na.rm=T)
  Io_percent_annual = apply(ncvar_get(file,'indian_ocean_contribution_relative')[,,stime:stime],c(3),mean,na.rm=T)
  lon = ncvar_get(file,'longitude')
  lat = ncvar_get(file,'latitude')
  nc_close(file)
  ####
  dim(Wo_mm_annual) = c(12,nyears)
  dim(IO_mm_annual) = c(12,nyears)
  dim(Wo_percent_annual) = c(12,nyears)
  dim(Io_percent_annual) = c(12,nyears)

  Wo_mm = apply(Wo_mm_annual,c(1),mean,na.rm=T)
  Io_mm = apply(IO_mm_annual,c(1),mean,na.rm=T)


  if(ifile==1){
    ERAI_Wo_mm = Wo_mm
    ERAI_Io_mm = Io_mm

  }
  if(ifile==2){
    JRA55_Wo_mm = Wo_mm
    JRA55_Io_mm = Io_mm

  }
  if(ifile==3){
    MERRA2_Wo_mm = Wo_mm
    MERRA2_Io_mm = Io_mm

  }

}

cal_ave <- function(x,y,z){
  data_M = (x+y+z)/3
  return(data_M)
}

Wo_MME_mm = cal_ave(ERAI_Wo_mm,JRA55_Wo_mm,MERRA2_Wo_mm)
Io_MME_mm = cal_ave(ERAI_Io_mm,JRA55_Io_mm,MERRA2_Io_mm)

absoute_all = cbind(Io_MME_mm,Wo_MME_mm)

WO_mm = c(sum(ERAI_Wo_mm),sum(JRA55_Wo_mm),sum(MERRA2_Wo_mm),sum(Wo_MME_mm))
IO_mm = c(sum(ERAI_Io_mm),sum(JRA55_Io_mm),sum(MERRA2_Io_mm),sum(Io_MME_mm))
abs_con = cbind(IO_mm,WO_mm)
write.csv(abs_con,
          file = paste0(path,'box_con.csv'))

