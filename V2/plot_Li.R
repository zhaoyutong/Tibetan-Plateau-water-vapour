rm(list=ls())
library(sf)
library(raster)

path = '/Users/lezi/Desktop/data/'
data_shp = read.csv(paste0(path,'shp_con.csv'),header = T)

data_box = read.csv(paste0(path,'box_con.csv'),header = T)

path_pic = path

pic_width = 16;pic_height=9;
tiff(
  filename = paste(path_pic,"c001_01_Li.tiff", sep = ""),
  width = pic_width,
  height = pic_height,
  units = "cm",
  pointsize = 8,
  res = 300,
  family = 'Helvetica-Narrow'
)

par( oma = c(1, 3, 1, 1),mar = c(0, 0, 0, 0))
position11 = cbind(0.02,0.45,0.2,0.98)
position13 = cbind( 0.55,0.98, 0.2, 0.98)
position1 = rbind(position11,position13)
title_name = c(expression(paste('The moisture from the Indian Ocean (mm year'^{-1},')',sep=' ')  ),
              expression(paste('The moisture from the Western Ocean (mm year'^{-1},')',sep=' ')  ))          
for (ipic in 1:2) {
  if(ipic==1){
    par(fig=position1[ipic,], new=F)
    plot_data_shp = as.matrix(data_shp[,2]) 
    plot_data_box = as.matrix(data_box[,2]) 
    
    ymin = 0;ymax =300
    ylim  = seq(0,300,100)
  }else{
    par(fig=position1[ipic,], new=T)
    plot_data_shp = as.matrix(data_shp[,3]) 
    plot_data_box = as.matrix(data_box[,3]) 
    
    ymin = 0;ymax =300
    ylim  = seq(0,300,100)
  }
  plot(plot_data_shp,type="n",axes = F,xlab="",ylab="",xaxs="i",yaxs="i",
       ylim=c(ymin,ymax),lwd=2,xlim=c(0.5,4.5))
  
  
  x_loc = seq(1,4,1)-0.2
  rect_w = 0.15
  col_box = c('#EF5350','#64B5F6','#AB47BC','gray50')
  rect(x_loc-rect_w,0,x_loc+rect_w,plot_data_shp,col=col_box,border = col_box)
  
  x_loc = seq(1,4,1)+0.2
  rect_w = 0.15
  rect(x_loc-rect_w,rep(0,4),x_loc+rect_w,plot_data_box,
       col=col_box,border = col_box)
  
  for (iii in 1:4) {
    rect((x_loc[iii] - rect_w), 0, (x_loc[iii] + rect_w), plot_data_box[iii], 
         col = 'white', border = "transparent",
         density = 30, angle = 135)
  }

  #, 
       
  
  box()
  
  xlim = seq(1,4,1)
  xlim_text = c("ERA-Interim",'JRA-55','MERRA-2',"Average")
  axis(1,xlim,tck=-0.02,cex=10,labels = F,line=0)
  mtext(side=1,at=xlim,xlim_text,line=0.35,las=1)
  
  axis(side=2, at=ylim,col="black",las=1,col.axis="black",tck=-0.02,labels = F,line=0)  ## las=1 makes horizontal labels
  mtext(side=2,at=ylim,ylim,line=0.6,las=1)
  par(usr=c(0,1,0,1))
  mtext(side=2,at=0.5,title_name[ipic],
        line=2.5,las=0)
  text(0.02,0.95,letters[ipic],adj=0,font=2)
  

  
}

#########################
path_gis = '/Users/lezi/Downloads/GIS/TP_runoff_basin/'
TP_shp = st_read(paste(path_gis,"TibetanPlateau.shp",sep=""))
par(fig=c(0.75,0.98,0.65,0.98), new=T)
plot(seq(1,9,1),type="n",axes = F,xlab="",ylab="",xaxs="i",yaxs="i",
     ylim=c(0,1),xlim=c(0,1))
library(maps)
lon = seq(50,120,1)
lat = seq(5,55,1)
par(usr=c(min(lon),max(lon),min(lat),max(lat)))

map("world",interior = F,add=T, col = "gray90",border="gray90",fill = T,
    ylim = range(lat),xlim = range(lon))
#plot(region_shp,add=T, col = "gray80",border="gray80")
col_TP = 'gray60'
rect(66,24,107,41,
     col = 'white', border = "transparent",
     density = 30, angle = 135)
#rect(66,24,107,41,border = 'black',lty=1,lwd=2)

plot(TP_shp,add=T,axes = F,xlab="",ylab="",
     col = col_TP,border=col_TP,lwd=1)
par(fig=position1[2,], new=T)
plot(plot_data_shp,type="n",axes = F,xlab="",ylab="",xaxs="i",yaxs="i",
     ylim=c(0,1),lwd=2,xlim=c(0,1))
box()

par(usr=c(0,1,0,1))

  x1 = 0.5;x2 = 0.6;
  rect(x1,0.4,x2,0.45,col=col_box[4],border = "transparent")
  rect(x1,0.4,x2,0.45,
       col = 'white', border = "transparent",
       density = 30, angle = 135)
  rect(x1,0.5,x2,0.55,col=col_box[4],border = "transparent")
  
  text(x2+0.03,0.425,adj=0,'TPBoundary_V2')
  text(x2+0.03,0.525,adj=0,'TPBoundary_V1')
  


dev.off()


