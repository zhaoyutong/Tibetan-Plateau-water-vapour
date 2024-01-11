
path_out = 'D:/Nature_comments/path_out/'
result = load(paste0(path_out,'c006_03_TWS_PMR_trend_SR3_TP_upstream_basin.Rdata'))

path_pic = 'D:/Nature_comments/response/pics/'
pic_width = 22;pic_height=16;
tiff(
  filename = paste(path_pic,"a003_01_plot_runoff.tiff", sep = ""),
  width = pic_width,
  height = pic_height,
  units = "cm",
  pointsize = 9,
  res = 300,
  family = 'Helvetica-Narrow'
)


par( oma = c(1, 3, 1, 1),mar = c(0, 0, 0, 0))
x1 = 0.05 ;x2 = 0.4;
y_heigt = 0.4;ymax = 0.98
position12 = cbind(x1,x2,ymax-y_heigt,ymax)
position11 = cbind(x1,0.33,ymax-y_heigt/2,ymax)
position1 = rbind(position11,position12)

for (ipic in 1:2) {
  if(ipic==1){
    par(fig=position1[ipic,], new=F)
  }else{
    par(fig=position1[ipic,], new=T)
  }

 
  if(ipic==1){
    
    plot(seq(1,9,1),xlim=c(0.7,2.3),ylim=c(-30,30),type="n",
         axes = F,xlab="",ylab="",xaxs="i",yaxs="i")
    library(maps)
    lon = seq(63,150,1)
    lat = seq(12,52,1)
    par(usr=c(min(lon),max(lon),min(lat),max(lat)))
    map("world",interior = F,add=T, col = "gray95",border="gray95",fill = T,
        ylim = range(lat),xlim = range(lon))
    path_gis= 'D:/GIS/TP_basin_boundary/'
    TP_shp = rgdal::readOGR(paste(path_gis,"TibetanPlateau.shp",sep=""))
    #plot(region_shp,add=T, col = "gray80",border="gray80")
    col_TP = '#D6D8EE'
    plot(TP_shp,add=T,axes = F,xlab="",ylab="",
         col = col_TP,border=col_TP,lwd=1)

    
    rect(80,20,105,50,border = 'gray87',lty=1,lwd=2)

    
    region_shp = rgdal::readOGR(paste(path_gis,"TP_Endorheic.shp",sep=""))
    #plot(region_shp,add=T, col = "gray80",border="gray80")
    plot(region_shp,add=T,axes = F,xlab="",ylab="",col = "gray95",border="gray95",lwd=1)
    plot(TP_shp,add=T,axes = F,xlab="",ylab="",
         border="gray30",lwd=1)
    par(usr=c(0,1,0,1))
    text(0.21,0.48,'Exorheic',adj=0,col='#506DAE')
    text(0.21,0.87,'SR3',adj=0,col='gray50')
    #SR3:80-105E;20-50N
  }
  if(ipic==2){
    ###
    plot(seq(1,9,1),xlim=c(0.7,2.3),ylim=c(-30,30),type="n",
         axes = F,xlab="",ylab="",xaxs="i",yaxs="i")
    TP_trend = c(TP_TWS_trend,TP_PMEMR_trend,TP_PME_trend)*12*10
    SR3_trend = c(SR3_TWS_trend,SR3_PMEMR_trend,SR3_PME_trend)*12*10
    TP_pvalue = c(TP_TWS_pvalue,TP_PMEMR_pvalue,TP_PME_pvalue)
    SR3_pvalue = c(SR3_TWS_pvalue,SR3_PMEMR_pvalue,SR3_PME_pvalue)
    # box1 = c(TP_TWS_trend,SR3_TWS_trend)
    # box2 = c(TP_PME_trend,SR3_PME_trend)
    # box3 = c(SR3_PMEMR_trend,SR3_PMEMR_trend)
    
    rect_width=0.06
    rect_loc_w=0.08
    col_used = c(rgb(50/255,113/255,105/255),
                 rgb(141/255,201/255,194/255),
                 rgb(77/255,151/255,198/255))
    
    for (iii in 1:2) {
      if(iii==1){
        rect_loc = c((1-rect_loc_w*2),1,(1+rect_loc_w*2))
        rect_var = TP_trend
        rect_pvalue = TP_pvalue
      }
      if(iii == 2){
        rect_loc = c((2-rect_loc_w*2),2,(2+rect_loc_w*2))
        rect_var = SR3_trend
        rect_pvalue = SR3_pvalue
      }
      for (irect in 1:3) {
        if(iii==1&irect==3){
          dis_rect = -2
        }else{
          dis_rect = 2.5
        }
        rect(rect_loc[irect] - rect_width, 0, 
             rect_loc[irect] + rect_width,
             rect_var[irect], 
             col = col_used[irect], 
             border = col_used[irect])
        
        if(rect_pvalue[irect]<0.05&rect_pvalue[irect]>0.01){
          text(rect_loc[irect],(rect_var[irect]-dis_rect),'*',adj=0.5) 
         
        }
        if(rect_pvalue[irect]<0.01){
          text(rect_loc[irect],(rect_var[irect]-dis_rect),'* *',adj=0.5) 
        }
        
        text(rect_loc[irect],(rect_var[irect]+dis_rect),round(rect_var[irect],0) ,adj=0.5)
      }
      
      # for (irect in 1:3) {
      #   rect(rect_loc[irect] - rect_width, 0, 
      #        rect_loc[irect] + rect_width, 
      #        rect_var[irect], 
      #        #density = 20, angle = 45, 
      #        col = "white", border = "white")
      #   }
    }
    
    abline(h=0,lwd=1)
    box()
    xlim = seq(1,2,1)
    xlim_text = c(expression('Exorheic basin in TP'^{''}),
                  expression('SR3 region in Zhang'^{'1'}) )
    axis(1,xlim,tck=-0.02,cex=10,labels = F,line=0)
    #text(xlim,-0.37,xlim_text,adj = 1,xpd=NA,srt=10)
    mtext(side=1,at=xlim,xlim_text,line=c(0.3,0.5),las=1,adj=0.5)
    
    ylim = seq(-30,30,10)
    ylim_text = ylim
    axis(side=2, at=ylim,col="black",las=1,col.axis="black",tck=-0.015,labels = F,line=0)  ## las=1 makes horizontal labels
    mtext(side=2,at=ylim,ylim_text,line=0.5,las=1)
    
    
    
    par(usr=c(0,1,0,1))
    mtext(side=2,at=0.5,expression(paste('Trend (mm decades'^{-1},')',sep=''))  ,line=2,las=0)

    x1 = 0.75;x2 = 0.85
    y1 = 0.76;y2 = 0.8;width_rect = 0.07

    # rect(x1,y1,x2,y2,col=col_used[1],border =col_used[1])
    # rect(x1,y1+width_rect,x2,y2+width_rect,border=col_used[2])
    # rect(x1,y1+width_rect*2,x2,y2+width_rect*2,border=col_used[3])

    for (irect in 1:3) {
      rect(x1,y1+width_rect*(irect-1),x2,y2+width_rect*(irect-1),
           col=col_used[irect],border =col_used[irect])
      }



    x3 = 0.87;y3 = 0.78
    text(x3,y3,'TWS',adj=0,col=col_used[1])
    text(x3,y3+width_rect,'P-E-R',adj=0,col=col_used[2])
    text(x3,y3+width_rect*2,'P-E',adj=0,col=col_used[3])
  }
}

dev.off()




