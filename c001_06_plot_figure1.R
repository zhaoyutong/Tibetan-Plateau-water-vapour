{
  rm(list=ls())
  source('D:/R_function/find_location.R')
  
  path_out = 'D:/Nature_comments/path_out/'
  {
    lat1_used = 0; lat2_used = 65
    lon1_used = -80; lon2_used = 122
    ###################
    result_EMP = load(paste0(path_out,'c000_03_North_hemisphere_EMP_trend.Rdata'))
    lat_region_used = lat_region(lat ,lat1=lat1_used,lat2=lat2_used)
    indexlat1 = lat_region_used[[1]]
    indexlat2 = lat_region_used[[2]]
    latitude = lat[indexlat1:indexlat2];nlats = length(latitude)
    lon_region_used = lon_region(lon,lon1=lon1_used,lon2=lon2_used)
    indexlon1 = lon_region_used[[1]]
    indexlon2 = lon_region_used[[2]]
    longitude = lon[indexlon1:indexlon2];nlons = length(longitude)
    EMP_trend = E_minus_P_trend[indexlon1:indexlon2,indexlat1:indexlat2]
    #################
    result_QU = load(paste0(path_out,'c003_03_Q_qv_trend_regrid_annual.Rdata'))
    lat_region_used = lat_region(latitude_used ,lat1=lat1_used,lat2=lat2_used)
    indexlat1 = lat_region_used[[1]]
    indexlat2 = lat_region_used[[2]]
    lat_wind= latitude_used[indexlat1:indexlat2];nlats = length(latitude)
    lon_region_used = lon_region(longitude_used,lon1=lon1_used,lon2=lon2_used)
    indexlon1 = lon_region_used[[1]]
    indexlon2 = lon_region_used[[2]]
    lon_wind = longitude_used[indexlon1:indexlon2];nlons = length(longitude)
    Q_qv_trend_wind = Q_qv_trend_regrid[indexlon1:indexlon2,indexlat1:indexlat2]
    Q_qu_trend_wind = Q_qu_trend_regrid[indexlon1:indexlon2,indexlat1:indexlat2]
    
  }
  #################################
  {
    file = read.csv(paste0(path_out,'a002_01_rewrite_supplementfig2_abs_Monthly_rates_accounting_for_on-route_changes.csv') )
    resource_con = round(as.matrix(file[1:4,2:181]) ,2)
    names = c('Asia','Indian Ocean','North Africa (NAF)','North Atlantic (NATO)')
    nyears = 2017-2003+1
    dim(resource_con) = c(4,12,nyears)
    
    resource_con_monthly = array(NaN,dim = c(4,12,5))
    for (iregion in 1:4) {
      for (imonth in 1:12) {
        resource_con_monthly[iregion,imonth,] = quantile(resource_con[iregion,imonth,],na.rm=TRUE)
      }
    }
  }
  ###########################
  file = read.csv(paste0(path_out,'a002_01_rewrite_supplementfig2_abs_Monthly_rates_accounting_for_on-route_changes.csv') )
  index_number = c(1,2,3,4)
  ###
  resource_con_yearly = array(NaN,dim = c(4,5))
  for (iii in 1:4) {
    
    index = index_number[iii]
    resource_con = round(as.numeric(file[index,2:181]) ,2)
    # dim(resource_con) = c(12,nyears)
    # resource_year = apply(resource_con, c(2), mean,na.rm=T)
    resource_con_yearly[iii,] = quantile(resource_con,na.rm=TRUE)
  }
  names_plot = c('Asian','Indian Ocean',
                 'North Africa','North Atlantic Ocean')
  rownames(resource_con_yearly) = names_plot
  colnames(resource_con_yearly) = c('0%',' 25%','50%','75%','100%' )
  resource_con_yearly_sfig2_mm = resource_con_yearly
  
  ###########################
}
####################
{
  library(raster)
  library(maps)
  library(plotrix)
  library(yarrr)
  
  path_pic = 'D:/Nature_comments/pics/'
  
  pic_width = 21;pic_height=29;
  tiff(
    filename = paste(path_pic,"c001_03_figure1.tiff", sep = ""),
    width = pic_width,
    height = pic_height,
    units = "cm",
    pointsize = 9,
    res = 300,
    family = 'Helvetica-Narrow'
  )
  ######################
  
  map1_loc = c(0.05,0.6,0.8,0.98)
  legend_loc = c(0.05,0.6,0.755,0.77)
  map11_loc = c(0.67,0.96,0.8,0.98)
  
  map2_loc = c(0.05,0.4,0.52,0.7)
  map3_loc = c(0.67,0.98,0.52,0.7)
  
  # map4_loc = c(0.05,0.36,0.24,0.42)
  # map5_loc = c(0.67,0.98,0.24,0.42)
  
  map4_loc = c(0.05,0.55,0.24,0.42)
  map5_loc = c(0.05,0.55,0.18,0.23)
  map6_loc=  c(0.67,0.98,0.24,0.42)
  map7_loc = c(0.67,0.98,0.18,0.23)
  position1 = rbind(map1_loc,map11_loc,map2_loc,map2_loc,map3_loc,
                    map4_loc,map5_loc,map6_loc)
  #,map6_loc,map7_loc)
  col_num = 0.5
  Contourcolors = transparent((rev(c(rgb(36/255, 0/255, 217/255),rgb(25/255, 29/255, 247/255),
                                     rgb(41/255, 87/255, 255/255), rgb(61/255, 135/255, 255/255),
                                     rgb(87/255, 176/255, 255/255), rgb(117/255, 211/255, 255/255),
                                     rgb(153/255, 235/255, 255/255),rgb(189/255, 249/255, 255/255),
                                     rgb(235/255,1,1),
                                     rgb(255/255, 255/255, 235/255),
                                     rgb(255/255, 245/255, 189/255),rgb(255/255, 214/255, 153/255), 
                                     rgb(255/255, 172/255, 117/255),rgb(255/255, 120/255, 87/255), 
                                     rgb(255/255, 61/255, 61/255),
                                     rgb(247/255, 40/255, 54/255),
                                     rgb(217/255, 22/255, 45/255),rgb(166/255, 0/255, 33/255)))), col_num)
  
  breaks_var = c(-50,seq(-0.8,0.8,0.1),50) 
  path_f = 'D:/Nature_comments/path_out/'
  result = load(paste0(path_f,'Tuotuohe_monsoon_day10.Rdata'))
  lon_path = path_arr[,1,]
  lat_path = path_arr[,2,]
  #
  for (ipic in 1:8) {
    if(ipic==1){
      par(fig=position1[ipic,], new=F)
    }else{
      par(fig=position1[ipic,], new=T)
    }
    ###
    par( oma = c(2, 2, 1, 2),mar = c(0, 0, 0, 0))
    
    
    if(ipic==1){
      plot(seq(1,9,1),xlim=c(1,46),ylim=c(-5,4),type="n",axes = F,xlab="",ylab="")
      lon = longitude
      lat = latitude
      par(usr=c(min(lon),max(lon),min(lat),max(lat)))
      map("world",interior = F,add=T, col = "gray90",border="gray90",fill = T,
          ylim = range(lat),xlim = range(lon))
      
      
      .filled.contour(lon,lat,EMP_trend*10,
                      col  = Contourcolors,levels = breaks_var)
      
      # grepal('red')[1:13]
      # grepal("orange")[1:11]
      # grepal("pink$|purple$|violet$")[1:9]
      # grepal('#588300')
      # grepal('#1380A1')[1:11]
      ###NINO3
      col_box = 'gray10'
      col_num = 0
      lty_num = 1
      
      for (iii in 50:150) {#163
        lines(lon_path[,iii],
              lat_path[,iii],
              col = '#80CBC4',lwd = 0.05)
      }
      lines(lon_path[,2],
            lat_path[,2],
            col = '#64FFDA',lwd=2)
      lines(lon_path[,1],
            lat_path[,1],
            col = '#64FFDA',lwd=2)
      path_gis= 'D:/GIS/TP_runoff_basin/'
      
      ######################
      region_shp = rgdal::readOGR(paste(path_gis,"TP_Endorheic.shp",sep=""))
      #plot(region_shp,add=T, col = "gray80",border="gray80")
      plot(region_shp,add=T,axes = F,xlab="",ylab="",border = "gray20",lwd=1)
      region_shp = rgdal::readOGR(paste(path_gis,"TibetanPlateau.shp",sep=""))
      #plot(region_shp,add=T, col = "gray80",border="gray80")
      plot(region_shp,add=T,axes = F,xlab="",ylab="",border = "gray10",lwd=2)
      rect(-50,0,0,43,col = rgb(1,1,1,col_num),border = col_box,lty=lty_num,lwd=2)
      # rect(50,2,90,28,col = rgb(1,1,1,col_num),border = col_box,lty=lty_num,lwd=2)
      
      # ##SR1
      # rect(-20,20,28,50,col = rgb(1,1,1,col_num),border = col_box,lty=lty_num)
      # ##SR2
      # rect(28,20,80,50,col = rgb(1,1,1,col_num),border = col_box,lty=lty_num)
      # ##SR3
      # rect(80,20,105,50,col = rgb(1,1,1,col_num),border = col_box,lty=lty_num)
      ##########################
      
      {
        
        lon = lon_wind
        lat = lat_wind
        u = Q_qu_trend_wind
        v = Q_qv_trend_wind
        
        speed <- sqrt(u^2 + v^2)
        direction  <- atan2(v, u) * 180 / pi
        # ???ݷ??ټ?????ͷ?Ĵ?С
        arrow_size <- speed / max(speed, na.rm = TRUE)
        
        # create a new plot
        
        
        
        
        par(usr=c(min(lon),max(lon),
                  min(lat),max(lat)))
        
        
        
        # set arrow parameters
        # ?ƶ?ƽ???˲???
        window_size <- 3
        s <- rep(1, window_size) / window_size
        
        # ???ٶ????ݽ????ƶ?ƽ???˲?
        speed_smooth <- t(apply(speed, 1, function(x) filter(x, s, sides=2)))
        
        # ???ݷ??ټ?????ͷ?߶εĳ??Ⱥͼ?ͷ?Ĵ?С
        line_length <- speed_smooth / max(speed_smooth, na.rm = TRUE) * 10
        arrow_size <- speed_smooth / max(speed_smooth, na.rm = TRUE) * 10
        
        ration = 2
        max_num = 5;min_num = 2
        index_min = which(line_length<min_num)
        line_length[index_min] = min_num
        index_max = which(line_length>max_num)
        line_length[index_max] = max_num
        
        
        index_min = which(arrow_size<3)
        arrow_size[index_min] = 0.8
        index_max = which(arrow_size>3)
        arrow_size[index_max] = 1.2
        
        
        map("world",interior = F,add=T, col = "gray40",border="gray40",fill = F,
            ylim = range(lat),xlim = range(lon))
        for (i in 1:dim(u)[1]) {
          for (j in 1:dim(u)[2]) {
            x <- lon[i]
            y <- lat[j]
            dx <- line_length[i,j] * cos(direction[i,j] * pi / 180)
            dy <- line_length[i,j] * sin(direction[i,j] * pi / 180)
            arrows(x, y, x+dx, y+dy, col="gray80", length=0.04,
                   lwd=arrow_size[i,j], angle=20)##263238
            
            
            
          }
        }
        index_max = which(arrow_size<1)
        arrow_size[index_max] = 0
        for (i in 1:dim(u)[1]) {
          for (j in 1:dim(u)[2]) {
            x <- lon[i]
            y <- lat[j]
            dx <- line_length[i,j] * cos(direction[i,j] * pi / 180)
            dy <- line_length[i,j] * sin(direction[i,j] * pi / 180)
            arrows(x, y, x+dx, y+dy, col="gray10", length=0.04,
                   lwd=arrow_size[i,j], angle=20)##263238
            
            
            
          }
        }
        
        
      }
      
      ##########################
      # map("world",interior = F,add=T, col = "gray",border="gray",fill = F,
      #     ylim = range(lat),xlim = range(lon))
      cexaxissize=1;lon_ylim = seq(-120,120,60);lon_ylim2 = lon_ylim;
      lonlabels<-c(expression('120'*degree*'W'),expression('60'*degree*'W'),
                   expression('0'*degree*''),
                   expression('60'*degree*'E'),expression('120'*degree*'E'))
      axis(side=1,at=lon_ylim2,labels=F,tck=-0.02,lwd=1)
      mtext(side=1,at=lon_ylim2,lonlabels,line=0.35)
      lat_xlim = seq(60,10,-20)
      axis(side=2,at=lat_xlim,labels=F,tck=-0.02,lwd=1)
      
      
      latlabels<-c(expression('60'*degree*'N'),expression('40'*degree*'N'),
                   expression('20'*degree*'N')#expression('20'*degree*'N'),
      )#expression('20'*degree*'N'),,expression('20'*degree*'S')
      axis(side=2,at=lat_xlim,labels=F,tck=-0.015,lwd=1,las=2)
      mtext(side=2,at=lat_xlim,latlabels,cex=cexaxissize,line=0.5,las=2)
      par(usr=c(0,1,0,1))
      
      rect(0.8,0.92,1,1,col='white',border = 'white')
      mtext(side=3,at=0.02,'a',line=0.5,las=0,font=2)
      box()
      arrows(0.9, 0.93, 0.93, 0.93, col="#263238", length=0.04,
             lwd=1, angle=20)
      text(0.995,0.97,expression(paste('Kg m'^{-1},' s'^{-1},' decades'^{-1},sep="") ) ,adj=1,cex=0.6)
    }
    if(ipic==2){
      plot(seq(1,9,1),xlim=c(0,3),ylim=c(-24,24),type="n",
           axes = F,xlab="",ylab="",xaxs="i",yaxs="i")
      result_EMP = load(paste0(path_out,'c000_03_TP_Indianociean_northatlantic_EMP.Rdata'))
      
      box()
      EMO_cli = c(mean(EMP_IO,na.rm=T)*12,
                  mean(EMP_NA,na.rm=T)*12,
                  mean(EMP_tp,na.rm=T)*12)
      EMO_sd = c(sd(apply(EMP_IO,c(3),mean,na.rm=T)*12),
                 sd(apply(EMP_NA,c(3),mean,na.rm=T)*12),
                 sd(apply(EMP_tp,c(3),mean,na.rm=T)*12))
      rect_loc = seq(0.4,3,1); rect_width = 0.2
      col_used = c('#BF360C','#1B5E20','#673AB7')
      abline(h=0)
      for (ibox in 1:3) {
        
        rect((rect_loc[ibox]-rect_width),0,
             (rect_loc[ibox]+rect_width),EMO_cli[ibox],
             col = col_used[ibox],border = 'black')
        ##############################################
        segments(rect_loc[ibox],(EMO_cli[ibox]-EMO_sd[ibox]),
                 rect_loc[ibox],(EMO_cli[ibox]+EMO_sd[ibox]))
        
        number_rect = 4
        segments((rect_loc[ibox]-rect_width/number_rect),
                 (EMO_cli[ibox]-EMO_sd[ibox]),
                 (rect_loc[ibox]+rect_width/number_rect),
                 (EMO_cli[ibox]-EMO_sd[ibox]))
        
        segments((rect_loc[ibox]-rect_width/number_rect),
                 (EMO_cli[ibox]+EMO_sd[ibox]),
                 (rect_loc[ibox]+rect_width/number_rect),
                 (EMO_cli[ibox]+EMO_sd[ibox]))
        
        
        
      }
      
      xlim <- (seq(0.85,5,1.2)-seq(0.4,4,1.2))/2+seq(0.4,4,1.2);
      ylim = seq(-20,20,10)
      axis(1,rect_loc,tck=-0.02,cex=1,labels = F,line=0)
      text(rect_loc,-26,c('Indian Ocean','North Atlantic Ocean','Tibetan Plateau'),
           adj = 1,xpd=NA,srt=45)
      
      mtext(side=2,at=12,'Net source',line=0.5,las=0)
      mtext(side=2,at=-12,'Net sink',line=0.5,las=0)
      
      
      ####
      ylim =seq(-20,20,10)
      axis(side=4,at=ylim,labels=F,tck=-0.015,lwd=1,las=2)
      mtext(side=4,at=ylim,ylim,line=0.5,las=2)
      mtext(side=4,at=0,expression(paste('Annual mean EMP (mm year'^{-1},')'),sep='') ,
            line=2,las=0)
      
      ####
      par(usr=c(0,1,0,1))
      mtext(side=3,at=0.02,'b',line=0.5,las=0,font=2)
      
      
    }
    if(ipic==3){
      
      
      plot(seq(1,9,1),xlim=c(1,12),ylim=c(0,60),type="n",
           axes = F,xlab="",ylab="",xaxs="i",yaxs="i")
      
      rect(6,-2,9,61,col='gray95',border = 'white')
      box()
      Asian_median = resource_con_monthly[1,,3]
      month_num = seq(1,12,1)
      col_asian = 'black'
      lines(month_num,Asian_median,col=col_asian,lwd=2)
      points(month_num,Asian_median,col=col_asian,pch=19)
      
      month_text = seq(1,12,2)
      axis(side=1,at=month_text,labels=F,tck=-0.02,lwd=1)
      #mtext(side=1,at=month_text,month.abb[month_text],line=0.35)
      text(month_text,-4,month.name[month_text],adj = 1,xpd=NA,srt=45)
      
      ###
      ylim =seq(0,60,15)
      axis(side=2,at=ylim,labels=F,tck=-0.015,lwd=1,las=2,col=col_asian)
      mtext(side=2,at=ylim,ylim,line=0.5,las=2,col=col_asian)
      mtext(side=2,at=30,' Water vapor from  Asian (mm)',line=2,las=0,col=col_asian)
      segments(1,-5,1,65,col=col_asian)
      par(usr=c(0,1,0,1))
      
      mtext(side=3,at=0.02,'c',line=0.5,las=0,font=2)
    }
    if(ipic==4){
      
      
      plot(seq(1,9,1),xlim=c(1,12),ylim=c(0,20),type="n",
           axes = F,xlab="",ylab="",xaxs="i",yaxs="i")
      plot_num = resource_con_monthly[2:4,,3]
      month_num = seq(1,12,1)
      col_asian = c('#BF360C','#0D47A1','#1B5E20')
      text_asian = c(' Water vapor from  Indian Ocean (mm) ',
                     'Water vapor from  North Africa (mm)',
                     'Water vapor from  North Atlantic (mm)')
      axis_line = c(0,3,6)
      for (iline in 1:3) {
        lines(month_num,plot_num[iline,],col=col_asian[iline],lwd=2)
        points(month_num,plot_num[iline,],col=col_asian[iline]   ,pch=19)
        
        ylim =seq(0,20,5)
        axis(side=4,at=ylim,labels=F,line=axis_line[iline],tck=-0.015,lwd=1,las=2,col=col_asian[iline])
        mtext(side=4,at=ylim,ylim,line=(axis_line[iline]+0.5),las=2,col=col_asian[iline])
        mtext(side=4,at=10,text_asian[iline],line=(axis_line[iline]+1.5),las=0,col=col_asian[iline])
      }
      
      ###
      
      
    }
    if(ipic==5){
      plot(seq(1,9,1),seq(1,9,1),
           type="n",axes = F,xlab="",ylab="",#xaxs="i",yaxs="i",
           ylim=c(0,80),xlim=c(0,4.3))
      box()
      plot_box = resource_con_yearly_sfig2_mm
      rect_loc = seq(0.4,4,1.2); rect_width = 0.2
      col_used = c('gray30','#BF360C','#0D47A1','#1B5E20')
      for (ibox in 1:4) {
        segments(rect_loc[ibox],plot_box[ibox,1],
                 rect_loc[ibox],plot_box[ibox,5],
                 lwd=1.5)
        rect((rect_loc[ibox]-rect_width),plot_box[ibox,2],
             (rect_loc[ibox]+rect_width),plot_box[ibox,4],
             col = col_used[ibox],border = 'black')
        segments((rect_loc[ibox]-rect_width),plot_box[ibox,3],
                 (rect_loc[ibox]+rect_width),plot_box[ibox,3],
                 lwd=2)
        
      }
      xlim <- (seq(0.85,5,1.2)-seq(0.4,4,1.2))/2+seq(0.4,4,1.2);
      ylim = seq(0,80,20)
      axis(1,rect_loc,tck=-0.02,cex=1,labels = F,line=0)
      text(rect_loc,-8,names_plot,adj = 1,xpd=NA,srt=45)
      
      axis(side=2, at=ylim,col="black",las=1,col.axis="black",tck=-0.02,labels = F,line=0)  ## las=1 makes horizontal labels
      mtext(side=2,at=ylim,ylim,line=0.35,las=1)
      mtext(side=2,at=40,'Contributing water vapor (mm)',line=2,las=0)
      
      
      ####
      
      ####
      par(usr=c(0,1,0,1))
      mtext(side=3,at=0.02,'d',line=0.5,las=0,font=2)
      rect(0.68,0.42, 1,1,border = 'gray95',col='gray95')
      
      rect(0.7,0.55, 0.8,0.75,border = 'black')
      segments(0.7,0.65,0.8,0.65,lwd=2)
      segments(0.75,0.75,0.75,0.85,lwd=1.5)
      segments(0.75,0.55,0.75,0.45,lwd=1.5)
      text(0.99,0.95,'Legend',adj=1)
      
      
      arrow_lines= c(0.45,0.55,0.65,0.75,0.85)
      arrows(0.81,arrow_lines,0.86,arrow_lines,length=0.04)
      text(0.99,arrow_lines,c('Min','25%',"50%","75%","Max"),
           adj=1,cex=0.9)
      box()
      
    }
    

    if(ipic==6){
      result = load(paste0(path_out,'ERA5_2003_2017_boundary_6_9.Rdata'))
      years = seq(2003,2017,1)
      plot(seq(1,9,1),xlim=c(2002.5,2017.5),ylim=c(-500,1000),type="n",
           axes = F,xlab="",ylab="",xaxs="i",yaxs="i")
      box()
      lwd_number = 2;lty_num = 1
      net_years = SBall5_yeras+NBall5_yeras+WBall5_yeras+EBall5_yeras
      loc_rect = seq(2003,2017,1)
      width_rect = 0.2
      rect(loc_rect-width_rect,0,loc_rect+width_rect,net_years,
           col='gray90',border = 'transparent')
      abline(h=0)
      lines(years,SBall5_yeras,col='#D1C4E9',lwd=lwd_number,lty=lty_num)
      lines(years,NBall5_yeras,col='#64B5F6',lwd=lwd_number,lty=lty_num)
      lines(years,WBall5_yeras,col='#81C784',lwd=lwd_number,lty=lty_num)
      lines(years,EBall5_yeras,col='#FF8A65',lwd=lwd_number,lty=lty_num)
      pch_number = 19
      points(years,SBall5_yeras,col='#7B1FA2',pch=pch_number)
      points(years,NBall5_yeras,col='#1565C0',pch=pch_number)
      points(years,WBall5_yeras,col='#1B5E20',pch=pch_number)
      points(years,EBall5_yeras,col='brown',pch=pch_number)
      
      # abline(lm(SBall5_yeras~years),col='#7B1FA2',lty=2)
      # abline(lm(NBall5_yeras~years),col='#1565C0',lty=2)
      # abline(lm(WBall5_yeras~years),col='#1B5E20',lty=2)
      # abline(lm(EBall5_yeras~years),col='brown',lty=2)
      
      xlim <- seq(2003,2017,2)
      axis(1,xlim,tck=-0.02,cex=1,labels = F,line=0)
      mtext(side=1,at=xlim,xlim,line=0.35,las=1)
      
      
      ylim = seq(-500,1000,500)
      ylim_text = ylim
      axis(side=2, at=ylim,col="black",las=1,col.axis="black",tck=-0.02,labels = F,line=0)  ## las=1 makes horizontal labels
      mtext(side=2,at=ylim,ylim_text,line=0.35,las=1)
      
      par(usr=c(0,1,0,1))
      mtext(side=2,at=0.5,expression(paste("Water vapor budget (10"^{6}," kg s"^{-1},")",sep="")),
            line=2.5,las=0)
      mtext(side=3,at=0.02,'e',line=0.5,las=0,font=2)
      text(0.98,0.92,adj=1,'JJAS')
      
    }
    if(ipic==7){
      plot(seq(1,9,1),xlim=c(0,1),ylim=c(0,1),type="n",
           axes = F,xlab="",ylab="",xaxs="i",yaxs="i")
      #box()
      ###
      y0 = 0.6
      segments(0.02,y0,0.08,y0,col='#D1C4E9',lwd=lwd_number,lty=lty_num)
      segments(0.23,y0,0.29,y0,col='#64B5F6',lwd=lwd_number,lty=lty_num)
      segments(0.44,y0,0.5,y0,col='#81C784',lwd=lwd_number,lty=lty_num)
      segments(0.65,y0,0.71,y0,col='#FF8A65',lwd=lwd_number,lty=lty_num)
      rect(0.86,0.55,0.92,0.65,col='gray90',border = 'gray90')
      
      
      points(0.05,y0,col='#7B1FA2',pch=pch_number)
      points(0.26,y0,col='#1565C0',pch=pch_number)
      points(0.47,y0,col='#1B5E20',pch=pch_number)
      points(0.68,y0,col='brown',pch=pch_number)
      
      text(0.1,y0,adj=0,'South',col='#7B1FA2')
      text(0.31,y0,adj=0,'North',col='#1565C0')
      text(0.52,y0,adj=0,'West',col='#1B5E20')
      text(0.73,y0,adj=0,'East',col='brown')
      text(0.94,y0,adj=0,'Net',col='black')
    }
    if(ipic==8){
      result = load(paste0(path_out,'c006_03_TWS_PMR_trend_SR3_TP_upstream_basin.Rdata'))

      plot(seq(1,9,1),xlim=c(0.7,2.3),ylim=c(-0.35,0.05),type="n",
           axes = F,xlab="",ylab="",xaxs="i",yaxs="i")
      box()

      ###
      TP_trend = c(TP_TWS_trend,TP_PME_trend,TP_PMEMR_trend)
      SR3_trend = c(SR3_TWS_trend,SR3_PME_trend,SR3_PMEMR_trend)
      TP_pvalue = c(TP_TWS_pvalue,TP_PME_pvalue,TP_PMEMR_pvalue)
      SR3_pvalue = c(SR3_TWS_pvalue,SR3_PME_pvalue,SR3_PMEMR_pvalue)
      # box1 = c(TP_TWS_trend,SR3_TWS_trend)
      # box2 = c(TP_PME_trend,SR3_PME_trend)
      # box3 = c(SR3_PMEMR_trend,SR3_PMEMR_trend)
      
      rect_width=0.06

      col_used = c('#b794f6','gray10','#ffaf49')
      
      for (iii in 1:2) {
        if(iii==1){
          rect_loc = c((1-rect_width*2),1,(1+rect_width*2))
          rect_var = TP_trend
          rect_pvalue = TP_pvalue
        }
        if(iii == 2){
          rect_loc = c((2-rect_width*2),2,(2+rect_width*2))
          rect_var = SR3_trend
          rect_pvalue = SR3_pvalue
        }
        for (irect in 1:3) {
          rect(rect_loc[irect] - rect_width, 0, 
               rect_loc[irect] + rect_width,
               rect_var[irect], 
               col = col_used[irect], 
               border = col_used[irect])
          dis_rect = 0.01
          if(rect_pvalue[irect]<0.05&rect_pvalue[irect]>0.01){
            text(rect_loc[irect],(rect_var[irect]-dis_rect),'*',adj=0.5) 
          }
          if(rect_pvalue[irect]<0.01){
            text(rect_loc[irect],(rect_var[irect]-dis_rect),'* *',adj=0.5) 
          }
        }
        
        for (irect in 1:3) {
          rect(rect_loc[irect] - rect_width, 0, 
               rect_loc[irect] + rect_width, 
               rect_var[irect], 
               density = 20, angle = 45, 
               col = "white", border = "white")}
      }

      abline(h=0)
      xlim = seq(1,2,1)
      xlim_text = c(expression('Exorheic basin in TP'^{''}),
                    expression('SR3 region in Zhang'^{'1'}) )
      axis(1,xlim,tck=-0.02,cex=10,labels = F,line=0)
      #text(xlim,-0.37,xlim_text,adj = 1,xpd=NA,srt=10)
      mtext(side=1,at=xlim,xlim_text,line=c(0.3,0.5),las=1,adj=0.5)
      
      ylim = seq(-0.3,0,0.1)
      ylim_text = seq(-0.3,0,0.1)*12*10
      axis(side=2, at=ylim,col="black",las=1,col.axis="black",tck=-0.02,labels = F,line=0)  ## las=1 makes horizontal labels
      mtext(side=2,at=ylim,ylim_text,line=0.35,las=1)
      
     
      
      par(usr=c(0,1,0,1))
      mtext(side=2,at=0.5,expression(paste('Trend (mm decades'^{-1},')',sep=''))  ,line=2,las=0)
      mtext(side=3,at=0.02,'f',line=0.5,las=0,font=2)
      
      x1 = 0.05;x2 = 0.15
      y1 = 0.04;y2 = 0.08;width_rect = 0.07
      
      # rect(x1,y1,x2,y2,col=col_used[1],border =col_used[1])
      # rect(x1,y1+width_rect,x2,y2+width_rect,border=col_used[2])
      # rect(x1,y1+width_rect*2,x2,y2+width_rect*2,border=col_used[3])
      
      for (irect in 1:3) {
        rect(x1,y1+width_rect*(irect-1),x2,y2+width_rect*(irect-1),
             col=col_used[irect],border =col_used[irect])
        
        rect(x1, y1+width_rect*(irect-1), 
             x2, 
             y2+width_rect*(irect-1), 
             density = 20, angle = 45, 
             col = "white", border = "white")}
    
    
   
      x3 = 0.18;y3 = 0.06
      text(x3,y3,'TWS',adj=0,col=col_used[1])
      text(x3,y3+width_rect,'PME',adj=0,col=col_used[2])
      text(x3,y3+width_rect*2,'PMEMR',adj=0,col=col_used[3])
    }



      

  }
  
  ######################
  position_location = legend_loc
  par(fig=position_location, new=T)
  par(usr=position_location)
  cexaxissize=1
  x11 = position_location[1];x2 = position_location[2];y1 = position_location[3];y2=position_location[4]
  color.legend(position_location[1],position_location[3],position_location[2],position_location[4]# coordinates
               ,align="rb",breaks_var # legend values (mean of color value)
               ,rect.col=Contourcolors # colors
               ,gradient='x',col="white" # vertical gradient
  )
  box()
  
  breaks_var = c(-2,seq(-0.8,0.8,0.1),2)
  breaks2 = c(seq(-0.8,0.8,0.2))
  text_name = expression(paste("EMP trend (mm decades"^{-1},')',sep=""))
  ratio = round((x2-x11)/length(breaks2),2)/2
  #axis(side=1,at=seq(x11,x2,(x2-x11)/(length(breaks2)-1)),labels=F,tck=0.05,lwd=1,line=-0.27)
  mtext(breaks2,side=1,at=seq(x11+ratio,x2,((x2-x11)/(length(breaks2)))),line=0,cex=cexaxissize,las=1)
  par(usr=c(0,1,0,1))
  mtext(side=1,at=0.5,text_name,cex=1,line=1.5,las=0,adj=0.5)
  
  
  dev.off()
}



