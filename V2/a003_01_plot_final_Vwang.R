
rm(list=ls())
library(yarrr)
nor_data <- function(data){
  data_ano = as.numeric((data-mean(data,na.rm=T))/sd(data,na.rm=T))
  return(data_ano)
}
path_out = 'D:/Nature_comments/path_out/'
eyear = 2022

result_tws = load(paste0(path_out,'a002_01_upstream_shp_tws.Rdata'))
TWS_EX = nor_data(TWS_SR_trend_EXT[1:((eyear-2003+1)*12)])
TWS_EN = nor_data(TWS_SR_trend_TPS[1:((eyear-2003+1)*12)])
index_NAN = which(round(tws_SR_ori_EXT,2)==0)
TWS_EX_NAN = TWS_EX
TWS_EX_NAN[index_NAN] = NaN


result_PME = load(paste0(path_out,'a001_00_',2003,'_',2022,'_NATO_all_pattern_data.Rdata'))
PME_NATO3_ano = nor_data(NTAO_PME_trend[1:((eyear-2003+1)*12),3])

nperiod = eyear-2003+1-14+1
run_cor_coff = array(NaN,dim = c(nperiod,2))
run_pvalue_coff = array(NaN,dim = c(nperiod,2))
for (iregion in 1:2) {
  if(iregion==1){
    y_data = TWS_EX
  }
  if(iregion==2){
    y_data = TWS_EN
  }
  ##################
  x_data = PME_NATO3_ano
  
  
  for (iii in 1:nperiod) {
    stime =(2003+iii-2003-1)*12+1;etime = (2016-2003+0+iii)*12
    pme_period1 = x_data[stime:etime]
    tws_period1 = y_data[stime:etime]
    period1 = seq(stime,etime,1)
    fit_2003 = cor.test(pme_period1,tws_period1)
    run_cor_coff[iii,iregion] = fit_2003$estimate
    run_pvalue_coff[iii,iregion] = fit_2003$p.value
  }
}


#################
path_pic = 'D:/Nature_comments/response/pics/'

pic_width = 22;pic_height=16;
tiff(
  filename = paste(path_pic,"a003_01_plot_run_correlation_14year.tiff", sep = ""),
  width = pic_width,
  height = pic_height,
  units = "cm",
  pointsize = 11,
  res = 300,
  family = 'Helvetica-Narrow'
)


par( oma = c(1, 3, 1, 1),mar = c(0, 0, 0, 0))
x1 = 0.05 ;x2 = 0.52
position11 = cbind(x1,x2,0.63,0.98)
position13 = cbind( x1,x2, 0.18, 0.53)
position12 = cbind(0.54,0.8,0.18,0.98)
position14 = cbind( 0.82,0.99,0.18,0.98)
position1 = rbind(position11,position13,position12,position14)
path_out = 'D:/Nature_comments/path_out/'

eyear = 2022
nperiod = eyear-2003+1-14+1
###########################
for (ipic in 1:4) {
  if(ipic==1){
    par(fig=position1[ipic,], new=F)
  }else{
    par(fig=position1[ipic,], new=T)
  }
  if(ipic==1){

    result_PME = load( paste0(path_out,'a001_00_',2003,'_',2022,'_NATO_all_pattern_data.Rdata'))
    PME_NATO3_ano = nor_data(NTAO_PME_trend[1:((eyear-2003+1)*12),3])
    
    plot(PME_NATO3_ano,type="n",axes = F,xlab="",ylab="",xaxs="i",yaxs="i",
         ylim=c(-2.7,4),lwd=2,xlim=c(-2,240))
    
    col1 = 'gray95'
    col2 = '#D50000'#'red'#'#B71C1C'
    
    col_num = 0.5
    rect((12*15)-6,-4,280,6,col=col1,border = col1)
    #abline(h=0)
    #abline(v=170,col='gray60')
    lines(PME_NATO3_ano,col=rgb(0,70/255,139/255),lwd=2)
    
    #lines(TWS_EX,col='red',lwd=1,lty=3)
    #lines(TWS_EX_NAN,col='red',lwd=2,lty=1)
    
    box()
    
    
    xlim <- seq(24,228,5*12)
    axis(1,xlim,tck=-0.02,cex=1,labels = F,line=0)
    mtext(side=1,at=xlim,seq(2005,2020,5),line=0.35,las=1)
    ylim = seq(-2,4,2);ylim_text = ylim;number=0.2
    axis(side=2, at=ylim,col="black",las=1,col.axis="black",tck=-0.02,labels = F,line=0)  ## las=1 makes horizontal labels
    mtext(side=2,at=ylim,ylim_text,line=0.4,las=1)
    
    par(usr=c(0,1,0,1))
    text(0.02,0.95,letters[ipic],adj=0,font=2)
    mtext(side=2,at=0.5,expression(paste('Indices',sep='')) ,
          line=2,las=0)
    
    #region_name = expression(paste('NATO3 region (50'*degree*'W~0'*degree*', 43'*degree*'N~0'*degree*')',sep='') )
    
    region_name = expression(paste('PME over NATO3 (50'*degree*'W~0'*degree*', 43'*degree*'N~0'*degree*')',sep='') )
    segments(0.16,0.92,0.24,0.92,col=rgb(0,70/255,139/255),lwd=2)
    text(0.26,0.92,region_name,adj=0,col=rgb(0,70/255,139/255))
    
    # segments(0.16,0.82,0.24,0.82,col='red',lwd=2)
    # text(0.26,0.82,"TWS over exorheic Tibetan Plateau",adj=0,col='red')
    
    #mtext(side=2,at=0.5,expression(paste(',line=1.5,las=0)
    

    
  }
  ###
  if(ipic==2){
    
    ############
    
    ymin = -0.12
    plot(run_cor_coff[,1],type="n",axes = F,xlab="",ylab="",xaxs="i",yaxs="i",
         ylim=c(ymin,0.8),lwd=2,xlim=c(0.2,nperiod+0.5))
   
    
    col_blue =c( rgb(36/255, 0/255, 217/255),rgb(25/255, 29/255, 247/255),
                 rgb(41/255, 87/255, 255/255),
                 rgb(87/255, 176/255, 255/255), rgb(117/255, 211/255, 255/255),
                 rgb(153/255, 235/255, 255/255)
                 )
    col_box = 'gray30'
    rect_w = 0.21
    for (iset in 1:1) {
      if(iset==1){
        set_M = rect_w*(-1)
        #col_box = rgb(255/255,160/255,0/255)#rgb(72/255,124/255,190/255)
      }
      if(iset==2){
        set_M = rect_w
        col_box = rgb(255/255,160/255,0/255)
      }
      
      rect_loc = seq(1,nperiod,1)
      rect_width = abs(set_M)-0.01
      
      rect((rect_loc-rect_width),0,
           (rect_loc+rect_width),run_cor_coff[,iset],
           col=col_box,border = col_box)
      box()
      
      
      for (ibox in 1:nperiod) {
        if(run_pvalue_coff[ibox,iset]<0.05&run_pvalue_coff[ibox,iset]>0.01){
          text(rect_loc[ibox],run_cor_coff[ibox,iset]+0.03,'*',adj=0.5) 
        }
        if(run_pvalue_coff[ibox,iset]<0.01){
          text(rect_loc[ibox],run_cor_coff[ibox,iset]+0.03,'**',adj=0.5) 
        }
      }
    }
    

   
    #######################################
    abline(h=0,lty=1)
    xlim <- seq(1,nperiod,1)
    xlim_text = c('2003-2016','2004-2017','2005-2018',
                  '2006-2019','2007-2020','2008-2021','2009-2022')
    axis(1,xlim,tck=-0.02,cex=1,labels = F,line=0)
    text(xlim+0.02,(ymin-0.06),xlim_text,adj = 1,xpd=NA,srt=30)
    ##
    ylim = seq(0,0.8,0.2);ylim_text = ylim;number=0.2
    axis(side=2, at=ylim,col="black",las=1,col.axis="black",tck=-0.02,labels = F,
         line=0)  ## las=1 makes horizontal labels
    mtext(side=2,at=ylim,ylim_text,line=0.4,las=1)
    par(usr=c(0,1,0,1))
    mtext(side=2,at=0.5,"Correlation coefficient" , 
          line=2.5,las=0)
    text(0.02,0.95,letters[ipic],adj=0,font=2)
    
    x0 = 0.45+0.06
    rect(x0,0.88,x0+0.1,0.95,col=col_box,border = col_box)#rgb(72/255,124/255,190/255)
    #rect(x0,0.73,x0+0.1,0.8,col=rgb(255/255,160/255,0/255))
    # xlim_text = c(expression(paste(italic(R)[paste('NATO3'[PME])]^'Exorheic'[TWS],sep="")),
    #               expression(paste(italic(R)[paste('NATO3'[PME])]^'Endorheic'[TWS],sep="")))
    xlim_text = c(expression(italic(R)[paste("NATO3"[PME], ", Exorheic"[TWS], sep = "")]),
                  expression(italic(R)[paste("NATO3"[PME], ", Endorheic"[TWS], sep = "")]))
    
    text(x0+0.12,0.915,xlim_text[1],adj=0)
    #text(x0+0.12,0.76,xlim_text[2],adj=0)
  }
  
  if(ipic==3){
    plot(PME_NATO3_ano,type="n",axes = F,xlab="",ylab="",xaxs="i",yaxs="i",
         ylim=c(0.5,8.5),lwd=2,xlim=c(-1,1))
    
    
    
    path_out = 'D:/Nature_comments/path_out/'
    result = load(paste0(path_out,'a002_01_plot_cros_correlation_PME_NATO3TP_LR_lag12_max_p.Rdata'))
    
    

    #abline(v=0,lty=2)
    
    cols_all = c('#D50000','#6200EA','#304FFE','#0091EA','#00B8D4',
                 '#00BFA5','#00C853','#AEEA00','#FFD600','#FFAB00',
                 '#FF6D00','#DD2C00','#3E2723','#AA00FF')
    
    cols_all =c(rgb(213/255,62/255,79/255),rgb(232/255,90/255,71/255),rgb(246/255,124/255,73/255),
                rgb(251/255,164/255,92/255),rgb(253/255,197/255,116/255),rgb(254/255,226/255,143/255),
                rgb(254/255,245/255,175/255),rgb(247/255,251/255,179/255),rgb(231/255,245/255,155/255),
                rgb(198/255,232/255,158/255),rgb(160/255,216/255,164/255),rgb(117/255,200/255,164/255),
                rgb(81/255,171/255,174/255),rgb(50/255,136/255,189/255))
    
    for (itype in 1:2) {
      if(itype==1){
        y_mov = 0.4
        y_seg = 0.2
        y_txt = 0.7
        
        col1 = '#B71C1C'
        pch_num  =1
        
        path = 'D:/Nature_comments/supplementdata/nature/main_plot/main_plot_data/fig2/'
        file = read.csv(paste0(path,'bar_cordf.csv'))
        data = as.numeric(file[,3])  
        dim(data) = c(4,14)
        plot_dot = t(data)
        
        mean_loc = c(0.3,0.5,0.59,0.1)
        
      }
      if(itype==2){
        y_mov = -0.4
        y_seg = -0.2
        y_txt = -0.7
        plot_dot = plot_cross_cor_SR_period3
        col1 = '#6200EA'
        pch_num  =16
        mean_loc = c(0.38,-0.03,0.08,-0.2)
      }
      ylim_loc = seq(1.5,8,2);
      
      for (iregion in 1:4) {
        seg_width = 0.08
        xx1 = min(plot_dot[,iregion])
        xx2 = max(plot_dot[,iregion])
        segments(xx1,ylim_loc[iregion]+y_seg,
                 xx2,ylim_loc[iregion]+y_seg)
        segments(xx1,ylim_loc[iregion]+y_seg+seg_width,
                 xx1,ylim_loc[iregion]+y_seg-seg_width)
        
        segments(xx2,ylim_loc[iregion]+y_seg+seg_width,
                 xx2,ylim_loc[iregion]+y_seg-seg_width)

      }

      
      for (itws in 1:14) {
        points(plot_dot[itws,],ylim_loc,pch = pch_num,
               col =transparent(cols_all[itws],0)  )
      }
      ###########
      ###########
     
      plot_text = round(apply(plot_dot,c(2),mean,na.rm=T),2)
      #mean_loc = round(apply(plot_dot,c(2),median,na.rm=T),2)
      text(mean_loc,ylim_loc+y_mov,
           plot_text,adj=0.5,col=col1)
      text(mean_loc,ylim_loc+y_txt,
           "Mean CC",adj=0.6,col=col1)

    }
    box()
    ylim = seq(1.5,8,2);
    ylim_text = c('PME in NATO1','PME in NATO2','PME in NATO3','PME in NATO4');
    number=0.2
    axis(side=4, at=ylim,col="black",las=1,col.axis="black",tck=-0.02,labels = F,line=0)  ## las=1 makes horizontal labels
    mtext(side=4,at=ylim,ylim_text,line=0.6,las=0)
    
    xlim <- seq(-1,1,0.5)
    axis(1,xlim,tck=-0.02,cex=1,labels = F,line=0)
    mtext(side=1,at=xlim,c('-1.0','-0.5','0','0.5','1.0'),line=0.35,las=1)
    par(usr=c(0,1,0,1))
    mtext(side=1,at=0.5,'TWS in HSRs and TPMs',
          line=2,las=0)
    text(0.02,0.98,letters[ipic],adj=0,font=2)
    
  }
  
  if(ipic==4){
    plot(PME_NATO3_ano,type="n",axes = F,xlab="",ylab="",xaxs="i",yaxs="i",
         ylim=c(0.5,18.5),lwd=2,xlim=c(0,1))
    #box()
    text(0.25,14.5,adj=0,'2003-2016',xpd=NA,srt=90,col='#B71C1C')
    text(0.45,14.5,adj=0,'2008-2021',xpd=NA,srt=90,col='#6200EA')
    
    text_name = c('HSR1',
                  'HSR2','HSR3','HSR4',
                  'HSR5','HSR6','HSR7',
                  'HSR8','HSR9','HSR10',
                  'HRS11','HSR12',
                  "TPM1",'TPM2')
    pch_rec = seq(14,1,-1)
    points(rep(0.25,14),pch_rec,pch=1,col=cols_all,lwd=2)
    points(rep(0.45,14),pch_rec,pch=16,col=cols_all,lwd=2)
    text(rep(0.55,14),pch_rec,text_name,adj=0)
    
  }

  
}


dev.off()