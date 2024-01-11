rm(list=ls())

path_out = 'E:/Nature_comments/path_out/'

result = load(paste0(path_out,'b001_01_CSR_GRACE_GRACE-FO_RL06_Mascons_SR.Rdata'))
##########################################
result_P_E = load( paste0(path_out,'b001_02_Land_SR_PME_ERA5.Rdata'))

path_pic = 'E:/Nature_comments/pics/'
pic_width = 21;pic_height=12;
tiff(
  filename = paste(path_pic,"c001_04_follow_supplementFig5.tiff", sep = ""),
  width = pic_width,
  height = pic_height,
  units = "cm",
  pointsize = 9,
  res = 300,
  family = 'Helvetica-Narrow'
)

nrow = 2;ncol = 3;
width = 0.25;height = 0.32;
break1 = 0.08; break2 = 0.11
xstart=0.05;ystart=0.95;
position2 = array(NaN,dim = c(nrow*ncol,4))
for (r in 1:nrow) {
  for (c in 1:ncol) {
    position2[(r-1)*ncol+c,1] = xstart + (c-1)*width+(c-1)*break1;
    position2[(r-1)*ncol+c,2] = xstart + c*width+(c-1)*break1;
    position2[(r-1)*ncol+c,3] = ystart - r*height-(r-1)*break2;
    position2[(r-1)*ncol+c,4] = ystart - (r-1)*height-(r-1)*break2;
  }
}
position1 = round(position2,2)
par( oma = c(1, 2, 1, 1),mar = c(0, 0, 0, 0))
title_name = c('SR1','SR2','SR3')
for (ipic in 1:3) {
  if(ipic==1){
    par(fig=position1[ipic,], new=F)
  }else{
    par(fig=position1[ipic,], new=T)
  }
  if(ipic==1){
    PME = (PME_demo_trend_SR1-mean(PME_demo_trend_SR1,na.rm=T))/sd(PME_demo_trend_SR1,na.rm=T)
    TWS = (TWS_SR1_trend/sd(TWS_SR1_trend,na.rm=T))[1:174]
    #text(x_start+11.5,line1,labels= expression(paste(italic(Slope),' = 3.4 mm decade'^{-1},sep='') ),col="#0245EE",adj=0)
    R_text =  expression(paste(italic(R),' = 0.26, ',italic(P),' < 0.001',sep='') )
    xlim = seq(25,180,60)
    xlim_text = c(2005,2010,2015)
    times = seq(1,174,1)
  }
  if(ipic==2){
    PME = (PME_demo_trend_SR2-mean(PME_demo_trend_SR2,na.rm=T))/sd(PME_demo_trend_SR2,na.rm=T)
    TWS = (TWS_SR2_trend/sd(TWS_SR2_trend,na.rm=T))[1:174]
    #text(x_start+11.5,line1,labels= expression(paste(italic(Slope),' = 3.4 mm decade'^{-1},sep='') ),col="#0245EE",adj=0)
    R_text =  expression(paste(italic(R),' = 0.04, ',italic(P),' > 0.1',sep='') )
  }
  
  if(ipic==3){
    PME = (PME_demo_trend_SR3-mean(PME_demo_trend_SR3,na.rm=T))/sd(PME_demo_trend_SR3,na.rm=T)
    TWS = (TWS_SR3_trend/sd(TWS_SR3_trend,na.rm=T))[1:174]
    #text(x_start+11.5,line1,labels= expression(paste(italic(Slope),' = 3.4 mm decade'^{-1},sep='') ),col="#0245EE",adj=0)
    R_text =  expression(paste(italic(R),' = -0.05, ',italic(P),' > 0.1',sep='') )
  }
  
  if(ipic==4){
    PME =( P_minus_E_annual_SR1-mean(P_minus_E_annual_SR1))/sd(P_minus_E_annual_SR1)
    TWS = tws_SR1_annual/sd(tws_SR1_annual)
    R_text =  expression(paste(italic(R),' = 0.37, ',italic(P),' > 0.1',sep='') )
    xlim = seq(3,14,5)
    xlim_text = c(2005,2010,2015)
    times = seq(1,(2017-2003+1),1)
  }
  if(ipic==5){
    PME =( P_minus_E_annual_SR2-mean(P_minus_E_annual_SR2))/sd(P_minus_E_annual_SR2)
    TWS = tws_SR2_annual/sd(tws_SR2_annual)
    R_text =  expression(paste(italic(R),' = 0.17, ',italic(P),' > 0.1',sep='') )
    xlim = seq(3,14,5)
    xlim_text = c(2005,2010,2015)
    times = seq(1,(2017-2003+1),1)
  }
  if(ipic==6){
    PME =( P_minus_E_annual_SR3-mean(P_minus_E_annual_SR3))/sd(P_minus_E_annual_SR3)
    TWS = tws_SR3_annual/sd(tws_SR3_annual)
    R_text =  expression(paste(italic(R),' = -0.06, ',italic(P),' > 0.1',sep='') )
    xlim = seq(3,14,5)
    xlim_text = c(2005,2010,2015)
    times = seq(1,(2017-2003+1),1)
  }
  years = seq(2003,2020,1)
  #ntimes = 12*length(years)

  plot(seq(1,9,1),xlim = range(times),
       ylim=c(-3.2,3.2),type="n",axes = F,xlab="",ylab="")
  box()
  
  lines(times,PME,lwd=2,col = '#316ca3')
  lines(times,TWS,lwd=2,col= '#BE3257')
  
  #text(x_start+11.5,line1,labels= expression(paste(italic(Slope),' = 3.4 mm decade'^{-1},sep='') ),col="#0245EE",adj=0)


  ylim = seq(-3,3,1.5)
  ylim_text = c('-3.0','-1.5','0.0','1.5','3.0')
  axis(side=2, at=ylim,col="black",las=1,col.axis="black",tck=-0.02,labels = F,line=0)  ## las=1 makes horizontal labels
  mtext(side=2,at=ylim,ylim_text,line=0.35,las=1)
  axis(1,xlim,tck=-0.02,cex=1,labels = F,line=0)
  mtext(side=1,at=xlim,xlim_text,line=0.2,las=1)
  par(usr=c(0,1,0,1))
  text(0.05,0.08,labels=R_text,adj=0)
  text(0.05,0.95,letters[ipic],adj=0,font=2)
  if(ipic==1|ipic==2|ipic==3){
    mtext(side=3,at=0.5,title_name[ipic],line=0.35,las=1)
    
  }
  
}
###########################################
dev.off()
