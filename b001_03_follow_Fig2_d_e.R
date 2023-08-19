rm(list=ls())

path_out = 'E:/Nature_comments/path_out/'

result_SR = load(paste0(path_out,'c001_02_CSR_GRACE_GRACE-FO_RL06_Mascons_SR.Rdata'))
TWS_SR_annul = (tws_SR1_annual+tws_SR2_annual+tws_SR3_annual)/3
#TWS_SR_trend = (TWS_SR1_trend+TWS_SR2_trend+TWS_SR3_trend)/3

TWS_SR_all = cbind(TWS_SR1_trend,TWS_SR2_trend,TWS_SR3_trend)
TWS_SR_trend = apply(TWS_SR_all,c(1),mean,na.rm=T)
TWS_SR_max = apply(TWS_SR_all,c(1),max,na.rm=T)
TWS_SR_min = apply(TWS_SR_all,c(1),min,na.rm=T)
TWS_SR_sd = apply(TWS_SR_all,c(1),sd,na.rm=T)

TWS_SR_max[which(TWS_SR_max==-Inf)]=NaN
TWS_SR_min[which(TWS_SR_min==-Inf)]=NaN


result_P_E = load( paste0(path_out,'c001_01_Ocean_PME_ERA5.Rdata'))

result = load(paste0(path_out,'c001_02_CSR_GRACE_GRACE-FO_RL06_Mascons_tibet.Rdata'))

path_pic = 'E:/Nature_comments/pics/'
pic_width = 21;pic_height=12;
tiff(
  filename = paste(path_pic,"c001_05_follow_Fig2_de.tiff", sep = ""),
  width = pic_width,
  height = pic_height,
  units = "cm",
  pointsize = 9,
  res = 300,
  family = 'Helvetica-Narrow'
)

nrow = 1;ncol = 2;
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
for (ipic in 1:2) {
  if(ipic==1){
    par(fig=position1[ipic,], new=F)
  }else{
    par(fig=position1[ipic,], new=T)
  }
  
  if(ipic==1){
    xlim = seq(25,180,60)
    xlim_text = c(2005,2010,2015)
    times = seq(1,174,1)
    R_text =  expression(paste(italic(R),' = 0.66, ',italic(P),' < 0.01',sep='') )
  }
  if(ipic==3){
    xlim = seq(3,14,5)
    xlim_text = c(2005,2010,2015)
    times = seq(1,(2017-2003+1),1)
    R_text =  expression(paste(italic(R),' = 0.66, ',italic(P),' < 0.01',sep='') )
    
  }
  plot(seq(1,9,1),xlim = range(times),
       ylim=c(-3.2,3.2),type="n",axes = F,xlab="",ylab="")
  box()
  if(ipic==1|ipic==3){
    if(ipic==1){
      TWS_SR = TWS_SR_trend[1:174]
      TWS_SR_max_used = TWS_SR_max[1:174]
      TWS_SR_min_used = TWS_SR_min[1:174]
      
      TWS_SR_sd_used = TWS_SR_sd[1:174]
    }
    if(ipic==3){
      TWS_SR = TWS_SR_annul
    }
    TWS_ano = TWS_SR/sd(TWS_SR,na.rm=T)
    TWS_max_ano = TWS_SR_max_used/sd(TWS_SR_max_used,na.rm=T)
    TWS_min_ano = TWS_SR_min_used/sd(TWS_SR_min_used,na.rm=T)
    
    P_E_ano = (PME_demo_trend_NATO3-mean(PME_demo_trend_NATO3,na.rm=T))/sd(PME_demo_trend_NATO3,na.rm=T)
    # polygon(c(rev(times),times), c(rev(TWS_min_ano), (TWS_max_ano)), 
    #         col='#d3ece4',border = NA)
    # polygon(c(rev(years),years), c((rev(ensemble_ano-ensemble_sd)), (ensemble_ano+ensemble_sd)), 
    #         col='#E8EAFE',border = NA)
    lines(times,TWS_ano,lwd=2,col='#99d2bf')
    lines(times,P_E_ano,lwd=2,col='#316ca3')

  }
  
  if(ipic==2|ipic==4){
    if(ipic==2){
      TWS_TPS = TWS_SR_trend_TPS[1:174]
      TWS_TPM1 = TWS_SR_trend_TPM1[1:174]
      TWS_TPM2 = TWS_SR_trend_TPM2[1:174]
      

    }
    if(ipic==4){
      TWS_SR = TWS_SR_annul
    }
    TWS_TPS_ano = TWS_TPS/sd(TWS_TPS,na.rm=T)
    TWS_TPM1_ano = TWS_TPM1/sd(TWS_TPM1,na.rm=T)
    TWS_TPM2_ano = TWS_TPM2/sd(TWS_TPM2,na.rm=T)
    
    P_E_ano = (PME_demo_trend_NATO3-mean(PME_demo_trend_NATO3,na.rm=T))/sd(PME_demo_trend_NATO3,na.rm=T)
    # polygon(c(rev(times),times), c(rev(TWS_min_ano), (TWS_max_ano)), 
    #         col='#d3ece4',border = NA)
    # polygon(c(rev(years),years), c((rev(ensemble_ano-ensemble_sd)), (ensemble_ano+ensemble_sd)), 
    #         col='#E8EAFE',border = NA)
    lines(times,TWS_TPS_ano,lwd=2,col='#b31373')
    lines(times,TWS_TPM1_ano,lwd=2,col='#199aab')
    lines(times,TWS_TPM2_ano,lwd=2,col='#AA00FF')
    
    lines(times,P_E_ano,lwd=2,col='#316ca3')
    
  }
  
  axis(1,xlim,tck=-0.02,cex=1,labels = F,line=0)
  mtext(side=1,at=xlim,xlim_text,line=0.2,las=1)
  ylim = seq(-3,3,1)
  axis(side=2, at=ylim,col="black",las=1,col.axis="black",tck=-0.02,labels = F,line=0)  ## las=1 makes horizontal labels
  mtext(side=2,at=ylim,ylim,line=0.35,las=1)
  par(usr=c(0,1,0,1))
  
  #text(0.05,0.08,labels=R_text,adj=0)
  text(0.02,0.96,letters[ipic+3],font=2,adj=0)
  
}
par(fig=c(0.63,0.99,0.63,0.95), new=T)
plot(seq(1,9,1),xlim = c(0,1),
     ylim=c(0,1),type="n",axes = F,xlab="",ylab="")
y0 = 0.8;y_break = 0.2
x0 = 0.05
line_width = 0.1;
col_all = c('#316ca3','#99d2bf','#b31373',
            '#199aab','#AA00FF')
text_all = c('PME in NATO3',
             'average TWS of SR1,SR2 and SR3',
             'TWS_TPS',
             'TWS_TPS1',
             'TWS_TPS2')
# lines(times,TWS_TPS_ano,lwd=2,col='#b31373')
# lines(times,TWS_TPM1_ano,lwd=2,col='#199aab')
# lines(times,TWS_TPM2_ano,lwd=2,col='#AA00FF')
# 
# lines(times,P_E_ano,lwd=2,col='#316ca3')
for (iline in 1:5) {
  segments(x0,y0-(iline-1)*y_break,
           x0+line_width,y0-(iline-1)*y_break,
           col = col_all[iline])
  text(x0+line_width+0.05,y0-(iline-1)*y_break,text_all[iline],adj=0)

}


dev.off()
