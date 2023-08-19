rm(list=ls())

path = 'I:/Nature_comments/?ṩ????/nature/clean_up/'
path_out = 'I:/Nature_comments/path_out/'
nyears = 2017-2003+1

file = read.csv(paste0(path_out,'a002_01_rewrite_supplementfig3_contribution_yearly_rates_accounting_for_disregarding_on-route_changes.csv') )

path_pic = 'I:/Nature_comments/pics/'
library(maps)
library(plotrix)
pic_width = 25;pic_height=21;
tiff(
  filename = paste(path_pic,"a002_03_rewrite_supplement_figure3.tiff", sep = ""),
  width = pic_width,
  height = pic_height,
  units = "cm",
  pointsize = 10,
  res = 300,
  family = 'Helvetica-Narrow'
)
nrow = 3;ncol = 5;
width = 0.17;height = 0.26;
xstart=0.03;ystart=0.97;
break1=0.03;break2 = 0.08
position2 = array(NaN,dim = c(nrow*ncol,4))
for (r in 1:nrow) {
  for (c in 1:ncol) {
    position2[(r-1)*ncol+c,1] = xstart + (c-1)*width+(c-1)*break1;
    position2[(r-1)*ncol+c,2] = xstart + c*width+(c-1)*break1;
    position2[(r-1)*ncol+c,3] = ystart - r*height-(r-1)*break2;
    position2[(r-1)*ncol+c,4] = ystart - (r-1)*height-(r-1)*break2;
  }
}
par( oma = c(2, 2, 2, 0.3),mar = c(0, 0, 0, 0))
region_name = c('Asia (AS)',  'North Africa (NAF)', 'North Atlantic (NATO)','Indian Ocean (IO)',
                'Arctic Ocean (AO)', 'Black Sea (BS)', 'Caspian Sea (CS)', 'Europe (EU)',  'Mediterranean Sea (MS)',
                'North America (NAM)',  'Pacific Ocean (PO)', 'Red Sea (RS)', 'Tibet Plateau (TP)')
index_number = c(9,12,2,5,
                 1,3,4,11,6,
                 13,7,8,10)

col_used = c(rgb(227/255,76/255,55/255),
             rgb(51/255,178/255,188/255),
             rgb(0/255,160/255,135/255),
             rgb(128/255,149/255,159/255),
             rgb(127/255,97/255,73/255),
             rgb(189/255,33/255,22/255),
             rgb(194/255,70/255,66/255),
             rgb(121/255,107/255,133/255),
             rgb(39/255,109/255,135/255),
             rgb(144/255,209/255,193/255),
             rgb(242/255,155/255,128/255),
             rgb(169/255,149/255,161/255),
             rgb(137/255,166/255,184/255))
for (ipic in 1:13) {
  if(ipic==1){
    par(fig=position2[ipic,], new=F)
  }else{
    par(fig=position2[ipic,], new=T)
  }
  
  resource_con = round(as.numeric(file[ipic,2:181]) ,2) 
  dim(resource_con) = c(12,nyears)
  resource_con_monthly = array(NaN,dim = c(12,5))
  for (imonth in 1:12) {
    resource_con_monthly[imonth,] = quantile(resource_con[imonth,],na.rm=TRUE)
  }
  if(ipic==1){
    y_min = 0;y_max = 2
    row_lines = seq(y_min,y_max,0.25)
    ylim = seq(y_min,y_max,0.5)
    ytext = c('0.0','0.5','1.0','1.5','2.0')
  }
  if(ipic==2){
    y_min = 0;y_max = 6
    row_lines = seq(y_min,y_max,1)
    ylim = seq(y_min,y_max,2)
    ytext = ylim
  }
  if(ipic==3){
    y_min = 0;y_max = 1.25
    row_lines = seq(y_min,y_max,0.25)
    ylim = seq(y_min,y_max,0.5)
    ytext = c('0.0','0.5','1.0')
  }
  if(ipic==4){
    y_min = 0;y_max = 1
    row_lines = seq(y_min,y_max,0.2)
    ylim = seq(y_min,y_max,0.4)
    ytext = ylim
  }
  if(ipic==5){
    y_min = 0;y_max = 0.043
    row_lines = seq(y_min,y_max,0.01)
    ylim = seq(y_min,y_max,0.02)
    ytext = ylim
  }
  if(ipic==6){
    y_min = 0;y_max = 0.13
    row_lines = seq(y_min,y_max,0.02)
    ylim = seq(y_min,y_max,0.04)
    ytext = c('0.0','0.04','0.08','0.12','0.16')
  }
  if(ipic==7){
    y_min = 0;y_max = 0.18
    row_lines = seq(y_min,y_max,0.02)
    ylim = seq(y_min,y_max,0.04)
    ytext =c('0.0','0.04','0.08','0.12','0.16')
  }
  if(ipic==8){
    y_min = 0;y_max = 0.6
    row_lines = seq(y_min,y_max,0.1)
    ylim = seq(y_min,y_max,0.2)
    ytext = ylim
  }
  if(ipic==9){
    y_min = 0;y_max = 0.3
    row_lines = seq(y_min,y_max,0.05)
    ylim = seq(y_min,y_max,0.1)
    ytext = ylim
  }
  if(ipic==10){
    y_min = 0;y_max = 0.3
    row_lines = seq(y_min,y_max,0.05)
    ylim = seq(y_min,y_max,0.1)
    ytext = ylim
  }
  if(ipic==11){
    y_min = 0;y_max = 1
    row_lines = seq(y_min,y_max,0.2)
    ylim = seq(y_min,y_max,0.4)
    ytext = ylim
  }
  if(ipic==12){
    y_min = 0;y_max = 0.16
    row_lines = seq(y_min,y_max,0.02)
    ylim = seq(y_min,y_max,0.04)
    ytext = ylim
  }
  if(ipic==13){
    y_min = 0;y_max = 0.16
    row_lines = seq(y_min,y_max,0.02)
    ylim = seq(y_min,y_max,0.04)
    ytext = ylim
  }
  
  y_min = 0;y_max = 2
  row_lines = seq(y_min,y_max,0.25)
  ylim = seq(y_min,y_max,0.5)
  ytext = ylim
  plot(seq(1,9,1),seq(1,9,1),
       type="n",axes = F,xlab="",ylab="",#xaxs="i",yaxs="i",
       ylim=c(y_min,y_max),xlim=c(0.5,12.5))
  segments(seq(1,12,3),(y_min-10),seq(1,12,3),(y_max+10),col='gray85')
  segments(0,row_lines,13,row_lines,col='gray85')
  
  rect_loc = seq(1,12,1);rect_width = 0.4
  for (ibox in 1:12) {
    segments(rect_loc[ibox],resource_con_monthly[ibox,1],
             rect_loc[ibox],resource_con_monthly[ibox,5],
             lwd=1.5)
    rect((rect_loc[ibox]-rect_width),resource_con_monthly[ibox,2],
         (rect_loc[ibox]+rect_width),resource_con_monthly[ibox,4],
         col = col_used[ipic],border = 'black')
    segments((rect_loc[ibox]-rect_width),resource_con_monthly[ibox,3],
             (rect_loc[ibox]+rect_width),resource_con_monthly[ibox,3],
             lwd=2)
    
  }
  ######
  xlim <- seq(1,12,3);
  axis(1,xlim,tck=-0.02,cex=1,labels = F,line=0)
  mtext(side=1,at=xlim,month.abb[xlim],line=0.2,las=1)
  axis(side=2, at=ylim,col="black",las=1,col.axis="black",tck=-0.02,labels = F,line=0)  ## las=1 makes horizontal labels
  mtext(side=2,at=ylim,ytext,line=0.35,las=1)
  ##
  ###
  mtext(side=3,at=0.5,letters[ipic],font=2,line=0.3)
  mtext(side=3,at=13,region_name[ipic],adj=1,line=0.3)
  par(usr=c(0,1,0,1))
  if(ipic==6){
    mtext(side=2,at=0.5,'Monthly contribution rates (MCR) of water vapor (%)',line=2,las=0)
  }
  box()
}

par(fig=position2[14,], new=T)
par(usr=c(0,1,0,1))
plot(seq(1,9,1),seq(1,9,1),
     type="n",axes = F,xlab="",ylab="",#xaxs="i",yaxs="i",
     ylim=c(0,1),xlim=c(0,1))
#box()
text(0.01,0.95,'Definition of the boxplot',adj=0)
segments(0.25,0.2,0.25,0.4,lwd=1.5)
segments(0.25,0.6,0.25,0.8,lwd=1.5)
rect(0.2,0.4, 0.3,0.6,border = 'black')
segments(0.2,0.5,0.3,0.5,lwd=2)

arrow_lines= c(0.2,0.4,0.5,0.6,0.8)
arrows(0.35,arrow_lines,0.55,arrow_lines,length=0.08)
text(0.6,arrow_lines,adj=0,c('Min','25%',"Median","75%","Max"))
dev.off()






