
# Script to make Figure 6 in McGuirk and Burke 2020
# Workflow is to get figures as close to how we want them to look in R, then add titles, labels and such in Illustrator.
# Script below generates the vector graphics that we then import into Illustrator. 

# SSRP April 2021
# directories updated to facilitate new TIER folder organization


# uncomment line below and add your directory (direct to the folder prior to the TIER subfolders: command_rep, documents_rep, etc.)
#setwd(*YOUR_DIRECTORY*/SSRP_McGuirk_Burke_2020_JPE/)

rm(list=ls())
library(lfe)
library(tidyverse)
library(readstata13)


# read in and organize data
dt <- read.dta13("input_rep/cell_data.dta")
dt <- mutate(dt,cell=paste(x,y,sep="_"))

# read in neighbor price data
price <- read.csv("input_rep/NeighborPrice0_500km.csv")
dt <- left_join(dt,price,by=c('year','x','y'))
dt <- dplyr::arrange(dt,cell,year)  #sort data


#write a function to generate lags. data need to be sorted
shift <- function(x,shift_by,y) {
  ana <- rep(NA,abs(shift_by))
  if (shift_by > 0 ) {
    out <- c(x[(1+shift_by):length(x)],ana)
    ck <- c(y[(1+shift_by):length(y)],ana) 
    out[y!=ck] <- NA } #fills in NAs when you inappropriately drag observations across cells
  else if (shift_by < 0 ) {
    out <- c(ana,x[1:(length(x)+shift_by)])
    ck <- c(ana,y[1:(length(y)+shift_by)])
    out[y!=ck] <- NA }
  else 
    out <- x
  return(out)      
}

# now use this in shameful loop to generate lags. 
ann <- c("0.100km",   "100.200km", "200.300km", "300.400km", "400.500km")  #annuli we want to use
for (v in ann) {
  for (i in c(-2:0)) {  #generate lags
    dt[,paste0('z_CPI_',v,'_lag',abs(i))] <- shift(dt[,paste0('z_CPI_',v)],i,dt$cell)
    dt[,paste0('z_PPI_',v,'_lag',abs(i))] <- shift(dt[,paste0('z_PPI_',v)],i,dt$cell)
  }}
for (i in c(-2:0)) {  #generate lags
  dt[,paste0('z_CPI_lag',abs(i))] <- shift(dt$z_CPI,i,dt$cell)
  dt[,paste0('z_PPI_lag',abs(i))] <- shift(dt$z_PPI,i,dt$cell)
}



pre="z"
lag=2
lead=0
nms <- expand.grid(0:lag,ann)  #vars defined above!
ll <- c(paste0(pre,"_PPI_lag",0:lag),paste0(pre,"_PPI_",nms[,2],"_lag",nms[,1]),paste0(pre,"_CPI_lag",0:lag)) 
lgs <- paste(ll,collapse="+")


for (y in c("ucdp_10","acled_riot_67")) {
  fmla <- as.formula(paste0(y," ~",lgs, "+ as.factor(country):year | cell | 0 | cell + cy "))
  mod <- felm( fmla, data=dt)
  out <- list()
  vars = 1:(lag+lead+1)  #own effect
  out[[1]] <- data.frame(b=sum(mod$coefficients[vars,1]), se=sqrt(sum(vcov(mod)[vars,vars])) )  #store cumulative effects
  for (v in 1:length(ann)) {
    vars <- grep(ann[v],rownames(mod$coefficients))
    out[[v+1]] <- data.frame(b=sum(mod$coefficients[vars,1]), se=sqrt(sum(vcov(mod)[vars,vars])) )  #store cumulative effects
  }
  out <- matrix(unlist(out),nrow=length(out),byrow=T)
  if (y=="ucdp_10") {outFactor <- out} else {outOutput <- out}
}

pdf(file="command_rep/analysis/output_rep/plots/Figure6b.pdf",height=5,width=10,useDingbats = F)
par(mfrow=c(1,2), mar=c(4,4,1,1))
out <- outFactor
nn <- 1:dim(out)[1]
plot(1,type="n",xlab="Spatial lag (km)",ylab="dy/dp",xaxt="n",las=1,ylim=c(-0.02,0.02),xlim=c(1,dim(out)[1]),main="Factor, PPI")
abline(h=0,lty=2)
axis(1,at=1:6,labels = c("own","0-100","100-200","200-300","300-400","400-500"))
segments(nn,out[,1]-1.96*out[,2],nn,out[,1]+1.96*out[,2])
points(nn,out[,1],pch=19,col="black",cex=1.5)
out <- outOutput
nn <- 1:dim(out)[1]
plot(1,type="n",xlab="Spatial lag (km)",ylab="dy/dp",xaxt="n",las=1,ylim=c(-0.02,0.02),xlim=c(1,dim(out)[1]),main="Output, PPI")
abline(h=0,lty=2)
axis(1,at=1:6,labels = c("own","0-100","100-200","200-300","300-400","400-500"))
segments(nn,out[,1]-1.96*out[,2],nn,out[,1]+1.96*out[,2])
points(nn,out[,1],pch=19,col="black",cex=1.5)
dev.off()



# Now make cumulative plots, where you run regs adding lags sequentially, re-estimating regression each time
outFactor <- outOutput <- c()
for (k in 0:length(ann)) {
  if (k==0) {
    ll <- c(paste0(pre,"_PPI_lag",0:lag),paste0(pre,"_CPI_lag",0:lag)) 
  } else {
    loc = 1:(3+3*(k-1))
    ll <- c(paste0(pre,"_PPI_lag",0:lag),paste0(pre,"_PPI_",nms[loc,2],"_lag",nms[loc,1]),paste0(pre,"_CPI_lag",0:lag)) 
  }
  lgs <- paste(ll,collapse="+")
  for (y in c("ucdp_10","acled_riot_67")) {
    fmla <- as.formula(paste0(y," ~",lgs, "+ as.factor(country):year | cell | 0 | cell + cy "))
    mod <- felm( fmla, data=dt)
    out <- list()
    vars = grep("z_PPI",rownames(mod$coefficients))
    out <- data.frame(b=sum(mod$coefficients[vars,1]), se=sqrt(sum(vcov(mod)[vars,vars])) )  #store cumulative effects
    if (y=="ucdp_10") {outFactor <- rbind(outFactor,out) } else {outOutput <- rbind(outOutput,out)}
  }  
  print(k)
}

plotcumul <- function(toplot,name) {
  cihi <- toplot$b + 1.96*toplot$se
  cilo <- toplot$b - 1.96*toplot$se
  plot(1,type="n",xlim=c(0,5),ylim=c(-0.03,0.03),las=1,xlab="cumul. effect at spatial lag (km)",ylab="dy/dp",xaxt="n")
  abline(h=0,lty=2)
  polygon(c(0:5,5:0),c(cilo,rev(cihi)),col="grey",border="NA")
  lines(0:5,toplot$b)
  points(0:5,toplot$b,pch=21,col="black")
  points(0,toplot$b[1],pch=19,col="black")
  mtext(text=name,side=3,line=-1,adj=0,cex=1)
  axis(1,at=0:5,labels = c("own","0-100","100-200","200-300","300-400","400-500"))
}

pdf(file="command_rep/analysis/output_rep/plots/Figure6a.pdf",height=5,width=10,useDingbats = F)
par(mfrow=c(1,2), mar=c(4,4,1,1))
plotcumul(outFactor,"Factor conflict, PPI")
plotcumul(outOutput,"Output conflict, PPI")
dev.off()
