
# Script to make Figure 5 in McGuirk and Burke 2020
# Workflow is to get figures as close to how we want them to look in R, then add titles, labels and such in Illustrator.
# Script below generates the vector graphics that we then import into Illustrator. 

rm(list=ls())
library(lfe)
library(tidyverse)
library(readstata13)


# read in and organize data
dt <- read.dta13("input_rep/cell_data.dta")
dt <- mutate(dt,cell=paste(x,y,sep="_"))
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

# shameful loop to generate lags and leads
for (i in c(-5:0)) {  #generate lags
  dt[,paste0('zt_CPI_lag',abs(i))] <- shift(dt$zt_CPI,i,dt$cell)
  dt[,paste0('zt_PPI_lag',abs(i))] <- shift(dt$zt_PPI,i,dt$cell)
  dt[,paste0('z_CPI_lag',abs(i))] <- shift(dt$z_CPI,i,dt$cell)
  dt[,paste0('z_PPI_lag',abs(i))] <- shift(dt$z_PPI,i,dt$cell)
}
for (i in c(1:2)) {  #generate leads
  dt[,paste0('zt_CPI_lead',abs(i))] <- shift(dt$zt_CPI,i,dt$cell)
  dt[,paste0('zt_PPI_lead',abs(i))] <- shift(dt$zt_PPI,i,dt$cell)
  dt[,paste0('z_CPI_lead',abs(i))] <- shift(dt$z_CPI,i,dt$cell)
  dt[,paste0('z_PPI_lead',abs(i))] <- shift(dt$z_PPI,i,dt$cell)
}

# FUNCTION TO RUN MAIN REGRESSION WITH WHATEVER NUMBER OF LEADS AND LAGS ARE DESIRED, so long as these have been generated above
runreg <- function (lag=0,lead=0,y="ucdp_10",tw="yes") {
  if (tw=="yes") { pre="zt"} else {pre="z"}
  if (lead==0) {
    ll <- c(paste0(pre,"_CPI_lag",0:lag),paste0(pre,"_PPI_lag",0:lag)) }
  else {
    ll <- c(paste0(pre,"_CPI_lead",lead:1),paste0(pre,"_CPI_lag",0:lag),paste0(pre,"_PPI_lead",lead:1),paste0(pre,"_PPI_lag",0:lag)) }
  lgs <- paste(ll,collapse="+")
  fmla <- as.formula(paste0(y," ~",lgs, "+ as.factor(country):year | cell | 0 | cell + cy "))
  mod <- felm( fmla, data=dt)
  out <- list()
  vars = 1:(lag+lead+1)  #CPI
  vars1 = (1+lead):(lag+lead+1)  #lag coefficients only, CPI
  out[[1]] <- summary(mod)$coefficients[vars,1:2]  #store individual coefficients
  out[[2]] <- data.frame(b=sum(mod$coefficients[vars1,1]), se=sqrt(sum(vcov(mod)[vars1,vars1])) )  #store cumulative effects
  vars = (lag+lead+2):(2*(lag+lead)+2)  #PPI
  vars1 = (lag+lead+2+lead):(2*(lag+lead)+2)  #lag coefficients only, PPI
  out[[3]] <- summary(mod)$coefficients[vars,1:2]
  out[[4]] <- data.frame(b=sum(mod$coefficients[vars1,1]), se=sqrt(sum(vcov(mod)[vars1,vars1])) )    
  # if (lead>0) {
  #   vars2 = 1:lead
  #   out[[5]] <- data.frame(b=sum(mod$coefficients[vars2,1]), se=sqrt(sum(vcov(mod)[vars2,vars2])) )  #store cumulative effect of leads, CPI
  #   vars2 = (lag+lead+2):(lag+lead+1+lead)
  #   out[[6]] <- data.frame(b=sum(mod$coefficients[vars2,1]), se=sqrt(sum(vcov(mod)[vars2,vars2])) )  #store cumulative effect of leads, CPI
  # }  
  out
}

# now runn regressions with desired lags and leads
llF0 <- llF1 <-  list(); 
oldwarn <- getOption('warn')
options(warn=-1)  #turn off warnings for regs
for (i in 0:5) {
  llF0[[i+1]] <- runreg(lag=i,lead=0,y="ucdp_10",tw="no")
  llF1[[i+1]] <- runreg(lag=i,lead=2,y="ucdp_10",tw="no")
}
options(warn=oldwarn)
regsFactor <- list(llF0,llF1)  #put lists together for easy plotting

llF0 <- llF1 <-  list(); 
oldwarn <- getOption('warn')
options(warn=-1)  #turn off warnings for regs
for (i in 0:5) {
  llF0[[i+1]] <- runreg(lag=i,lead=0,y="acled_riot_67",tw="no")
  llF1[[i+1]] <- runreg(lag=i,lead=2,y="acled_riot_67",tw="no")
}
options(warn=oldwarn)
regsOutput <- list(llF0,llF1)  #put lists together for easy plotting


pdf(file="output_rep/plots/Figure5a.pdf",height=6,width=8,useDingbats = F)
par(mfrow=c(2,2), mar=c(4,4,1,1))

rr <- regsFactor[[1]]
cumulC <- cumulP <- c()
for (i in 1:6) {
  cumulC = rbind(cumulC,rr[[i]][[2]])
  cumulP = rbind(cumulP,rr[[i]][[4]])
}
cumulC$cihi <- cumulC$b + 1.96*cumulC$se
cumulC$cilo <- cumulC$b - 1.96*cumulC$se
cumulP$cihi <- cumulP$b + 1.96*cumulP$se
cumulP$cilo <- cumulP$b - 1.96*cumulP$se

plot(1,type="n",xlim=c(0,5),ylim=c(-0.02,0.02),las=1,xlab="cum. effect after n years",ylab="dy/dp")
abline(h=0,lty=2)
polygon(c(0:5,5:0),c(cumulP$cilo,rev(cumulP$cihi)),col="grey",border="NA")
lines(0:5,cumulP$b)
points(0:5,cumulP$b,pch=21,col="black")
points(2,cumulP$b[3],pch=19)
mtext(text="Factor conflict, PPI",side=3,line=-1,adj=0,cex=1)

plot(1,type="n",xlim=c(0,5),ylim=c(-0.02,0.02),las=1,xlab="cum. effect after n years",ylab="dy/dp")
abline(h=0,lty=2)
polygon(c(0:5,5:0),c(cumulC$cilo,rev(cumulC$cihi)),col="grey",border="NA")
lines(0:5,cumulC$b)
points(0:5,cumulC$b,pch=21,col="black")
points(2,cumulC$b[3],pch=19)
mtext(text="Factor conflict, CPI",side=3,line=-1,adj=0,cex=1)

rr <- regsOutput[[1]]
cumulC <- cumulP <- c()
for (i in 1:6) {
  cumulC = rbind(cumulC,rr[[i]][[2]])
  cumulP = rbind(cumulP,rr[[i]][[4]])
}
cumulC$cihi <- cumulC$b + 1.96*cumulC$se
cumulC$cilo <- cumulC$b - 1.96*cumulC$se
cumulP$cihi <- cumulP$b + 1.96*cumulP$se
cumulP$cilo <- cumulP$b - 1.96*cumulP$se

plot(1,type="n",xlim=c(0,5),ylim=c(-0.02,0.02),las=1,xlab="cum. effect after n years",ylab="dy/dp")
abline(h=0,lty=2)
polygon(c(0:5,5:0),c(cumulP$cilo,rev(cumulP$cihi)),col="grey",border="NA")
lines(0:5,cumulP$b)
points(0:5,cumulP$b,pch=21,col="black")
points(2,cumulP$b[3],pch=19)
mtext(text="Output conflict, PPI",side=3,line=-1,adj=0,cex=1)

plot(1,type="n",xlim=c(0,5),ylim=c(-0.02,0.02),las=1,xlab="cum. effect after n years",ylab="dy/dp")
abline(h=0,lty=2)
polygon(c(0:5,5:0),c(cumulC$cilo,rev(cumulC$cihi)),col="grey",border="NA")
lines(0:5,cumulC$b)
points(0:5,cumulC$b,pch=21,col="black")
points(2,cumulC$b[3],pch=19)
mtext(text="Output conflict, CPI",side=3,line=-1,adj=0,cex=1)
dev.off()


# THIS FUNCTION PLOTS THE INDIVIDUAL AND CUMULATIVE EFFECTS
loc <- -5:2
plotindividual <- function(toplot,lag=-5,lead=2,j=1) {
  b <- toplot[[j]][,1]; se <- toplot[[j]][,2]
  cilo = b - 1.96*se
  cihi = b + 1.96*se
  xm = max(loc)
  vars = -lag:lead
  plot(vars,rev(b),las=1,ylim=c(-.015,.015),xlim=c(min(loc)-0.5,max(loc)+1.3),ylab="dy/dp",xlab="",xaxt="n",pch=19,cex=2)
  rect(xm+0.5,-0.2,xm+1.5,0.2,col="grey95",border=NA)
  abline(h=0,lty=2)
  abline(v=0,lty=3)
  segments(vars,rev(cilo),vars,rev(cihi))
  #plot cumulative effect of lags
  bb <- toplot[[j+1]]$b
  ses <- toplot[[j+1]]$se
  segments(xm+1,bb-1.96*ses,xm+1,bb+1.96*ses,col="blue")
  points(xm+1,bb,pch=19,col="blue",cex=2)
  nn = c(paste0("t-",5:1),"t","t+1","t+2","CumL.")
  axis(1,at=c(vars,xm+1),nn,cex.axis=0.8)
}


f=2; j=5
pdf(file="output_rep/plots/Figure5b.pdf",height=6,width=8,useDingbats = F)
par(mfrow=c(2,2), mar=c(4,4,1,1))
plotindividual(regsFactor[[2]][[6]],lag=j,lead=f,j=3)  #j=3 is PPI, j=1 is CPI based on how lists were generated
plotindividual(regsFactor[[2]][[6]],lag=j,lead=f,j=1)
plotindividual(regsOutput[[2]][[6]],lag=j,lead=f,j=3)
plotindividual(regsOutput[[2]][[6]],lag=j,lead=f,j=1)
dev.off()