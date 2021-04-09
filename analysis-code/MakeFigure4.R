
# Script to make Figure 4 in McGuirk and Burke 2020
# Workflow is to get figures as close to how we want them to look in R, then add titles, labels and such in Illustrator.
# Script below generates the vector graphics that we then import into Illustrator. 

rm(list=ls())
library(lfe)
library(readstata13)


# function to generate lags. data need to be sorted first, very impt! this is legacy, better implementations in data.table now..
shift <- function(x,shift_by,y) {
  ana <- rep(NA,abs(shift_by))
  if (shift_by > 0 ) {
    out <- c(x[(1+shift_by):length(x)],ana)
    ck <- c(y[(1+shift_by):length(y)],ana) }
  else if (shift_by < 0 ) {
    out <- c(ana,x[1:(length(x)+shift_by)])
    ck <- c(ana,y[1:(length(y)+shift_by)])}
  else 
    out <- x
  out[y!=ck] <- NA  #fills in NAs when you inappropriately drag observations across cells
  return(out)      
}


# read in and organize data
dt <- read.dta13("input_rep/cell_data.dta")
ccode <- read.csv("input_rep/FAOcountryCodes.csv") #using this to generate country codes for plotting purposes
dt <- mutate(dt,cell=paste(x,y,sep="_"))
dt <- dplyr::arrange(dt,cell,year)  #sort data

# use crappy loop to generate lags
for (i in c(-5:-1)) {  
  dt[,paste0('zt_CPI_lag',abs(i))] <- shift(dt$zt_CPI,i,dt$cell)
  dt[,paste0('zt_PPI_lag',abs(i))] <- shift(dt$zt_PPI,i,dt$cell)
  dt[,paste0('z_CPI_lag',abs(i))] <- shift(dt$z_CPI,i,dt$cell)
  dt[,paste0('z_PPI_lag',abs(i))] <- shift(dt$z_PPI,i,dt$cell)
}


# make regression formulas we want to run
fmlaF <- as.formula("ucdp_10 ~ z_CPI*as.factor(country) + z_CPI_lag1*as.factor(country) + z_CPI_lag2*as.factor(country) + z_PPI*as.factor(country) + z_PPI_lag1*as.factor(country) + z_PPI_lag2*as.factor(country) + as.factor(country):year | cell | 0 | cell")
fmlaO <- as.formula("acled_riot_67 ~ z_CPI*as.factor(country) + z_CPI_lag1*as.factor(country) + z_CPI_lag2*as.factor(country) + z_PPI*as.factor(country) + z_PPI_lag1*as.factor(country) + z_PPI_lag2*as.factor(country) + as.factor(country):year | cell | 0 | cell")

cty <- as.character(unique(dt$country))

getcoeff <- function(mod) {  #function to extract for each country the sum of contemporaneous and lagged effects for CPI and PPI separately.  omitted category in regression is Algeria, so need to add those effects to each interaction term (i.e. to each dummy estimate)
  coef <- mod$coefficients
  # algeria is the omitted country here, so z_PPI is the effect for algeria, e.g
  feff <- c()
  ctys <- c()
  v1 <- which(rownames(coef)%in%c("z_CPI","z_CPI_lag1","z_CPI_lag2"))  #base rates - for algeria, which is the ommited group
  v2 <- which(rownames(coef)%in%c("z_PPI","z_PPI_lag1","z_PPI_lag2"))
  for (i in cty) {
    nn <- grep(i,row.names(coef))
    if (i == "Niger")  {nx <- grep("Nigeria",row.names(coef)); nn <- nn[nn%in%nx==F]}  #"niger" picks up "nigeria" too; dunno how to exact match
    if (i == "Guinea")  {nx <- grep("Guinea-Bissau",row.names(coef)); nn <- nn[nn%in%nx==F]}  #same problem for guinea, guinea bissau
    if (length(nn)>0) {
      vars <- nn[2:4]
      vars <- c(vars,v1)
      cpi <- sum(mod$coefficients[vars,1])
      cpise <- sqrt(sum(vcov(mod)[vars,vars]))
      vars <- nn[5:7]
      vars <- c(vars,v2)
      ppi <- sum(mod$coefficients[vars,1])
      ppise <- sqrt(sum(vcov(mod)[vars,vars]))
      out <- c(cpi,cpise,ppi,ppise)
      feff <- rbind(feff,out)
      ctys <- c(ctys,i)
    }
  }
  eff <- data.frame(ctys,feff)
  names(eff) <- c("country","cpi","cpi_se","ppi", "ppi_se")
  eff$cpi_lo <- eff$cpi - 1.96*eff$cpi_se
  eff$ppi_lo <- eff$ppi - 1.96*eff$ppi_se
  eff$cpi_hi <- eff$cpi + 1.96*eff$cpi_se
  eff$ppi_hi <- eff$ppi + 1.96*eff$ppi_se
  # get short labels
  n1 <- c("Gambia, The","Ivory Coast","Zaire","Tanzania, United Republic of")
  n2<-c("Gambia","C\x99te d'Ivoire","Democratic Republic of the Congo","United Republic of Tanzania")
  ctyr <- as.character(ctys)
  for (var in 1:length(n1)) {
    ctyr[ctyr==n1[var]] <- as.character(n2[var])
  }
  tln <- left_join(as.data.frame(ctyr),ccode,by=c('ctyr'='Short.name'))
  tln <- as.character(tln$ISO3)
  eff$tln <- tln
  return(eff)
}

# Factor conflict - regression takes about 30seconds on 2019 macbook pro
modF <- felm(fmlaF,data=dt)
eff <- getcoeff(modF)
effFactor <- eff

#output conflict
keep <- dt$country%in%c("Burundi","Comoros","Equatorial Guinea","Eritrea","Western Sahara","Zaire","Djibouti")==F
modO <- felm(fmlaO,data=dt[keep,])
eff <- getcoeff(modO)
effOutput <- eff


# Now make the figure

effs <- c(-.0046,.0095,.0023,.0072)  # main effects from baseline models
lwd  = 1
lc = "grey"
pdf(file="output_rep/plots/Figure4.pdf",width=8,height=8,useDingbats = F)
par(mfrow=c(2,2),mar=c(2,3,2,1))

# Factor conflict, PPI
eff <- arrange(effFactor,ppi)
ll <- 1:sum(is.na(eff$ppi)==F)
plot(ll,eff$ppi[ll],ylim=c(-0.12,0.05),pch=19,las=1,xaxt="n",xlab="country",ylab="dy/dP")
segments(ll,eff$ppi_lo[ll],ll,eff$ppi_hi[ll],col = lc,lwd=lwd)
abline(h=0,col="grey",lty=2)
abline(h=effs[1],col="red")
text(ll,eff$ppi[ll]-0.01,eff$tln[ll],srt=90,cex=0.5)

# Output conflict, PPI
eff <- arrange(effOutput,ppi)
ll <- 1:sum(is.na(eff$ppi)==F)
plot(ll,eff$ppi[ll],ylim=c(-0.12,0.05),pch=19,las=1,xaxt="n",xlab="country",ylab="dy/dP")
segments(ll,eff$ppi_lo[ll],ll,eff$ppi_hi[ll],col = lc,lwd=lwd)
abline(h=0,col="grey",lty=2)
abline(h=effs[1],col="red")
text(ll,eff$ppi[ll]-0.01,eff$tln[ll],srt=90,cex=0.5)

# Factor conflict CPI
eff <- arrange(effFactor,cpi)
ll <- 1:sum(is.na(eff$cpi)==F)
plot(ll,eff$cpi[ll],ylim=c(-0.05,0.08),las=1,pch=19,xaxt="n",xlab="country",ylab="dy/dP")
segments(ll,eff$cpi_lo[ll],ll,eff$cpi_hi[ll],col = lc,lwd=lwd)
abline(h=0,col="grey",lty=2)
abline(h=effs[3],col="red")
text(ll,eff$cpi[ll]-0.01,eff$tln[ll],srt=90,cex=0.5)

# Output conflict CPI
eff <- arrange(effOutput,cpi)
ll <- 1:sum(is.na(eff$cpi)==F)
plot(ll,eff$cpi[ll],ylim=c(-0.05,0.08),las=1,pch=19,xaxt="n",xlab="country",ylab="dy/dP")
segments(ll,eff$cpi_lo[ll],ll,eff$cpi_hi[ll],col = lc,lwd=lwd)
abline(h=0,col="grey",lty=2)
abline(h=effs[3],col="red")
text(ll,eff$cpi[ll]-0.01,eff$tln[ll],srt=90,cex=0.5)
dev.off()