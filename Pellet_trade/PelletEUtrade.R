#############################################################################################
# Filename: PelletEUtrade.R
#
# Author: Fabian Schipfer (FS)
# Created: 04-Febr-2017
#
# Version: 1.0
#
# Changed on: 23-Apr-2017 by (FS)
#         preparation for submission in Energy Economics: documentation updated; 
# Run on: RStudio Version 0.98.1049 with R version 3.1.1 (2014-07-10)
#############################################################################################
#
# All calculations, graphics and tables presented in manuscript
#
#
# Data files required:   trade.rds, prices.rds
# Subfunctions:          none
# R-files required:      none
# other files:           none
# Problems:              none
#############################################################################################
rm(list=ls())
# Loading function libraries (can also be done manually in RStudio in "Packages")
library("openxlsx", lib.loc="~/R/win-library/3.1")
library(urca)
library(tseries)
library("forecast", lib.loc="~/R/win-library/3.1")
library("openxlsx", lib.loc="~/R/win-library/3.1")
library("zoo", lib.loc="~/R/win-library/3.1")
# Set your working directory
mainDir<-"~/your working directory here"
setwd(mainDir)
# Load data sets from working directory
pnl<-readRDS("trade.rds") #these are the trade streams downloaded from Eurostat and compiled to a panel data set
prmOLD<-readRDS("prices.rds") #these are the wood pellet prices
#prices.rds contains the prices which are publically available (France and Austria). Please
#contact DEPV for German prices and AIEL for Italien prices and subtract the taxes indicated in the manuscript

# For Italien prices, which are available in a 2-3 months time step, closest values are used for the months 
#which are not available
prm<-prmOLD
itp<-prm[prm$month<"2016-12-01"&prm$month>"2011-12-01",4]
prm[prm$month<"2016-12-01"&prm$month>"2011-12-01",4]<-
  round(c(242,as.numeric(na.locf(itp))),2) #closest value plus january 2012 at 242,-
prm[,4]<-as.numeric(prm[,4])

# Matrix for unique identification of Parter-Reporter couples ####
rel<-matrix(nrow=6,ncol=3)
rel[,1]<-c("DE","IT","AT","IT","FR","IT")
rel[,2]<-c("AT","AT","DE","DE","DE","FR")
rel[,3]<-c("A","B","C","D","E","F")

# Data summary ####
# for trade streams first
cmpr<-data.frame(Partner=rep(NA,6),Reporter=NA,Minimum=NA,Maximum=NA,Mean=NA,StandardDeviation=NA)
cmpr[,c(1,2)]<-rel  
Lcmpr<-list("expvalue"=cmpr,"impvalue"=cmpr,
            "expshre"=cmpr,
            "ntexp"=cmpr,"PARimpshre"=cmpr)

Lval<-list(pnl$physexp,pnl$physimp,pnl$expshare,pnl$netexp,pnl$PARimpshare)
for(i in 1:5){
  cmpr[,3]<-aggregate(Lval[[i]],list(pnl$PARTNER,pnl$REPORTER),min)$x
  cmpr[,4]<-aggregate(Lval[[i]],list(pnl$PARTNER,pnl$REPORTER),max)$x
  cmpr[,5]<-aggregate(Lval[[i]],list(pnl$PARTNER,pnl$REPORTER),mean)$x
  cmpr[,6]<-aggregate(Lval[[i]],list(pnl$PARTNER,pnl$REPORTER),sd)$x
  cmpr[,3:6]<-round(cmpr[,3:6],1)
  Lcmpr[[i]]<-cmpr
}
write.xlsx(Lcmpr$ntexp,"Table3.xlsx")

# for pellet prices second
prm[,4]<-as.numeric(prm[,4])
xl<-c(145:204)
cmprX<-data.frame(Set=c(colnames(prm)[2:5],"AT-DE","AT-IT","DE-IT","DE-FR","FR-IT"),
                  Minimum=NA,Maximum=NA,Mean=NA,StandardDeviation=NA)
for(i in 1:4){
  cmprX[i,2]<-round(min(prm[xl,1+i],na.rm=T),2)
  cmprX[i,3]<-round(max(prm[xl,1+i],na.rm=T),2)
  cmprX[i,4]<-round(mean(prm[xl,1+i],na.rm=T),2)
  cmprX[i,5]<-round(sd(prm[xl,1+i],na.rm=T),2)
}
for(i in 5:9){
  fl<-c("A","B","D","E","F")[i-4]
  pnlf<-pnl[pnl$flw==fl,]
  PAREP<-pnlf$PAR-pnlf$REP
  cmprX[i,2]<-round(min(PAREP,na.rm=T),2)
  cmprX[i,3]<-round(max(PAREP,na.rm=T),2)
  cmprX[i,4]<-round(mean(PAREP,na.rm=T),2)
  cmprX[i,5]<-round(sd(PAREP,na.rm=T),2)
}
write.xlsx(cmprX,"Table2.xlsx",sheetName="summary")

#Data analysis####
# Stationary tests of prices time series
# First difference of prices time series
M<-matrix(nrow=dim(prm[xl,])[1]-1,ncol=4)
colnames(M)<-c("AT","DE","FR","IT")
M[,1]<-diff(prm[xl,2])
M[,2]<-diff(prm[xl,3])
M[,3]<-diff(prm[xl,5])
M[,4]<-diff(prm[xl,4])
# Original prices time series
N<-matrix(nrow=dim(prm[xl,])[1],ncol=4)
colnames(N)<-c("AT","DE","FR","IT")
N[,1]<-prm[xl,2]
N[,2]<-prm[xl,3]
N[,3]<-prm[xl,5]
N[,4]<-prm[xl,4]
# Testing for unit roots with Phillips-Perron test
prADF<-data.frame(Stream=colnames(M),Levels=NA,First_difference=NA)
for(i in 1:4){
  n<-N[,i][!is.na(N[,i])]
  m<-M[,i][!is.na(M[,i])]
  AlphaN<-round(pp.test(n)$statistic,3)
  AlphaM<-round(pp.test(m)$statistic,3)
  LAGN<-pp.test(n)$parameter
  LAGM<-pp.test(m)$parameter
  mystarsN<-ifelse(pp.test(n)$p.value==.01, "***", ifelse(pp.test(n)$p.value<.05, "**",ifelse(pp.test(n)$p.value<.01, "*", "")))
  mystarsM<-ifelse(pp.test(m)$p.value==.01, "***", ifelse(pp.test(m)$p.value<.05, "**",ifelse(pp.test(m)$p.value<.01, "*", "")))
  prADF[i,2]<-paste(AlphaN,"(",LAGN,")",mystarsN,sep="")
  prADF[i,3]<-paste(AlphaM,"(",LAGM,")",mystarsM,sep="")
}
write.xlsx(prADF,"Table4.xlsx",sheetName="summary")

# Cointegration test of remaining prices time series 
#(data quality of others considered too low - see manuscript)
COin<-data.frame(Countries=c("Austria","Germany"),Austria=NA,Germany=NA)
n<-lm(M[,1]~M[,2])$residuals
m<-lm(M[,2]~M[,1])$residuals
AlphaN<-round(pp.test(n)$statistic,3)
AlphaM<-round(pp.test(m)$statistic,3)
LAGN<-pp.test(n)$parameter
LAGM<-pp.test(m)$parameter
mystarsN<-ifelse(pp.test(n)$p.value==.01, "***", ifelse(pp.test(n)$p.value<.05, "**",ifelse(pp.test(n)$p.value<.01, "*", "")))
mystarsM<-ifelse(pp.test(m)$p.value==.01, "***", ifelse(pp.test(m)$p.value<.05, "**",ifelse(pp.test(m)$p.value<.01, "*", "")))
COin[1,3]<-paste(AlphaN,"(",LAGN,")",mystarsN,sep="")
COin[2,2]<-paste(AlphaM,"(",LAGM,")",mystarsM,sep="")
write.xlsx(COin,"Table5.xlsx",sheetName="summary")

#ARIMA models ####
# Find best fitting ARIMA and ARIMAX (with prices as eXogenous variable) models
fits<-list(fA=0,fB=0,fC=0,fD=0,fE=0,fF=0)
fitsX<-list(fA=0,fB=0,fC=0,fD=0,fE=0,fF=0)
boxs<-list(bA=0,bB=0,bC=0,bD=0,bE=0,bF=0)
boxsX<-list(bA=0,bB=0,bC=0,bD=0,bE=0,bF=0)

for(f in 1:6){
  fl<-c("A","B","C","D","E","F")[f]
  pnlf<-pnl[pnl$flw==fl,][1:56,]
  
  fits[[f]]<-auto.arima(ts(pnlf$netexp,frequency=12),stepwise=FALSE,approximation=FALSE)
  JBL<-ifelse(fits[[f]]$arma[c(3)]+fits[[f]]$arma[c(7)]+fits[[f]]$arma[c(4)]==0,10,24)
  Fdf<-fits[[f]]$arma[c(1)]+fits[[f]]$arma[c(2)]+fits[[f]]$arma[c(3)]+fits[[f]]$arma[c(4)]
  boxs[[f]]<-Box.test(residuals(fits[[f]]),lag=JBL,fitdf=Fdf,type="Ljung")
  
  fitsX[[f]]<-auto.arima(ts(pnlf$netexp,frequency=12),xreg=ts(c(NA,diff(pnlf$PAR-pnlf$REP)),frequency=12),stepwise=FALSE,approximation=FALSE)
  JBLX<-ifelse(fitsX[[f]]$arma[c(3)]+fitsX[[f]]$arma[c(7)]+fitsX[[f]]$arma[c(4)]==0,10,24)
  FdfX<-fitsX[[f]]$arma[c(1)]+fitsX[[f]]$arma[c(2)]+fitsX[[f]]$arma[c(3)]+fitsX[[f]]$arma[c(4)]
  boxsX[[f]]<-Box.test(residuals(fitsX[[f]]),lag=JBLX,fitdf=FdfX,type="Ljung")
}

# Create tables for comparison
fins<-data.frame(Trade=paste(rel[,1],"<-",rel[,2],sep=""),
                 Model_type="NA",const.="NA",BIC="NA",MASE="NA",stringsAsFactors=FALSE)
finsX<-data.frame(Trade=paste(rel[,1],"<-",rel[,2],sep=""),
                  Model_type="NA",const.="NA",xreg="NA",BIC="NA",MASE="NA",stringsAsFactors=FALSE)

# Write Table without exogenous variables first
for(f in 1:6){
  tosum<-fits[[f]]
  fl<-c("A","B","C","D","E","F")[f]
  pnlf<-pnl[pnl$flw==fl,]  
  #Code for model description abbreviations
  nonseas<-paste("(",tosum$arma[c(1)],",",tosum$arma[c(6)],",",tosum$arma[c(2)],")",sep="")
  seas<-paste("(",tosum$arma[c(3)],",",tosum$arma[c(7)],",",tosum$arma[c(4)],")",sep="")
  per<-paste("[",tosum$arma[5],"]",sep="")
  ss<-tosum$arma[c(3)]+tosum$arma[c(7)]+tosum$arma[c(4)]
  const<-ifelse(is.na(tosum$coef["intercept"]),ifelse(is.na(tosum$coef["drift"]),NA,"drift"),"intercept")
  descr<-ifelse(is.na(tosum$coef["intercept"]),ifelse(is.na(tosum$coef["drift"]),"","with drift"),"with non-zero mean")  
  #Write values of modelling effort into tables
  fins$Model_type[[f]]<-ifelse(ss>0,
                               paste("ARIMA",nonseas,seas,per,descr,sep=""),
                               paste("ARIMA",nonseas,descr,sep=""))
  fins$const.[[f]]<-ifelse(is.na(const),"",round(tosum$coef[const]))
  fins$BIC[[f]]<-round(tosum$bic,2)  
  MASE<-round(mean(abs(ts(pnlf$netexp,frequency=12)-fitted(tosum)),na.rm=T)/mean(abs(ts(pnlf$netexp,frequency=12)[2:48]-ts(pnlf$netexp,frequency=12)[1:47]),na.rm=T),2)
  sMASE<-round(mean(abs(ts(pnlf$netexp,frequency=12)[13:48]-fitted(tosum)[13:48]),na.rm=T)/mean(abs(ts(pnlf$netexp,frequency=12)[13:48]-ts(pnlf$netexp,frequency=12)[1:36]),na.rm=T),2)
  fins$MASE[[f]]<-ifelse(ss==0,MASE,sMASE)
}

# and table with exogenous variables second
for(f in 1:6){
  tosum<-fitsX[[f]]
  fl<-c("A","B","C","D","E","F")[f]
  pnlf<-pnl[pnl$flw==fl,]
  #Code for model description abbreviations
  nonseas<-paste("(",tosum$arma[c(1)],",",tosum$arma[c(6)],",",tosum$arma[c(2)],")",sep="")
  seas<-paste("(",tosum$arma[c(3)],",",tosum$arma[c(7)],",",tosum$arma[c(4)],")",sep="")
  per<-paste("[",tosum$arma[5],"]",sep="")
  ss<-tosum$arma[c(3)]+tosum$arma[c(7)]+tosum$arma[c(4)]
  const<-ifelse(is.na(tosum$coef["intercept"]),ifelse(is.na(tosum$coef["drift"]),NA,"drift"),"intercept")
  descr<-ifelse(is.na(tosum$coef["intercept"]),ifelse(is.na(tosum$coef["drift"]),"","with drift"),"with non-zero mean")  
  #Write values of modelling effort into tables
  finsX$Model_type[[f]]<-ifelse(ss>0,
                                paste("ARIMA",nonseas,seas,per,descr,sep=""),
                                paste("ARIMA",nonseas,descr,sep=""))
  finsX$const.[[f]]<-ifelse(is.na(const),"",round(tosum$coef[const]))
  finsX$xreg[[f]]<-round(tosum$coef["ts(c(NA, diff(pnlf$PAR - pnlf$REP)), frequency = 12)"])  
  finsX$BIC[[f]]<-round(tosum$bic,2)  
  MASE<-round(mean(abs(ts(pnlf$netexp,frequency=12)-fitted(tosum)),na.rm=T)/mean(abs(ts(pnlf$netexp,frequency=12)[2:48]-ts(pnlf$netexp,frequency=12)[1:47]),na.rm=T),2)
  sMASE<-round(mean(abs(ts(pnlf$netexp,frequency=12)[13:48]-fitted(tosum)[13:48]),na.rm=T)/mean(abs(ts(pnlf$netexp,frequency=12)[13:48]-ts(pnlf$netexp,frequency=12)[1:36]),na.rm=T),2)
  finsX$MASE[[f]]<-ifelse(ss==0,MASE,sMASE)
}
write.xlsx(fins,"Table6.xlsx",sheetName="NetTrade")
write.xlsx(finsX,"Table7.xlsx",sheetName="NetTrade_prices")

#Graphs ####
# Graph for prices time series first
png(paste("Figure1.jpeg",sep=""),
    width=12*300,
    height=6*300,
    res=600,
    pointsize=5)

farben<-c("#a1dab4","#41b6c4","#253494","#2c7fb8")
xl<-c(145:204)
plot(ts(prmOLD[xl,2],start=2012,frequency=12),ylim=c(200,300),
     type="l",xaxt="n",xlab="",ylab="",lwd=2,col=farben[1])

mtext(seq(2012,2016,1),side=1,line=0.4,at=seq(2012.5,2016.5,1),cex=1.3)
mtext("Pellet prices (nominal,excl. VAT) [Euro/t]",side=2,line=2,cex=1.5)
abline(v=seq(2012,2017,1),lty=2)

lines(ts(prmOLD[xl,3],start=2012,frequency=12),col=farben[2],lwd=2)
points(ts(prmOLD[xl,4],start=2012,frequency=12),col=farben[3],cex=2,pch="*")
lines(ts(prmOLD[xl,5],start=2012,frequency=12),col=farben[4],lwd=2)

legend(2012,300,colnames(prmOLD)[c(4,5,3,2)],col=farben[c(3,4,2,1)],lty=1,lwd=2,cex=1.5,title="Countries")
dev.off()

# and graph for trade flow time series second
png(paste("Figure2.jpeg",sep=""),
    width=12*300,
    height=6*300,
    res=600,
    pointsize=5)

par(mfrow=c(2,3),oma=c(2,2,2,4),cex.axis=2)

for(f in 1:6){
  fl<-c("A","B","C","D","E","F")[f]
  plot(ts(pnl[pnl$flw==fl,]$netexp,start=2012,frequency=12)/1000,las=2,type="l",xaxt="n",xlab="",ylab="")
  
  abline(v=seq(2013,2016,1),lty=2)
  mtext(seq(2012,2016,1),side=1,line=0.4,at=seq(2012.5,2016.5,1),cex=1)
  title(paste(pnl[pnl$flw==fl,][1,]$PARTNER,"<-",pnl[pnl$flw==fl,][1,]$REPORTER,seq=""),cex.main=1.8)
}
dev.off()