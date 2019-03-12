#############################################################################################
# Filename: PelletEUtrade.R
#
# Author: Fabian Schipfer (FS)
# Created: 04-Febr-2017
#
# Version: 1.0
#
# Changed on: 11-Mar-2019 by FS
#         preparation for re-submission in the Journal of Commodity Markets: documentation updated;
#         Prices and trade flows as available on 11-Mar-2019: 
#             trade data incl. Nov.2018 /FR incl. Dec.2018, prices FR incl. Sept.2018, It incl. Jan.2019, DE&AT incl. Feb.2019
# Run on: RStudio Version 1.1.383 with R version 3.4.2 (2017-09-28)
#############################################################################################
#
# All calculations, graphics and tables presented in manuscript
#
#
# Data files required:   focusset2019-03-11.rds, pricesTS2019-03-11.rds
# Subfunctions:          none
# R-files required:      none
# other files:           none
# Problems:              none
#############################################################################################
rm(list=ls())
# Loading function libraries (can also be done manually in RStudio in "Packages")
library(openxlsx)
library(urca)
library(tseries)
library(forecast)
library(zoo)
# Set your working directory
mainDir<-"SET YOUR DIRECTORY WITCH INCLUDES THE DATA FILES HERE"
setwd(mainDir)

# Load data sets from working directory
pnl<-readRDS("focusset2019-03-11.rds") #these are the trade streams downloaded from Eurostat and compiled to a panel data set
prm <- readRDS("pricesTS2019-03-11.rds") # these are the prices collected from various sources (see Manuscript)
pnl <- pnl[!pnl$obsTime == "2018-12-01",] # November 2018 is the most current data - this is to get rid of the zeros for December 2018

# prices TS ranges for analysis of various time periods
Y2000to2006 <- c(1:60)
Y2006to2012 <- c(61:132)
Y2012to2015 <- c(133:168)
Y2015toNow <- c(169:218)
Yall <- c(1:218)
Y2012toNow <- c(133:218)

# Matrix for unique identification of Partner-Reporter couples
rel<-matrix(nrow=6,ncol=3)
rel[,1]<-c("DE","IT","AT","IT","FR","IT")
rel[,2]<-c("AT","AT","DE","DE","DE","FR")
rel[,3]<-c("A","B","C","D","E","F")

#############################################################################################
#
# Create summary tables 
#
# for trade flows and for pellet prices
#############################################################################################

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

# for pellet prices 
prm[,4]<-as.numeric(prm[,4])
xl<-Y2012toNow
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


#############################################################################################
#
# Data analysis of prices time series
#
# Stationary tests of prices time series
# Stationary tests of first difference of prices time series
# Cointegration tests for Austria<->Germany prices time series
#############################################################################################

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

# New Cointegration test of remaining prices time series 
#(data quality of others considered too low - see manuscript)

TSGM <- list(Y2000to2006, Y2006to2012, Y2012toNow, Yall) #combine time segments to list

COin<-data.frame(Countries=c("Austria","Germany"),Austria=NA,Germany=NA)
LCOin <- list(COin, COin, COin, COin)

COin2 <- data.frame(Integration=c(rep("Austria to Germany",4),rep("Germany to Austria",4)),
                    Time_periode = c(rep(c("2000 to 2006", "2006 to 2012", "2012 to Jul.2018","2000 to Jul.2018"),2)),
                    Parameter = rep(NA,8))

for(t in 1:4){
  n<-lm(prm[TSGM[[t]],2]~prm[TSGM[[t]],3])$residuals #check if correct order AND what to do if TS not I(1)
  m<-lm(prm[TSGM[[t]],3]~prm[TSGM[[t]],2])$residuals #check if correct order AND what to do if TS not I(1)
  AlphaN<-round(pp.test(n)$statistic,3)
  AlphaM<-round(pp.test(m)$statistic,3)
  LAGN<-pp.test(n)$parameter
  LAGM<-pp.test(m)$parameter
  mystarsN<-ifelse(pp.test(n)$p.value==.01, "***", ifelse(pp.test(n)$p.value<.05, "**",ifelse(pp.test(n)$p.value<.01, "*", "")))
  mystarsM<-ifelse(pp.test(m)$p.value==.01, "***", ifelse(pp.test(m)$p.value<.05, "**",ifelse(pp.test(m)$p.value<.01, "*", "")))
  LCOin[[t]][1,3]<-paste(AlphaN,"(",LAGN,")",mystarsN,sep="")
  LCOin[[t]][2,2]<-paste(AlphaM,"(",LAGM,")",mystarsM,sep="")
  COin2[t,3] <- paste(AlphaN,"(",LAGN,")",mystarsN,sep="")
  COin2[4+t,3] <- paste(AlphaM,"(",LAGM,")",mystarsM,sep="")
}


write.xlsx(COin2,"Table5_v2.xlsx",sheetName="summary")

#############################################################################################
#
# Data analysis of prices time series
#
# Find best fitting ARIMA and ARIMAX (with prices as eXogenous variable) models
#############################################################################################

fits<-list(fA=0,fB=0,fC=0,fD=0,fE=0,fF=0)
fitsX<-list(fA=0,fB=0,fC=0,fD=0,fE=0,fF=0)
boxs<-list(bA=0,bB=0,bC=0,bD=0,bE=0,bF=0)
boxsX<-list(bA=0,bB=0,bC=0,bD=0,bE=0,bF=0)

for(f in 1:6){
  #f <- 1
  fl<-c("A","B","C","D","E","F")[f]
  pnlf<-pnl[pnl$flw==fl,][1:74,] #update
  
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
  finsX$xreg[[f]]<-round(tosum$coef["xreg"])  
  finsX$BIC[[f]]<-round(tosum$bic,2)  
  MASE<-round(mean(abs(ts(pnlf$netexp,frequency=12)-fitted(tosum)),na.rm=T)/mean(abs(ts(pnlf$netexp,frequency=12)[2:48]-ts(pnlf$netexp,frequency=12)[1:47]),na.rm=T),2)
  sMASE<-round(mean(abs(ts(pnlf$netexp,frequency=12)[13:48]-fitted(tosum)[13:48]),na.rm=T)/mean(abs(ts(pnlf$netexp,frequency=12)[13:48]-ts(pnlf$netexp,frequency=12)[1:36]),na.rm=T),2)
  finsX$MASE[[f]]<-ifelse(ss==0,MASE,sMASE)
}
write.xlsx(fins,"Table6.xlsx",sheetName="NetTrade")
write.xlsx(finsX,"Table7.xlsx",sheetName="NetTrade_prices")


#############################################################################################
#
# Graphs
#
# Create Graph for prices time series
# graph for trade flow time series second
#############################################################################################

prmOLD <- prm
prmOLD$IT[217] <- NA

png(paste("Figure1.jpeg",sep=""),
    width=12*300,
    height=6*300,
    res=600,
    pointsize=5)

farben<-c("#a1dab4","#41b6c4","#253494","#2c7fb8")
xl<-Y2012toNow
plot(ts(prmOLD[xl,2],start=2012,frequency=12),ylim=c(200,300),
     type="l",xaxt="n",xlab="",ylab="",lwd=2,col=farben[1])

mtext(seq(2012,2018,1),side=1,line=0.4,at=seq(2012.5,2018.5,1),cex=1.3)
mtext("Pellet prices (nominal,excl. VAT) [Euro/t]",side=2,line=2,cex=1.5)
abline(v=seq(2012,2019,1),lty=2)

lines(ts(prmOLD[xl,3],start=2012,frequency=12),col=farben[2],lwd=2)
#points(ts(prmOLD[xl,4],start=2012,frequency=12),col=farben[3],cex=2,pch="*")
lines(ts(prmOLD[xl,5],start=2012,frequency=12),col=farben[4],lwd=2)
lines(ts(prmOLD[xl,4],start=2012,frequency=12),col=farben[3],lwd=2)

legend(2012,300,colnames(prmOLD)[c(4,5,3,2)],col=farben[c(3,4,2,1)],lty=1,lwd=2,cex=1.5,title="Countries")
dev.off()

# trade flows

png(paste("Figure2.jpeg",sep=""),
    width=12*300,
    height=6*300,
    res=600,
    pointsize=5)

par(mfrow=c(2,3),oma=c(2,2,2,4),cex.axis=2)

for(f in 1:6){
  fl<-c("A","B","C","D","E","F")[f]
  plot(ts(pnl[pnl$flw==fl,]$netexp,start=2012,frequency=12)/1000,las=2,type="l",xaxt="n",xlab="",ylab="")
  
  abline(v=seq(2013,2019,1),lty=2)
  mtext(seq(2012,2018,1),side=1,line=0.4,at=seq(2012.5,2018.5,1),cex=1)
  title(paste(pnl[pnl$flw==fl,][1,]$PARTNER,"<-",pnl[pnl$flw==fl,][1,]$REPORTER,seq=""),cex.main=1.8)
}
dev.off()

#some more analysis in the Paper
#sum(pnl[pnl$flw == "A",]$netexp)
#sum(pnl[pnl$flw == "C",]$netexp)

