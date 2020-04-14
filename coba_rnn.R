library(forecast)
library(fable)
library(forecastHybrid)
library(nnet)
library(readxl)
library(GA)
library(Metrics)
library(tidyverse)
library(TSrepr)
library(smooth)
library(forecTheta)
library(thief)
library(svrpath)
library(e1071)
library(NMOF)


Dataset_Surabaya <- read_excel("C:/Users/asus/OneDrive - Institut Teknologi Sepuluh Nopember/Kuliah/Thesis/Dataset_Surabaya.xlsx")
data_outflow<-data.frame(tahun=Dataset_Surabaya[["Tahun"]],
                         bulan=Dataset_Surabaya[["Bulan"]],
                         y=Dataset_Surabaya[["K20000"]])
data_outflow$bulan<-match(data_outflow$bulan,month.abb)
data_outflow<-na.omit(data_outflow)
head<-head(data_outflow)
tail<-tail(data_outflow)

daftar.mape.mae.smape<-data.frame(fh=NULL,mape=NULL,mae=NULL,smape=NULL,maape=NULL)
#daftar.mae<-data.frame(fh=NULL,mae=NULL)
#daftar.smape<-data.frame(fh=NULL,smape=NULL)
#daftar.mape<-rbind(daftar.mape,data.frame(fh=21,mape=12))

data_outflow.ts<-ts(data_outflow[["y"]])

dataset_outflow <- ts(data_outflow[["y"]],start=c(head[1,1], head[1,2]), end=c(2019, 12), frequency=12)
#myts <- ts(data_outflow_10000, frequency=12)
myts<-window(dataset_outflow,start=c(2014,1),end=c(2017,12))
myts_2018<-window(dataset_outflow,start=c(2018,1),end=c(2018,12))


require(RSNNS)
require(quantmod)
require(Metrics)
library(forecast)
library(tsDyn)
library(vars)
library(DMwR)
slog<-(read.csv("logistic-x.csv",F))
slog<-myts[1:length(myts)]

input.df<-data.frame(y=slog)
#input.df<-cbind(input.df,slog)

#select lag
var.select<-VARselect(slog)
optimum.aic<-var.select[["selection"]][["AIC(n)"]]
optimum.aic<-12

#m <- cbind(1, 1:7)
#m<-cbind(m,3:10)

for (a in c(1:optimum.aic)) {
  input.df<-cbind(input.df,Lag(slog,k=a))
}

input.df<-input.df[-(1:optimum.aic),]


#on looping next forecast horizon --> start from here / skip the training

fh<-12

for(iter in c(1:fh))
{
  input.df.scaled<-scale(input.df)
  input.df.unscale<-unscale(input.df.scaled,norm.data = input.df.scaled)
  
  store.df<-input.df.scaled
  inputs<-input.df.scaled[,2:(optimum.aic+1)]
  outputs<-input.df.scaled[,1]
  
  if(iter==1)
  {
    fit<-elman(x=inputs,
               y=outputs,
               size=c(5,10,3),
               maxit=500)
    plotIterativeError(fit)
    
    print(Metrics::mape(unscale(outputs,norm.data=input.df.scaled),
                  unscale(fit$fitted.values,norm.data=input.df.scaled)))
  }
  
  
  
  

  #ts.inputs<-ts(inputs)
  
  #rownames(input.df) <- NULL
  #fff<-data.frame(2,3,2,2,5,6,7,8)
  test1<-input.df.scaled[dim(input.df.scaled)[1],1:optimum.aic]
  #test2<-data.frame(NA,test1[,1:(optimum.aic)])
  #test1[,1:(optimum.aic)]
  label<-paste("Lag.",1:optimum.aic,sep="")
  transpose.test<-t(test1)
  colnames(transpose.test)<-label
  
  
  V1<-predict(fit,transpose.test)
  
  #unscale(transpose.test,norm.data = input.df.scaled)
  #unscale(V1,norm.data = input.df.scaled)
  combined<-cbind(unscale(V1,norm.data = input.df.scaled),
                  unscale(transpose.test,norm.data = input.df.scaled))
  colnames(combined)[1]<-"y"
  
  input.df<-rbind(input.df,combined)
}


#copy.input.df<-input.df
#copy.input.df<-rbind(copy.input.df,combined)


#xxx<-cbind(V1,transpose.test)
#unscale(xxx,norm.data = store.df)
#aaa<-input.df
#input.df<-rbind(input.df,xxx)
#rownames(input.df) <- NULL

#unscale data
#xxxx<-unscale(input.df,norm.data=store.df)

#nnetar.model<-forecast::nnetar(y=outputs,size = 60)
#forecast::accuracy(nnetar.model)

#lags.select(ts(outputs))
