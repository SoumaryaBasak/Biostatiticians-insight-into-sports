# Libraries
library(readxl)
library(dplyr)
library(survival)
library(ggfortify)
library(ggplot2)
library(ggthemes)
library(flexsurv)
library(timereg)
library(survminer)
library(ggmap)
library(ggthemes)
library(mice)
library(addhazard)
library(ahaz)
library(mstate)
# My functions 

# KM estimates: survival to hazard


km.sur.haz<- function(model, add, colour){
  km_surv<- model$surv    # values of the survival functions
  a<-(diff((-log(km_surv)),1) )
  km_haz<-c( (-log(km_surv))[1] ,a )
  mx=which.max(model$time)-1
  
  if(add==FALSE){
    plot(model$time, km_haz, type="l", col=colour,lwd=1,
         xlim=c(0, model$time[mx]),
        main=" Hazard Curve of  Batsman", 
        ylab= "rate", xlab="runs", xaxt="n")
    axis(1, at=model$time, labels = model$time)  
    }
  else  {lines(model$time,km_haz, col= colour, 
               xlim=c(0,model$time[mx]),
               type="l", lwd=1)}
  
}


smooth_haz<- function(model, colour){
  km_surv<- model$surv    # values of the survival functions
  a<-(diff((-log(km_surv)),1) )
  km_haz<-c( (-log(km_surv))[1] ,a )
  mx=which.max(model$time)-1
  
  x= model$time 
  y= km_haz 
  plot(x,y,col=NA,xlab = "Runs", ylab = "Hazard rate",
       xlim=c(0,120), ylim = c(0,0.7)
       )
  lines(smooth.spline(x[-length(x)],y[-length(y)],spar = 0.5))
}



# Weibull fitting function

wei.fit<- function(ini.alpha,  y,d){
  p_yi<- y
  p_di<- d
  alpha_hat<- function(x,p_yi,p_di){
    a1<- sum(p_di) * sum((p_yi^x))
    a2<- sum(p_di)* sum((p_yi^x)*log(p_yi)) - (sum((p_yi^x))*sum(p_di*log(p_yi)))
    return(a1/a2)
  }
  
  lambda<- function(x, p_yi, p_di){
    a1<- sum(p_di)
    a2<- sum(p_yi^x)
    return(a1/a2)
  }
  a.o<- ini.alpha  # initial value of alpha
  c<- 10
  while (c>0.00001) {
    a.n<- alpha_hat(a.o, p_yi, p_di)
    c=abs(a.n-a.o)
    a.o<- a.n
  }
  lambda_new<- lambda(a.o, p_yi, p_di)
  #paste("The value of alpha_hat is=", a.o)
  #paste("The value of lambda_hat is=",lambda_new)
  re<- c(a.o,lambda_new)
  names(re)<-c("alpha","lambda")
  
  return(re)
}

