

aft.aic<- function(my_dw){
  
bbb<-sum(my_dw$Runs==0)
my_dw$Runs[which(my_dw$Runs==0)]<- rep(0.001,bbb)
mll<- flexsurvreg(Surv(Runs, delta)~ Type+ Vanue + Chasing, data= my_dw, dist = "llogis")

mw<- flexsurvreg(Surv(Runs, delta)~ Type+ Vanue + Chasing, data= my_dw, dist = "weibull")

mln<- flexsurvreg(Surv(Runs, delta)~ Type+ Vanue + Chasing, data= my_dw, dist = "lnorm")

mg<- flexsurvreg(Surv(Runs, delta)~ Type+ Vanue + Chasing, data= my_dw, dist = "gompertz")

a1<- c("llogis","Weibull","lnorm","gompatz")
a2<- c(AIC(mll),AIC(mw),AIC(mln),AIC(mg))
names(a2)<- a1

return(a2)
}

www<-t(rbind(
  aft.aic(my_rs),
  aft.aic(my_kl),
  aft.aic(my_sd),
aft.aic(my_dw),
aft.aic(my_ml)
))
colnames(www)<- c("rs","kl","sd","dw","ml")
www



# Weibull AFT

est.wai<- function(my_dw){
  
bbb<-sum(my_dw$Runs==0)
my_dw$Runs[which(my_dw$Runs==0)]<- rep(0.001,bbb)
mw<- flexsurvreg(Surv(Runs, delta)~ Type+ Vanue + Chasing, data= my_dw, dist = "weibull")
mw1<- survreg(Surv(Runs, delta)~ Type+ Vanue + Chasing, data= my_dw, dist = "weibull")
est<-mw1$coefficients
my_dw$res<- residuals(mw, type="coxsnell")
cmodel<- survfit(Surv(res, delta)~1, data=my_dw)
plot(cmodel$time, cmodel$cumhaz, type="s", xlab="run",ylab="Cum.haz of residuals", main="Cox Snell residuals")
abline(0,1,lty=2, col="red")
return( list(ew=est, summary(mw1)))
}

est.wai(my_dw)




