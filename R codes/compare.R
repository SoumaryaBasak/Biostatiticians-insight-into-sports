
#================ Importing Data ================

### =================== KL ===================================

df_kl<- read_excel("kl_rahul_excel.xlsx")
df_kl<- as.data.frame(df_kl)
dim(df_kl)

df_kl %>% 
  group_by(Type) %>% 
  summarise(length(Type))

# removing the t20 innings
df_kl<- subset(df_kl, df_kl$Type != "T20I") # reduces to 130 inns


summary(df_kl)# All the columns are charecter

df_kl[,1]<- as.numeric(df_kl$Runs) # Runs col needs to be numeric
df_kl$Pos<- as.factor(df_kl$Pos)  # Pos col needs to be factor
df_kl$Vanue<- as.factor(df_kl$Vanue) # Vanue converted to char. to factor
df_kl$SR<- as.numeric(df_kl$SR)   # SR from chr. to factor
# creating delta variable
df_kl$delta<- ifelse(df_kl$Dismissal=="not out",0,1)

summary(df_kl)          # has 3 NA values
df_kl<- na.omit(df_kl)  # remove the NA values
dim(df_kl)

# creating chasing
df_kl$chasing<- ifelse(df_kl$Inns %in% c("2","4"),"c","d")
                            # c: chasing , d: Defending

# lets have a look to the df
head(df_kl)


# Data For survival analysis
my_kl<- data.frame("Runs"= df_kl$Runs, "delta"=df_kl$delta,
                   "Type"=as.factor(df_kl$Type),
                   "Opposition"= as.factor(df_kl$Opposition),
                   "Vanue"=as.factor(df_kl$Vanue),
                   "Chasing"=as.factor(df_kl$chasing)
                  )
dim(my_kl)
summary(my_kl)

sum(my_kl$Runs==0)  # scores 0: 9 times
#my_kl<- subset(my_kl, my_kl$Runs <= 120)
                # considering the runs between 0 to 120 as the highest runs come                   limited times that may hampper the analysis

make.cens<- function(my_kl){
  for(i in 1:nrow(my_kl)){
    if(my_kl$Runs[i]>120){
      my_kl$r[i]= 120
      my_kl$dx[i]=0
      }
    else{
      my_kl$r[i]= my_kl$Runs[i]
      my_kl$dx[i]=1
    }
  }
  return(my_kl)
}    #function to make 120+ score censored 


my_kl<- make.cens(my_kl)
dim(my_kl)
summary(my_kl)
#my_kl$y[which(my_kl$Runs==0)]<- rep(0.001,9) #replacing the 0s by 0.001




### =================== SD ========================================
df_sd<- read_excel("sd_excel.xlsx")
df_sd<- as.data.frame(df_sd)
dim(df_sd)


df_sd %>% 
  group_by(Type) %>% 
  summarise(length(Type))

# removing the t20 innings
df_sd<- subset(df_sd, df_sd$Type != "T20I") # reduces to 226 inns


summary(df_sd)# All the columns are charecter

df_sd[,1]<- as.numeric(df_sd$Runs) # Runs col needs to be numeric
df_sd$Pos<- as.factor(df_sd$Pos)  # Pos col needs to be factor
df_sd$Vanue<- as.factor(df_sd$Vanue) # Vanue converted to char. to factor
df_sd$SR<- as.numeric(df_sd$SR)   # SR from chr. to factor

# creating delta variable
df_sd$delta<- ifelse(df_sd$Dismissal=="not out",0,1)

summary(df_sd)         # has 4 NA values
df_sd<- na.omit(df_sd)  # remove the NA values
dim(df_sd)

# creating chasing
df_sd$chasing<- ifelse(df_sd$Inns %in% c("2","4"),"c","d")
# c: chasing , d: Defending

# lets have a look to the df
head(df_sd)


# Data For survival analysis
my_sd<- data.frame("Runs"= df_sd$Runs, "delta"=df_sd$delta,
                   "Type"=as.factor(df_sd$Type),
                   "Opposition"= as.factor(df_sd$Opposition),
                   "Vanue"=as.factor(df_sd$Vanue),
                   "Chasing"=as.factor(df_sd$chasing)
)
dim(my_sd)
summary(my_sd)

sum(my_sd$Runs==0)  # scores 0: 9 times
#my_sd<- subset(my_sd, my_sd$Runs <= 120)
# considering the runs between 0 to 120 as the highest runs come                   limited times that may hampper the analysis


my_sd<- make.cens(my_sd)
dim(my_sd)     
summary(my_sd)
#my_sd$y[which(my_sd$Runs==0)]<- rep(0.001,9) #replacing the 0s by 0.001





### =================== RS ==================================

df_rs<- read_excel("rs_excel.xlsx")
df_rs<- as.data.frame(df_rs)
dim(df_rs)   # 476 innings


df_rs %>% 
  group_by(Type) %>% 
  summarise(length(Type))

# removing the t20 innings
df_rs<- subset(df_rs, df_rs$Type != "T20I") # reduces to 328 inns


summary(df_rs)# All the columns are charecter

df_rs[,1]<- as.numeric(df_rs$Runs) # Runs col needs to be numeric
df_rs$Pos<- as.factor(df_rs$Pos)  # Pos col needs to be factor
df_rs$Vanue<- as.factor(df_rs$Vanue) # Vanue converted to char. to factor
df_rs$SR<- as.numeric(df_rs$SR)   # SR from chr. to factor

# creating delta variable
df_rs$delta<- ifelse(df_rs$Dismissal=="not out",0,1)

summary(df_rs)         # has 52 NA values
df_rs<- na.omit(df_rs)  # remove the NA values
dim(df_rs)              # reduces to 276 inngs

# creating chasing
df_rs$chasing<- ifelse(df_rs$Inns %in% c("2","4"),"c","d")
# c: chasing , d: Defending

# lets have a look to the df
head(df_rs)

df_rs<- df_rs %>% 
  filter(Year>=2013)

# Data For survival analysis
my_rs<- data.frame("Runs"= df_rs$Runs, "delta"=df_rs$delta,
                   "Type"=as.factor(df_rs$Type),
                   "Opposition"= as.factor(df_rs$Opposition),
                   "Vanue"=as.factor(df_rs$Vanue),
                   "Chasing"=as.factor(df_rs$chasing)
)
dim(my_rs)
summary(my_rs)

sum(my_rs$Runs==0)  # scores 0: 10 times
#my_rs<- subset(my_rs, my_rs$Runs <= 120)
# considering the runs between 0 to 120 as the highest runs come                   limited times that may hampper the analysis

my_rs<- make.cens(my_rs)
dim(my_rs)   
summary(my_rs)
#my_rs$y[which(my_rs$Runs==0)]<- rep(0.001,9) #replacing the 0s by 0.001




### =================== KW ==================================
df_kw<- read_excel("kw_excel.xlsx")
df_kw<- as.data.frame(df_kw)
dim(df_kw)      # 417 inngs


df_kw %>% 
  group_by(Type) %>% 
  summarise(length(Type))

# removing the t20 innings
df_kw<- subset(df_kw, df_kw$Type != "T20I") # reduces to 330 inns


summary(df_kw)# All the columns are charecter

df_kw[,1]<- as.numeric(df_kw$Runs) # Runs col needs to be numeric
df_kw$Pos<- as.factor(df_kw$Pos)  # Pos col needs to be factor
df_kw$Vanue<- as.factor(df_kw$Vanue) # Vanue converted to char. to factor
df_kw$SR<- as.numeric(df_kw$SR)   # SR from chr. to factor

# creating delta variable
df_kw$delta<- ifelse(df_kw$Dismissal=="not out",0,1)

summary(df_kw)         # has 45 NA values
df_kw<- na.omit(df_kw)  # remove the NA values
dim(df_kw)              # 285 inngs

# creating chasing
df_kw$chasing<- ifelse(df_kw$Inns %in% c("2","4"),"c","d")
# c: chasing , d: Defending

# lets have a look to the df
head(df_kw)


# Data for Survvival Analysis
my_kw<- data.frame("y"= df_kw$Runs, "d"=df_kw$delta,
                   "Type"=as.factor(df_kw$Type),
                   "Opposition"= as.factor(df_kw$Opposition),
                   "Vanue"=as.factor(df_kw$Vanue),
                   "Chasing"=as.factor(df_kw$chasing)
                    )
dim(my_kw)
summary(my_kw)

sum(my_kw$y==0)


#my_kw$y[which(my_kw$y==0)]<- rep(0.001,9) #replacing the 0s by 0.001

#my_kw<- subset(my_kw, my_kw$y <= 120)
# considering the runs between 0 to 120 as the highest runs come                   limited times that may happer the analysis
#dim(my_kw)


### =================== DW ==============================================
df_dw<- read_excel("dw_excel.xlsx")
df_dw<- as.data.frame(df_dw)
dim(df_dw)    # 430 inngs


df_dw %>% 
  group_by(Type) %>% 
  summarise(length(Type))

# removing the t20 innings
df_dw<- subset(df_dw, df_dw$Type != "T20I") # reduces to 331 inns


summary(df_dw)# All the columns are charecter

df_dw[,1]<- as.numeric(df_dw$Runs) # Runs col needs to be numeric
df_dw$Pos<- as.factor(df_dw$Pos)  # Pos col needs to be factor
df_dw$Vanue<- as.factor(df_dw$Vanue) # Vanue converted to char. to factor
df_dw$SR<- as.numeric(df_dw$SR)   # SR from chr. to factor

# creating delta variable
df_dw$delta<- ifelse(df_dw$Dismissal=="not out",0,1)

summary(df_dw)         # has 18 NA values
df_dw<- na.omit(df_dw)  # remove the NA values
dim(df_dw)     # reduces to 313 inngs

# creating chasing
df_dw$chasing<- ifelse(df_dw$Inns %in% c("2","4"),"c","d")
# c: chasing , d: Defending

# lets have a look to the df
head(df_dw)



# Data For survival analysis
my_dw<- data.frame("Runs"= df_dw$Runs, "delta"=df_dw$delta,
                   "Type"=as.factor(df_dw$Type),
                   "Opposition"= as.factor(df_dw$Opposition),
                   "Vanue"=as.factor(df_dw$Vanue),
                   "Chasing"=as.factor(df_dw$chasing)
)
dim(my_dw)
summary(my_dw)

sum(my_dw$Runs==0)  # scores 0: 15 times
#my_dw<- subset(my_dw, my_dw$Runs <= 120)
# considering the runs between 0 to 120 as the highest runs come                   limited times that may hampper the analysis


my_dw<- make.cens(my_dw)
dim(my_dw)     
summary(my_dw)
#my_dw$y[which(my_dw$Runs==0)]<- rep(0.001,9) #replacing the 0s by 0.001





### =================== JR ===============================
df_jr<- read_excel("jr_excel.xlsx")
df_jr<- as.data.frame(df_jr)
dim(df_jr)  # 190 inngs


df_jr %>% 
  group_by(Type) %>% 
  summarise(length(Type))

# removing the t20 innings
df_jr<- subset(df_jr, df_jr$Type != "T20I") # reduces to 126 inns


summary(df_jr)# All the columns are charecter

df_jr[,1]<- as.numeric(df_jr$Runs) # Runs col needs to be numeric
df_jr$Pos<- as.factor(df_jr$Pos)  # Pos col needs to be factor
df_jr$Vanue<- as.factor(df_jr$Vanue) # Vanue converted to char. to factor
df_jr$SR<- as.numeric(df_jr$SR)   # SR from chr. to factor

# creating delta variable
df_jr$delta<- ifelse(df_jr$Dismissal=="not out",0,1)

summary(df_jr)         # has 9 NA values
df_jr<- na.omit(df_jr)  # remove the NA values
dim(df_jr)             # reduces to 117 inngs

# creating chasing
df_jr$chasing<- ifelse(df_jr$Inns %in% c("2","4"),"c","d")
# c: chasing , d: Defending

# lets have a look to the df
head(df_jr)


# Data for Survvival Analysis
my_jr<- data.frame("y"= df_jr$Runs, "d"=df_jr$delta,
                   "Type"=as.factor(df_jr$Type),
                   "Opposition"= as.factor(df_jr$Opposition),
                   "Vanue"=as.factor(df_jr$Vanue),
                   "Chasing"=as.factor(df_jr$chasing)
)
dim(my_jr)
summary(my_jr)

sum(my_jr$y==0)


#my_jr$y[which(my_jr$y==0)]<- rep(0.001,9) #replacing the 0s by 0.001

#my_jr<- subset(my_jr, my_jr$y <= 120)
# considering the runs between 0 to 120 as the highest runs come                   limited times that may happer the analysis
#dim(my_jr)





### =================== ML ============================
df_ml<- read_excel("ml_excel.xlsx")
df_ml<- as.data.frame(df_ml)
dim(df_ml)  # 98 inngs


df_ml %>% 
  group_by(Type) %>% 
  summarise(length(Type))

# removing the t20 innings
df_ml<- subset(df_ml, df_ml$Type != "T20I") # reduces to 97 inns


summary(df_ml)# All the columns are charecter

df_ml[,1]<- as.numeric(df_ml$Runs) # Runs col needs to be numeric
df_ml$Pos<- as.factor(df_ml$Pos)  # Pos col needs to be factor
df_ml$Vanue<- as.factor(df_ml$Vanue) # Vanue converted to char. to factor
df_ml$SR<- as.numeric(df_ml$SR)   # SR from chr. to factor

# creating delta variable
df_ml$delta<- ifelse(df_ml$Dismissal=="not out",0,1)

summary(df_ml)         # has 11 NA values
df_ml<- na.omit(df_ml)  # remove the NA values
dim(df_ml)             # reduces to 86 inngs

# creating chasing
df_ml$chasing<- ifelse(df_ml$Inns %in% c("2","4"),"c","d")
# c: chasing , d: Defending

# lets have a look to the df
head(df_ml)



# Data For survival analysis
my_ml<- data.frame("Runs"= df_ml$Runs, "delta"=df_ml$delta,
                   "Type"=as.factor(df_ml$Type),
                   "Opposition"= as.factor(df_ml$Opposition),
                   "Vanue"=as.factor(df_ml$Vanue),
                   "Chasing"=as.factor(df_ml$chasing)
)
dim(my_ml)
summary(my_ml)

sum(my_ml$Runs==0)  # scores 0: 5 times
#my_ml<- subset(my_ml, my_ml$Runs <= 120)
# considering the runs between 0 to 120 as the highest runs come                   limited times that may hampper the analysis

my_ml<- make.cens(my_ml)

dim(my_ml)    
summary(my_ml)
#my_ml$y[which(my_ml$Runs==0)]<- rep(0.001,9) #replacing the 0s by 0.001







### ==================  Data analysis ================


df_kl %>% 
  group_by(Type) %>% 
  summarise("n.inns"= length(Type),
            "outs"= sum(delta),
            "Avg.run"= mean(Runs),
            "Avg.SR"=mean(SR))        #format wise summary for KL

# yearly performance in test
x<-df_kl %>%
  filter(Type=="Test") %>% 
        group_by(Year) %>% 
        summarise("n.inns"= length(Type),
                  "outs"= sum(delta),
                  "Avg.run"= mean(Runs),
                  "Avg.SR"=mean(SR))      
plot(x$Year,x$Avg.run,type="b", xlim=c(2014,2023),bty="l",
     ylim=c(0,140),main="Average Runs over the year in Test",
     xlab="years", ylab="Avg.Runs")

x<-df_sd %>%
  filter(Type=="Test") %>% 
  group_by(Year) %>% 
  summarise("n.inns"= length(Type),
            "outs"= sum(delta),
            "Avg.run"= mean(Runs),
            "Avg.SR"=mean(SR))      
lines(x$Year,x$Avg.run,type="b", col="blue",
     ylim=c(0,90),main="Average Runs over the year in Test")

x<-df_rs %>%
  filter(Type=="Test") %>% 
  group_by(Year) %>% 
  summarise("n.inns"= length(Type),
            "outs"= sum(delta),
            "Avg.run"= mean(Runs),
            "Avg.SR"=mean(SR))      
lines(x$Year,x$Avg.run,type="b",col="green3",
     ylim=c(0,90),main="Average Runs over the year in Test")


x<-df_dw %>%
  filter(Type=="Test") %>% 
  group_by(Year) %>% 
  summarise("n.inns"= length(Type),
            "outs"= sum(delta),
            "Avg.run"= mean(Runs),
            "Avg.SR"=mean(SR))      
lines(x$Year,x$Avg.run,type="b", col="orange",lty=2, 
     ylim=c(0,90),main="Average Runs over the year in Test")


x<-df_ml %>%
  filter(Type=="Test") %>% 
  group_by(Year) %>% 
  summarise("n.inns"= length(Type),
            "outs"= sum(delta),
            "Avg.run"= mean(Runs),
            "Avg.SR"=mean(SR))      
lines(x$Year,x$Avg.run,type="b",col="red", lty=2,
     ylim=c(0,90),main="Average Runs over the year in Test")


legend("topright", legend = c("KL","SD","RS","DW","ML"), col=c("black","blue","green3","orange","red"), lty=c(1,1,1,2,2))

# yearly performance of kl in ODI
x<-df_kl %>%
  filter(Type=="ODI") %>% 
  group_by(Year) %>% 
  summarise("n.inns"= length(Type),
            "outs"= sum(delta),
            "Avg.run"= mean(Runs),
            "Avg.SR"=mean(SR))      
plot(x$Year,x$Avg.run,type="b", xlim=c(2014,2023),bty="l",
     ylim=c(0,120),main="Average Runs over the year in ODI",
     xlab="years", ylab="Avg.Runs")

x<-df_sd %>%
  filter(Type=="ODI") %>% 
  group_by(Year) %>% 
  summarise("n.inns"= length(Type),
            "outs"= sum(delta),
            "Avg.run"= mean(Runs),
            "Avg.SR"=mean(SR))      
lines(x$Year,x$Avg.run,type="b", col="blue")

x<-df_rs %>%
  filter(Type=="ODI") %>% 
  group_by(Year) %>% 
  summarise("n.inns"= length(Type),
            "outs"= sum(delta),
            "Avg.run"= mean(Runs),
            "Avg.SR"=mean(SR))      
lines(x$Year,x$Avg.run,type="b",col="green3")


x<-df_dw %>%
  filter(Type=="ODI") %>% 
  group_by(Year) %>% 
  summarise("n.inns"= length(Type),
            "outs"= sum(delta),
            "Avg.run"= mean(Runs),
            "Avg.SR"=mean(SR))      
lines(x$Year,x$Avg.run,type="b", col="orange",lty=2)


x<-df_ml %>%
  filter(Type=="ODI") %>% 
  group_by(Year) %>% 
  summarise("n.inns"= length(Type),
            "outs"= sum(delta),
            "Avg.run"= mean(Runs),
            "Avg.SR"=mean(SR))      
lines(x$Year,x$Avg.run,type="b",col="red", lty=2)


legend("topright", legend = c("KL","SD","RS","DW","ML"), col=c("black","blue","green3","orange","red"), lty=c(1,1,1,2,2))


# yearly change of strike rate in Odi
x<-df_kl %>%
  filter(Type=="ODI") %>% 
  group_by(Year) %>% 
  summarise("n.inns"= length(Type),
            "outs"= sum(delta),
            "Avg.run"= mean(Runs),
            "Avg.SR"=mean(SR))      
plot(x$Year,x$Avg.SR,type="b", xlim=c(2014,2023),bty="l",
     ylim=c(30,180),main="Average Strike Rate over the year in ODI",
     xlab="years", ylab="Avg.Runs")

x<-df_sd %>%
  filter(Type=="ODI") %>% 
  group_by(Year) %>% 
  summarise("n.inns"= length(Type),
            "outs"= sum(delta),
            "Avg.run"= mean(Runs),
            "Avg.SR"=mean(SR))      
lines(x$Year,x$Avg.SR,type="b", col="blue")

x<-df_rs %>%
  filter(Type=="ODI") %>% 
  group_by(Year) %>% 
  summarise("n.inns"= length(Type),
            "outs"= sum(delta),
            "Avg.run"= mean(Runs),
            "Avg.SR"=mean(SR))      
lines(x$Year,x$Avg.SR,type="b",col="green3")


x<-df_dw %>%
  filter(Type=="ODI") %>% 
  group_by(Year) %>% 
  summarise("n.inns"= length(Type),
            "outs"= sum(delta),
            "Avg.run"= mean(Runs),
            "Avg.SR"=mean(SR))      
lines(x$Year,x$Avg.SR,type="b", col="orange",lty=2)


x<-df_ml %>%
  filter(Type=="ODI") %>% 
  group_by(Year) %>% 
  summarise("n.inns"= length(Type),
            "outs"= sum(delta),
            "Avg.run"= mean(Runs),
            "Avg.SR"=mean(SR))      
lines(x$Year,x$Avg.SR,type="b",col="red", lty=2)


legend("topleft", legend = c("KL","SD","RS","DW","ML"), col=c("black","blue","green3","orange","red"), lty=c(1,1,1,2,2))



#-----------------------------------------
df_sd %>% 
  filter(Year>=2013) %>% 
  group_by(Type) %>% 
  summarise("n.inns"= length(Type),
            "outs"= sum(delta),
            "Avg.run"= mean(Runs),
            "Avg.SR"=mean(SR))        #format wise summary for sd
df_rs %>% 
  filter(Year>=2013) %>% 
  group_by(Type) %>%
  summarise("n.inns"= length(Type),
            "outs"= sum(delta),
            "Avg.run"= mean(Runs),
            "Avg.SR"=mean(SR))        #format wise summary for rs
df_dw %>% 
  filter(Year>=2013) %>% 
    group_by(Type) %>% 
  summarise("n.inns"= length(Type),
            "outs"= sum(delta),
            "Avg.run"= mean(Runs),
            "Avg.SR"=mean(SR))        #format wise summary for dw
df_ml %>% 
  group_by(Type) %>% 
  summarise("n.inns"= length(Type),
            "outs"= sum(delta),
            "Avg.run"= mean(Runs),
            "Avg.SR"=mean(SR))        #format wise summary for ML


### =========== Yearly analysis =============
df_kl$Year<- as.numeric(df_kl$Year)
df_sd$Year<- as.numeric(df_sd$Year)
df_rs$Year<- as.numeric(df_rs$Year)
df_dw$Year<- as.numeric(df_dw$Year)
df_ml$Year<- as.numeric(df_ml$Year)

# A combined sample of all the players from 2013 to 2023
df<- rbind( cbind(df_kl, "player"="kl") ,
            cbind(subset(df_rs, Year>=2013), "player"="rs") ,
            cbind(subset(df_sd, Year>=2013), "player"="sd") ,
            cbind(subset(df_dw, Year>=2013), "player"="dw") ,
            cbind(df_ml, "player"="ml") 
            ) 
df$player<- as.factor(df$player)            
df.test<- subset(df, Type=="Test" )
df.odi<- subset(df, Type=="ODI")            

x<-df.test %>% 
  group_by(player) %>% 
  summarise("n.inns"= length(player), "Avg.run"= mean(Runs))

y<-df.odi %>% 
  group_by(player) %>% 
  summarise("n.inns"= length(player), "Avg.run"= mean(Runs))
 
z<-rbind("test"= x$Avg.run,"odi"=y$Avg.run)
colnames(z)<- x$player
z

b<-barplot(z, beside = T, legend.text = TRUE, border = NA, width=1,
        col=c("#609966","#9DC08B"),ylim = c(0,60),
        args.legend = list(x = "topright",inset = c(- 0.01,- 0.15)),
        main="Average Scores per innings for the batsman")
text(b, z-3,round(z,0), font = 2, cex=0.6)



# Avg in Home and away for test
x1<- df.test %>% 
  filter(player=="kl") %>% 
  group_by(Vanue) %>% 
  summarise("n.inns"= length(Vanue), "Avg.run"= mean(Runs))

x2 <- df.test %>% 
  filter(player=="rs") %>% 
  group_by(Vanue) %>% 
  summarise("n.inns"= length(Vanue), "Avg.run"= mean(Runs))

x3 <- df.test %>% 
  filter(player=="sd") %>% 
  group_by(Vanue) %>% 
  summarise("n.inns"= length(Vanue), "Avg.run"= mean(Runs))


x4 <- df.test %>% 
  filter(player=="dw") %>% 
  group_by(Vanue) %>% 
  summarise("n.inns"= length(Vanue), "Avg.run"= mean(Runs))

x5 <- df.test %>% 
  filter(player=="ml") %>% 
  group_by(Vanue) %>% 
  summarise("n.inns"= length(Vanue), "Avg.run"= mean(Runs))

z<-cbind(x1$Avg.run,x2$Avg.run,x3$Avg.run,x4$Avg.run,x5$Avg.run)
colnames(z)<- c("KL","RS","SD","DW","ML")
rownames(z)<- c("Away","Home")
z

b<-barplot(z, beside = T, col=c("#609966","#9DC08B"), border = NA, legend.text = T,args.legend = list(x = "topleft",inset = c(- 0.01,- 0.15), bty="n"),
        main="Average Scores per innings for the batsman", sub= "For test")
text(b, z-3,round(z,0), font = 2, cex=0.6)

# avg time spent in crease in test
x1<- df.test %>% 
  filter(player=="kl") %>% 
  group_by(Vanue) %>% 
  summarise("n.inns"= length(Vanue), "Avg.run"= mean(Mins))

x2 <- df.test %>% 
  filter(player=="rs") %>% 
  group_by(Vanue) %>% 
  summarise("n.inns"= length(Vanue), "Avg.run"= mean(Mins))

x3 <- df.test %>% 
  filter(player=="sd") %>% 
  group_by(Vanue) %>% 
  summarise("n.inns"= length(Vanue), "Avg.run"= mean(Mins))


x4 <- df.test %>% 
  filter(player=="dw") %>% 
  group_by(Vanue) %>% 
  summarise("n.inns"= length(Vanue), "Avg.run"= mean(Mins))

x5 <- df.test %>% 
  filter(player=="ml") %>% 
  group_by(Vanue) %>% 
  summarise("n.inns"= length(Vanue), "Avg.run"= mean(Mins))

z<-cbind(x1$Avg.run,x2$Avg.run,x3$Avg.run,x4$Avg.run,x5$Avg.run)
colnames(z)<- c("KL","RS","SD","DW","ML")
rownames(z)<- c("Away","Home")
z

b<-barplot(z, beside = T, col=c("#609966","#9DC08B"), border = NA, legend.text = T,args.legend = list(x = "topleft",inset = c( -0.02, -0.05), bty="n"),
           main="Average time spent per innings for the batsman", sub= "For test (in minutes)")
text(b, z-3,round(z,0), font = 2, cex=0.6)





# Avg in Home and away for odi
x1<- df.odi %>% 
  filter(player=="kl") %>% 
  group_by(Vanue) %>% 
  summarise("n.inns"= length(Vanue), "Avg.run"= mean(Runs))

x2 <- df.odi %>% 
  filter(player=="rs") %>% 
  group_by(Vanue) %>% 
  summarise("n.inns"= length(Vanue), "Avg.run"= mean(Runs))

x3 <- df.odi %>% 
  filter(player=="sd") %>% 
  group_by(Vanue) %>% 
  summarise("n.inns"= length(Vanue), "Avg.run"= mean(Runs))


x4 <- df.odi %>% 
  filter(player=="dw") %>% 
  group_by(Vanue) %>% 
  summarise("n.inns"= length(Vanue), "Avg.run"= mean(Runs))

x5 <- df.odi %>% 
  filter(player=="ml") %>% 
  group_by(Vanue) %>% 
  summarise("n.inns"= length(Vanue), "Avg.run"= mean(Runs))

z<-cbind(x1$Avg.run,x2$Avg.run,x3$Avg.run,x4$Avg.run,x5$Avg.run)
colnames(z)<- c("KL","RS","SD","DW","ML")
rownames(z)<- c("Away","Home")
z

b<-barplot(z, beside = T, col=c("#609966","#9DC08B"), border = NA, legend.text = T,args.legend = list(x = "topleft",inset = c(- 0.01,- 0.15), bty="n"),ylim = c(0,65),
        main="Average Scores per innings for the batsman", sub= "For ODI")
text(b, z-3,round(z,0), font = 2, cex=0.6)



####======================= Survival Analysis =============================

#========== kl ===================
head(my_kl)

# Survival Curve fit for kl
kl.odi<- survfit(Surv(r, dx)~1, data=subset(my_kl,Type=="ODI"))
kl.test<- survfit(Surv(Runs, delta)~1, data=subset(my_kl,Type=="Test"))


ggsurvplot(survfit(Surv(r, dx)~(Type), data=my_kl),
           data=my_kl, pval=TRUE, conf.int = TRUE,size=1,
           palette=c("#8F43EE","#0E8388"), conf.int.alpha=0.2
)+
  labs(title = "KL Rahul: Survival Curve for different format")

survdiff(Surv(r, dx)~(Type), data=my_kl)

# Survival Curve fit for sd
sd.odi<- survfit(Surv(r, dx)~1, data=subset(my_sd,Type=="ODI"))
sd.test<- survfit(Surv(Runs, delta)~1, data=subset(my_sd,Type=="Test"))


ggsurvplot(survfit(Surv(r, dx)~(Type), data=my_sd),
           data=my_sd, pval=TRUE, conf.int = TRUE,size=1,
           palette=c("#8F43EE","#0E8388"), conf.int.alpha=0.2
)+
  labs(title = "Shikhar Dhawan: Survival Curve for different format")


# Survival Curve fit for rs
rs.odi<- survfit(Surv(r, dx)~1, data=subset(my_rs,Type=="ODI"))
rs.test<- survfit(Surv(Runs, delta)~1, data=subset(my_rs,Type=="Test"))


ggsurvplot(survfit(Surv(r, dx)~(Type), data=my_rs),
           data=my_rs, pval=TRUE, conf.int = TRUE,size=1,
           palette=c("#8F43EE","#0E8388"), conf.int.alpha=0.2
)+
  labs(title = "Rohit Sharma: Survival Curve for different format")


# Survival Curve fit for dw
dw.odi<- survfit(Surv(r, dx)~1, data=subset(my_dw,Type=="ODI"))
dw.test<- survfit(Surv(Runs, delta)~1, data=subset(my_dw,Type=="Test"))


ggsurvplot(survfit(Surv(r, dx)~(Type), data=my_dw),
           data=my_dw, pval=TRUE, conf.int = TRUE,size=1,
           palette=c("#8F43EE","#0E8388"), conf.int.alpha=0.2
)+
  labs(title = "David Warner: Survival Curve for different format")



# Survival Curve fit for ml
ml.odi<- survfit(Surv(r, dx)~1, data=subset(my_ml,Type=="ODI"))
ml.test<- survfit(Surv(Runs, delta)~1, data=subset(my_ml,Type=="Test"))


ggsurvplot(survfit(Surv(r, dx)~(Type), data=my_ml),
           data=my_ml, pval=TRUE, conf.int = TRUE,size=1,
           palette=c("#8F43EE","#0E8388"), conf.int.alpha=0.2
)+
  labs(title = "M Labuschagne: Survival Curve for different format")




# Hazard Rates
# for odi
#smoothed
par(mfrow=c(1,5))
smooth_haz(kl.odi)
title(main="Rahul")
smooth_haz(rs.odi)
title(main="Rohit")
smooth_haz(sd.odi)
title(main="Dhawan")
smooth_haz(dw.odi)
title(main="Warner")
smooth_haz(ml.odi)
title(main="Marnus")

# usual
km.sur.haz(kl.odi, add=FALSE, colour="black")
km.sur.haz(rs.odi, add=FALSE, colour="red")
km.sur.haz(sd.odi, add=TRUE, colour="blue")
km.sur.haz(dw.odi, add=TRUE, colour="orange")
km.sur.haz(ml.odi, add=TRUE, colour="green3")
title(sub="for ODI")
legend("topleft", legend = c("KL","SD","RS","DW","ML"), col=c("black","blue","red","orange","green3"), lty=c(1,1,1,1,1))

# for test
km.sur.haz(kl.test, add=TRUE, colour="black")
km.sur.haz(rs.test, add=TRUE, colour="red")
km.sur.haz(sd.test, add=TRUE, colour="blue")
km.sur.haz(dw.test, add=TRUE, colour="orange")
km.sur.haz(ml.test, add=TRUE, colour="green3")
title(sub="for Test")
legend("topleft", legend = c("KL","SD","RS","DW","ML"), col=c("black","blue","red","orange","green3"), lty=c(1,1,1,1,1))

# smoothed
par(mfrow=c(1,5))
smooth_haz(kl.test)
title(main="Rahul")
smooth_haz(rs.test)
title(main="Rohit")
smooth_haz(sd.test)
title(main="Dhawan")
smooth_haz(dw.test)
title(main="Warner")
smooth_haz(ml.test)
title(main="Marnus")




# plot of cum haz odi (Not need)
plot(kl.odi$time, kl.odi$cumhaz, col="black", type="s", xlab="Runs",ylab = "Cumulative Hazard", main = "Failure rate of batsman",sub="For ODI", lwd=1.5)
lines(rs.odi$time, rs.odi$cumhaz, col="red", type="s",lwd=1.5)
lines(sd.odi$time, sd.odi$cumhaz, col="blue", type="s",lwd=1.5)
lines(dw.odi$time, dw.odi$cumhaz, col="orange3", type="s",lwd=1.5)
lines(ml.odi$time, ml.odi$cumhaz, col="green", type="s",lwd=1.5)
legend("topleft", legend = c("KL","SD","RS","DW","ML"), col=c("black","blue","red","orange","green3"), lty=c(1,1,1,1,1), bty="n")

abline(v=40, lty=2, col="skyblue")
abline(v=50, lty=2, col="skyblue")
abline(v=90, lty=2, col="skyblue")
abline(v=100, lty=2, col="skyblue")
abline(v=0, lty=2, col="skyblue")
abline(v=10, lty=2, col="skyblue")


# plot of cum haz test
plot(kl.test$time, kl.test$cumhaz, col="black", type="s", xlab="Runs",ylab = "Cumulative Hazard", main = "Failure rate of batsman",sub="For test", lwd=1.5)
lines(rs.test$time, rs.test$cumhaz, col="red", type="s",lwd=1.5)
lines(sd.test$time, sd.test$cumhaz, col="blue", type="s",lwd=1.5)
lines(dw.test$time, dw.test$cumhaz, col="orange3", type="s",lwd=1.5)
lines(ml.test$time, ml.test$cumhaz, col="green", type="s",lwd=1.5)
legend("topleft", legend = c("KL","SD","RS","DW","ML"), col=c("black","blue","red","orange","green3"), lty=c(1,1,1,1,1), bty="n")

abline(v=40, lty=2, col="skyblue")
abline(v=50, lty=2, col="skyblue")
abline(v=90, lty=2, col="skyblue")
abline(v=100, lty=2, col="skyblue")
abline(v=0, lty=2, col="skyblue")
abline(v=10, lty=2, col="skyblue")

# for odi
# for 0
a1<-kl.odi$surv[max(which(kl.odi$time==0))]
a1s<-summary(kl.odi)$std.err[max(which(kl.odi$time==0))]
a2<-rs.odi$surv[max(which(rs.odi$time==0))]
a2s<-summary(rs.odi)$std.err[max(which(rs.odi$time==0))]
a3<-sd.odi$surv[max(which(sd.odi$time==0))]
a3s<-summary(sd.odi)$std.err[max(which(sd.odi$time==0))]
a4<-dw.odi$surv[max(which(dw.odi$time==0))]
a4s<-summary(dw.odi)$std.err[max(which(dw.odi$time==0))]
a5<-ml.odi$surv[max(which(ml.odi$time==0))]
a5s<-summary(ml.odi)$std.err[max(which(ml.odi$time==0))]
m<- cbind(rbind(a1,a2,a3,a4,a5),rbind(a1s,a2s,a3s,a4s,a5s))
#for 50
a1<-kl.odi$surv[max(which(kl.odi$time<=50))]
a1s<-summary(kl.odi)$std.err[max(which(kl.odi$time<=50))]
a2<-rs.odi$surv[max(which(rs.odi$time<=50))]
a2s<-summary(rs.odi)$std.err[max(which(rs.odi$time<=50))]
a3<-sd.odi$surv[max(which(sd.odi$time<=50))]
a3s<-summary(sd.odi)$std.err[max(which(sd.odi$time<=50))]
a4<-dw.odi$surv[max(which(dw.odi$time<=50))]
a4s<-summary(dw.odi)$std.err[max(which(dw.odi$time<=50))]
a5<-ml.odi$surv[max(which(ml.odi$time<=50))]
a5s<-summary(ml.odi)$std.err[max(which(ml.odi$time<=50))]
m<- cbind(m,rbind(a1,a2,a3,a4,a5),rbind(a1s,a2s,a3s,a4s,a5s))
# for 100
a1<-kl.odi$surv[max(which(kl.odi$time<=100))]
a1s<-summary(kl.odi)$std.err[max(which(kl.odi$time<=100))]
a2<-rs.odi$surv[max(which(rs.odi$time<=100))]
a2s<-summary(rs.odi)$std.err[max(which(rs.odi$time<=100))]
a3<-sd.odi$surv[max(which(sd.odi$time<=100))]
a3s<-summary(sd.odi)$std.err[max(which(sd.odi$time<=100))]
a4<-dw.odi$surv[max(which(dw.odi$time<=100))]
a4s<-summary(dw.odi)$std.err[max(which(dw.odi$time<=100))]
a5<-ml.odi$surv[max(which(ml.odi$time<=100))]
a5s<-summary(ml.odi)$std.err[max(which(ml.odi$time<=100))]
m1<- cbind(m,rbind(a1,a2,a3,a4,a5),rbind(a1s,a2s,a3s,a4s,a5s))
rownames(m1)<- c("KL","RS","SD","DW","ML")
colnames(m1)<- c("at 0","se0","at 50","se50","at 100","se100")

barplot(t(m1), beside = T,border = NA, col=c("#4E944F","#83BD75","#B4E197"), legend.text = T,args.legend = list(x = "topright",inset = c(-0.01, -0.250), bty="n"), ylab="Cumulative Hazard", main="Survival probability in specific situation", sub=" For ODI")


# for test
# for test
# for 0
a1<-kl.test$surv[max(which(kl.test$time==0))]
a1s<-summary(kl.test)$std.err[max(which(kl.test$time==0))]
a2<-rs.test$surv[max(which(rs.test$time==0))]
a2s<-summary(rs.test)$std.err[max(which(rs.test$time==0))]
a3<-sd.test$surv[max(which(sd.test$time==0))]
a3s<-summary(sd.test)$std.err[max(which(sd.test$time==0))]
a4<-dw.test$surv[max(which(dw.test$time==0))]
a4s<-summary(dw.test)$std.err[max(which(dw.test$time==0))]
a5<-ml.test$surv[max(which(ml.test$time==0))]
a5s<-summary(ml.test)$std.err[max(which(ml.test$time==0))]
m<- cbind(rbind(a1,a2,a3,a4,a5),rbind(a1s,a2s,a3s,a4s,a5s))
#for 50
a1<-kl.test$surv[max(which(kl.test$time<=50))]
a1s<-summary(kl.test)$std.err[max(which(kl.test$time<=50))]
a2<-rs.test$surv[max(which(rs.test$time<=50))]
a2s<-summary(rs.test)$std.err[max(which(rs.test$time<=50))]
a3<-sd.test$surv[max(which(sd.test$time<=50))]
a3s<-summary(sd.test)$std.err[max(which(sd.test$time<=50))]
a4<-dw.test$surv[max(which(dw.test$time<=50))]
a4s<-summary(dw.test)$std.err[max(which(dw.test$time<=50))]
a5<-ml.test$surv[max(which(ml.test$time<=50))]
a5s<-summary(ml.test)$std.err[max(which(ml.test$time<=50))]
m<- cbind(m,rbind(a1,a2,a3,a4,a5),rbind(a1s,a2s,a3s,a4s,a5s))
# for 100
a1<-kl.test$surv[max(which(kl.test$time<=100))]
a1s<-summary(kl.test)$std.err[max(which(kl.test$time<=100))]
a2<-rs.test$surv[max(which(rs.test$time<=100))]
a2s<-summary(rs.test)$std.err[max(which(rs.test$time<=100))]
a3<-sd.test$surv[max(which(sd.test$time<=100))]
a3s<-summary(sd.test)$std.err[max(which(sd.test$time<=100))]
a4<-dw.test$surv[max(which(dw.test$time<=100))]
a4s<-summary(dw.test)$std.err[max(which(dw.test$time<=100))]
a5<-ml.test$surv[max(which(ml.test$time<=100))]
a5s<-summary(ml.test)$std.err[max(which(ml.test$time<=100))]
m2<- cbind(m,rbind(a1,a2,a3,a4,a5),rbind(a1s,a2s,a3s,a4s,a5s))
rownames(m2)<- c("KL","RS","SD","DW","ML")
colnames(m2)<- c("at 0","se0","at 50","se50","at 100","se100")


barplot(t(m2), beside = T,border = NA, col=c("#4E944F","#83BD75","#B4E197"), legend.text = T,args.legend = list(x = "topright",inset = c(-0.01, -0.25), bty="n"), ylab="Survival Probability", main="Survival Porbabilty in specific situation", sub=" For Test", ylim=c(0,1))

# Proportional Hazard model

###== check for proportionality of the hazard for KL
# For Venue
kl.ph.diag<- coxph(Surv(r,dx)~strata(Vanue),data=my_kl, method = "breslow")
df.base.haz<-basehaz(kl.ph.diag) 
diag.a<- df.base.haz %>%  filter(strata=="A")
diag.h<- df.base.haz %>%  filter(strata=="H")

plot( diag.a$time, log(diag.a$hazard), type="s", xlab="Runs", ylab="log cumulative hazard", main="Test for proportionality assumption for Venue variable" )
lines( diag.h$time, log(diag.h$hazard), type="s", lty=2 )
legend("bottomright", legend = c("Away","Home"),lty=c(1,2),bty="n", inset = 0.05, cex=0.8 )

# For Chasing
kl.ph.diag<- coxph(Surv(r,dx)~strata(Chasing),data=my_kl, method="breslow")
df.base.haz<-basehaz(kl.ph.diag) 
diag.a<- df.base.haz %>%  filter(strata=="c")
diag.b<- df.base.haz %>%  filter(strata=="d")

plot( diag.a$time, log(diag.a$hazard), type="s", xlab="Runs", ylab="log cumulative hazard", main="Test for proportionality assumption for Chasing variable" )
lines( diag.b$time, log(diag.b$hazard), type="s", lty=2 )
legend("bottomright", legend = c("Chase","defend"),lty=c(1,2),bty="n", inset = 0.05, cex=0.8 )

# For Type
kl.ph.diag<- coxph(Surv(r,dx)~strata(Type),data=my_kl, method="breslow")
df.base.haz<-basehaz(kl.ph.diag) 
diag.a<- df.base.haz %>%  filter(strata=="ODI")
diag.b<- df.base.haz %>%  filter(strata=="Test")

plot( diag.a$time, log(diag.a$hazard), type="s", xlab="Runs", ylab="log cumulative hazard", main="Test for proportionality assumption for Type variable" )
lines( diag.b$time, log(diag.b$hazard), type="s", lty=2 )
legend("bottomright", legend = c("ODI","Test"),lty=c(1,2),bty="n", inset = 0.05, cex=0.8 )


cox.zph(coxph(Surv(r, dx)~Type+Chasing+Vanue,data=my_kl, method="breslow"))
kl.ph<- coxph(Surv(r, dx)~Type+Vanue,data=my_kl, method="breslow")
kl.ph
AIC(kl.ph)


# cox snell residuals
my_kl$mart<- residuals(kl.ph, type = "martingale")
my_kl$cox_snell<- -(my_kl$mart - my_kl$dx)
# Fit model on Cox-Snell residuals (Approximately Expo(1) distributed under correct model)
## 
kl.coxsnell<- survfit(Surv(cox_snell,dx)~1,data=my_kl)
kl.coxsnell$cumhaz
plot(kl.coxsnell$time,kl.coxsnell$cumhaz,  pch=19,type="s",
     main="Cox Snell Residuals", xlab = "times for cox snell residuals",
     ylab="cumulative hazard of cox snell residuals")
abline(0,1, lty=2, col="red")



# additive hazard model
# we are using this model
a.model<- ahaz( surv = Surv(my_kl$Runs+runif(nrow(my_kl))*1e-2,my_kl$delta), X= model.matrix(~Type+ Vanue+Chasing, data=my_kl)[,-1])
summary(a.model)
plot(a.model)

w<-predict(a.model, type = "cumhaz")
h1<-diff(w$cumhaz,1)
plot(w$times,h1, type="s", main="Hazard", xlab="Runs ",ylab="Failure rates")

# for test
lines(w$times,h1+0.01526, type="s", lty=3, col="blue")
# for home matchs
lines(w$times,h1 -0.00650, type="s", lty=3, col="red")
# for defending
lines(w$times,h1-0.00280, type="s", lty=3, col="green3")
# test + defending + away
lines(w$times,h1+0.015337-0.002825, type="s", lty=2, col="brown")

# odi + home+ defending
lines(w$times,h1-0.002825-0.006357, type="s", lty=2, col="blue")

#legend("topleft", legend=c("Baseline", "Test", "Home matchs","Defend"),
#       lty=c(1,3,3,3), col=c("black", "blue","red","green3"), bty="n", inset=0.05)

legend("topleft", legend=c("Baseline", "Test+Away + Defend","ODI + Home + Defend"),
       lty=c(1,3,3), col=c("black", "brown","Blue"), bty="n", inset=0.05)









#===== check for proportionality of the hazard for rs
# For Venue
rs.ph.diag<- coxph(Surv(r,dx)~strata(Vanue),data=my_rs, method="breslow")
df.base.haz<-basehaz(rs.ph.diag) 
diag.a<- df.base.haz %>%  filter(strata=="A")
diag.h<- df.base.haz %>%  filter(strata=="H")

plot( diag.a$time, log(diag.a$hazard), type="s", xlab="Runs", ylab="log cumulative hazard", main="Test for proportionality assumption for Venue variable" )
lines( diag.h$time, log(diag.h$hazard), type="s", lty=2 )
legend("bottomright", legend = c("Away","Home"),lty=c(1,2),bty="n", inset = 0.05, cex=0.8 )

# For Chasing
rs.ph.diag<- coxph(Surv(r,dx)~strata(Chasing),data=my_rs, method="breslow")
df.base.haz<-basehaz(rs.ph.diag) 
diag.a<- df.base.haz %>%  filter(strata=="c")
diag.b<- df.base.haz %>%  filter(strata=="d")

plot( diag.a$time, log(diag.a$hazard), type="s", xlab="Runs", ylab="log cumulative hazard", main="Test for proportionality assumption for Chasing variable" )
lines( diag.b$time, log(diag.b$hazard), type="s", lty=2 )
legend("bottomright", legend = c("Chase","defend"),lty=c(1,2),bty="n", inset = 0.05, cex=0.8 )

# For Type
rs.ph.diag<- coxph(Surv(r,dx)~strata(Type),data=my_rs, method="breslow")
df.base.haz<-basehaz(rs.ph.diag) 
diag.a<- df.base.haz %>%  filter(strata=="ODI")
diag.b<- df.base.haz %>%  filter(strata=="Test")

plot( diag.a$time, log(diag.a$hazard), type="s", xlab="Runs", ylab="log cumulative hazard", main="Test for proportionality assumption for Type variable" )
lines( diag.b$time, log(diag.b$hazard), type="s", lty=2 )
legend("bottomright", legend = c("ODI","Test"),lty=c(1,2),bty="n", inset = 0.05, cex=0.8 )

rs.ph<- coxph(Surv(Runs, delta)~Type+Vanue,data=my_rs, method="breslow")

cox.zph(rs.ph)

# cox snell residuals
my_rs$mart<- residuals(rs.ph, type = "martingale")
my_rs$cox_snell<- -(my_rs$mart - my_rs$delta)
# Fit model on Cox-Snell residuals (Approximately Expo(1) distributed under correct model)
rs.coxsnell<- survfit(Surv(cox_snell,delta)~1,data=my_rs)
rs.coxsnell$cumhaz
plot(rs.coxsnell$time,rs.coxsnell$cumhaz, type="s", pch=19,
     main="Cox Snell Residuals", xlab = "times for cox snell residuals",
     ylab="cumulative hazard of cox snell residuals")
abline(0,1, lty=2, col="red")








# Additive Hazard model
a.model<- aalen(Surv(Runs, delta)~ const(Type)+ const(Vanue)+ const(Chasing), data=my_rs)
a.model
plot(a.model)

a.model<- ah(Surv(Runs+runif(nrow(my_rs))*1e-2, delta)~ const(Type)+ const(Vanue)+ const(Chasing), data=my_rs, ties = FALSE)
summary(a.model)



# we are using this model
a.model<- ahaz( surv = Surv(my_rs$Runs+runif(nrow(my_rs))*1e-2,my_rs$delta), X= model.matrix(~Type+ Vanue+Chasing, data=my_rs)[,-1])
summary(a.model)
plot(a.model)

w<-predict(a.model, type = "cumhaz")
h1<-diff(w$cumhaz,1)
plot(w$times,h1, type="s", main="Hazard", xlab="Runs ",ylab="Failure rates")

# for test
lines(w$times,h1+ 0.00647, type="s", lty=3, col="blue")
# for home matchs
lines(w$times,h1-0.00253, type="s", lty=3, col="red")
# for defending
lines(w$times,h1+0.00154, type="s", lty=3, col="green3")
# test + defending + away
lines(w$times,h1+0.006442+0.001662, type="s", lty=2, col="brown")

# odi + home+ defending
lines(w$times,h1-0.002588-0.001662, type="s", lty=2, col="blue")

#legend("topleft", legend=c("Baseline", "Test", "Home matchs","Defend"),
#       lty=c(1,3,3,3), col=c("black", "blue","red","green3"), bty="n", inset=0.05)

legend("topleft", legend=c("Baseline", "Test+Away + Defend","ODI + Home + Defend"),
       lty=c(1,3,3), col=c("black", "brown","Blue"), bty="n", inset=0.05)




#===== check for proportionality of the hazard for sd
# For Venue
sd.ph.diag<- coxph(Surv(r,dx)~strata(Vanue),data=my_sd, method="breslow")
df.base.haz<-basehaz(sd.ph.diag) 
diag.a<- df.base.haz %>%  filter(strata=="A")
diag.h<- df.base.haz %>%  filter(strata=="H")

plot( diag.a$time, log(diag.a$hazard), type="s", xlab="Runs", ylab="log cumulative hazard", main="Test for proportionality assumption for Venue variable" )
lines( diag.h$time, log(diag.h$hazard), type="s", lty=2 )
legend("bottomright", legend = c("Away","Home"),lty=c(1,2),bty="n", inset = 0.05, cex=0.8 )

# For Chasing
sd.ph.diag<- coxph(Surv(r,dx)~strata(Chasing),data=my_sd, method="breslow")
df.base.haz<-basehaz(sd.ph.diag) 
diag.a<- df.base.haz %>%  filter(strata=="c")
diag.b<- df.base.haz %>%  filter(strata=="d")

plot( diag.a$time, log(diag.a$hazard), type="s", xlab="Runs", ylab="log cumulative hazard", main="Test for proportionality assumption for Chasing variable" )
lines( diag.b$time, log(diag.b$hazard), type="s", lty=2 )
legend("bottomright", legend = c("Chase","defend"),lty=c(1,2),bty="n", inset = 0.05, cex=0.8 )

# For Type
sd.ph.diag<- coxph(Surv(r,dx)~strata(Type),data=my_sd, method="breslow")
df.base.haz<-basehaz(sd.ph.diag) 
diag.a<- df.base.haz %>%  filter(strata=="ODI")
diag.b<- df.base.haz %>%  filter(strata=="Test")

plot( diag.a$time, log(diag.a$hazard), type="s", xlab="Runs", ylab="log cumulative hazard", main="Test for proportionality assumption for Type variable" )
lines( diag.b$time, log(diag.b$hazard), type="s", lty=2 )
legend("bottomright", legend = c("ODI","Test"),lty=c(1,2),bty="n", inset = 0.05, cex=0.8 )

sd.ph<- coxph(Surv(Runs, delta)~Chasing+Vanue,data=my_sd, method="breslow")
sd.ph
cox.zph(coxph(Surv(Runs, delta)~Chasing+Vanue,data=my_sd, method="breslow"))

# cox snell residuals
my_sd$mart<- residuals(sd.ph, type = "martingale")
my_sd$cox_snell<- -(my_sd$mart - my_sd$delta)
# Fit model on Cox-Snell residuals (Approximately Expo(1) distributed under correct model)
sd.coxsnell<- survfit(Surv(cox_snell,delta)~1,data=my_sd)
sd.coxsnell$cumhaz
plot(sd.coxsnell$time,sd.coxsnell$cumhaz, type="s", pch=19,
     main="Cox Snell Residuals", xlab = "times for cox snell residuals",
     ylab="cumulative hazard of cox snell residuals")
abline(0,1, lty=2, col="red")



# additive hazard model
# we are using this model
a.model<- ahaz( surv = Surv(my_sd$Runs+runif(nrow(my_sd))*1e-2,my_sd$delta), X= model.matrix(~Type+ Vanue+Chasing, data=my_sd)[,-1])
summary(a.model)
plot(a.model)

w<-predict(a.model, type = "cumhaz")
h1<-diff(w$cumhaz,1)
plot(w$times,h1, type="s", main="Hazard", xlab="Runs ",ylab="Failure rates")

# for test
lines(w$times,h1+  0.006625 , type="s", lty=3, col="blue")
# for home matchs
lines(w$times,h1-0.000145, type="s", lty=3, col="red")
# for defending
lines(w$times,h1-0.000801 , type="s", lty=3, col="green3")
# test + defending + away
lines(w$times,h1+0.006634-0.0007194, type="s", lty=2, col="brown")

# odi + away+ defending
lines(w$times,h1-0.0002897-0.0007194, type="s", lty=2, col="blue")

#legend("topleft", legend=c("Baseline", "Test", "Home matchs","Defend"),
#       lty=c(1,3,3,3), col=c("black", "blue","red","green3"), bty="n", inset=0.05)

legend("topleft", legend=c("Baseline", "Test + Defend","ODI + Home + Defend"),
       lty=c(1,3,3), col=c("black", "brown","Blue"), bty="n", inset=0.05)


#===== check for proportionality of the hazard for dw
# For Venue
dw.ph.diag<- coxph(Surv(r,dx)~strata(Vanue),data=my_dw, method="breslow")
df.base.haz<-basehaz(dw.ph.diag) 
diag.a<- df.base.haz %>%  filter(strata=="A")
diag.h<- df.base.haz %>%  filter(strata=="H")

plot( diag.a$time, log(diag.a$hazard), type="s", xlab="Runs", ylab="log cumulative hazard", main="Test for proportionality assumption for Venue variable" )
lines( diag.h$time, log(diag.h$hazard), type="s", lty=2 )
legend("bottomright", legend = c("Away","Home"),lty=c(1,2),bty="n", inset = 0.05, cex=0.8 )

# For Chasing
dw.ph.diag<- coxph(Surv(r,dx)~strata(Chasing),data=my_dw, method="breslow")
df.base.haz<-basehaz(dw.ph.diag) 
diag.a<- df.base.haz %>%  filter(strata=="c")
diag.b<- df.base.haz %>%  filter(strata=="d")

plot( diag.a$time, log(diag.a$hazard), type="s", xlab="Runs", ylab="log cumulative hazard", main="Test for proportionality assumption for Chasing variable" )
lines( diag.b$time, log(diag.b$hazard), type="s", lty=2 )
legend("bottomright", legend = c("Chase","defend"),lty=c(1,2),bty="n", inset = 0.05, cex=0.8 )

# For Type
dw.ph.diag<- coxph(Surv(r,dx)~strata(Type),data=my_dw, method="breslow")
df.base.haz<-basehaz(dw.ph.diag) 
diag.a<- df.base.haz %>%  filter(strata=="ODI")
diag.b<- df.base.haz %>%  filter(strata=="Test")

plot( diag.a$time, log(diag.a$hazard), type="s", xlab="Runs", ylab="log cumulative hazard", main="Test for proportionality assumption for Type variable" )
lines( diag.b$time, log(diag.b$hazard), type="s", lty=2 )
legend("bottomright", legend = c("ODI","Test"),lty=c(1,2),bty="n", inset = 0.05, cex=0.8 )

# Coxph model/
dw.ph<- coxph(Surv(Runs,delta)~Vanue+Type,data=my_dw, method="breslow")
dw.ph
AIC(dw.ph)



# cox snell residuals
my_dw$mart<- residuals(dw.ph, type = "martingale")
my_dw$cox_snell<- -(my_dw$mart - my_dw$delta)
# Fit model on Cox-Snell residuals (Approximately Expo(1) distributed under correct model)
dw.coxsnell<- survfit(Surv(cox_snell,delta)~1,data=my_dw)
dw.coxsnell$cumhaz
plot(dw.coxsnell$time,dw.coxsnell$cumhaz, type="s", pch=19,
     main="Cox Snell Residuals", xlab = "times for cox snell residuals",
     ylab="cumulative hazard of cox snell residuals")
abline(0,1, lty=2, col="red")



# additive hazard model
# we are using this model
a.model<- ahaz( surv = Surv(my_dw$Runs+runif(nrow(my_dw))*1e-2,my_dw$delta), X= model.matrix(~Type+ Vanue+Chasing, data=my_dw)[,-1])
summary(a.model)
plot(a.model)

w<-predict(a.model, type = "cumhaz")
h1<-diff(w$cumhaz,1)
plot(w$times,h1, type="s", main="Hazard", xlab="Runs ",ylab="Failure rates")

# for test
lines(w$times,h1+ 0.001410 , type="s", lty=3, col="blue")
# for home matchs
lines(w$times,h1 -0.005174 , type="s", lty=3, col="red")
# for defending
lines(w$times,h1--0.005468, type="s", lty=3, col="green3")
# test + defending + away
lines(w$times,h1+0.001875-0.005507, type="s", lty=2, col="brown")

# odi + away+ defending
lines(w$times,h1-0.005507, type="s", lty=2, col="blue")

#legend("topleft", legend=c("Baseline", "Test", "Home matchs","Defend"),
#       lty=c(1,3,3,3), col=c("black", "blue","red","green3"), bty="n", inset=0.05)

legend("topleft", legend=c("Baseline", "Test+ Away+ Defend","ODI + Away + Defend"),
       lty=c(1,3,3), col=c("black", "brown","Blue"), bty="n", inset=0.05)



#===== check for proportionality of the hazard for ml
# For Venue
ml.ph.diag<- coxph(Surv(r,dx)~strata(Vanue),data=my_ml, method="breslow")
df.base.haz<-basehaz(ml.ph.diag) 
diag.a<- df.base.haz %>%  filter(strata=="A")
diag.h<- df.base.haz %>%  filter(strata=="H")

plot( diag.a$time, log(diag.a$hazard), type="s", xlab="Runs", ylab="log cumulative hazard", main="Test for proportionality assumption for Venue variable" )
lines( diag.h$time, log(diag.h$hazard), type="s", lty=2 )
legend("bottomright", legend = c("Away","Home"),lty=c(1,2),bty="n", inset = 0.05, cex=0.8 )

# For Chasing
ml.ph.diag<- coxph(Surv(r,dx)~strata(Chasing),data=my_ml, method="breslow")
df.base.haz<-basehaz(ml.ph.diag) 
diag.a<- df.base.haz %>%  filter(strata=="c")
diag.b<- df.base.haz %>%  filter(strata=="d")

plot( diag.a$time, log(diag.a$hazard), type="s", xlab="Runs", ylab="log cumulative hazard", main="Test for proportionality assumption for Chasing variable" )
lines( diag.b$time, log(diag.b$hazard), type="s", lty=2 )
legend("bottomright", legend = c("Chase","defend"),lty=c(1,2),bty="n", inset = 0.05, cex=0.8 )

# For Type
ml.ph.diag<- coxph(Surv(r,dx)~strata(Type),data=my_ml, method="breslow")
df.base.haz<-basehaz(ml.ph.diag) 
diag.a<- df.base.haz %>%  filter(strata=="ODI")
diag.b<- df.base.haz %>%  filter(strata=="Test")

plot( diag.a$time, log(diag.a$hazard), type="s", xlab="Runs", ylab="log cumulative hazard", main="Test for proportionality assumption for Type variable" )
lines( diag.b$time, log(diag.b$hazard), type="s", lty=2 )
legend("bottomright", legend = c("ODI","Test"),lty=c(1,2),bty="n", inset = 0.05, cex=0.8 )


# coxph model
# Coxph model
ml.ph<- coxph(Surv(Runs,delta)~+Vanue+Type+Chasing,data=my_ml, method="breslow")
ml.ph
AIC(ml.ph)



# cox snell residuals
my_ml$mart<- residuals(ml.ph, type = "martingale")
my_ml$cox_snell<- -(my_ml$mart - my_ml$delta)
# Fit model on Cox-Snell residuals (Approximately Expo(1) distributed under correct model)
ml.coxsnell<- survfit(Surv(cox_snell,delta)~1,data=my_ml)
ml.coxsnell$cumhaz
plot(ml.coxsnell$time,ml.coxsnell$cumhaz, type="s", pch=19,
     main="Cox Snell Residuals", xlab = "times for cox snell residuals",
     ylab="cumulative hazard of cox snell residuals")
abline(0,1, lty=2, col="red")



# additive hazard model
# we are using this model
a.model<- ahaz( surv = Surv(my_ml$Runs+runif(nrow(my_ml))*1e-2,my_ml$delta), X= model.matrix(~Type+ Vanue+Chasing, data=my_ml)[,-1])
summary(a.model)
plot(a.model)

w<-predict(a.model, type = "cumhaz")
h1<-diff(w$cumhaz,1)
plot(w$times,h1, type="s", main="Hazard", xlab="Runs ",ylab="Failure rates")

# for test
lines(w$times,h1-0.008042 , type="s", lty=3, col="blue")
# for home matchs
lines(w$times,h1-0.003602 , type="s", lty=3, col="red")
# for defending
lines(w$times,h1-0.011923, type="s", lty=3, col="green3")
# test + defending + away
lines(w$times,h1-0.008042-0.0011923, type="s", lty=2, col="brown")

# odi + away+ defending
lines(w$times,h1-0.0011923, type="s", lty=2, col="blue")

#legend("topleft", legend=c("Baseline", "Test", "Home matchs","Defend"),
#       lty=c(1,3,3,3), col=c("black", "blue","red","green3"), bty="n", inset=0.05)

legend("topleft", legend=c("Baseline", "Test+Away+Defend","ODI+Away+Defend"),
       lty=c(1,3,3), col=c("black", "brown","Blue"), bty="n", inset=0.05)



# competing risk analysis
# preparing the data frame
summary(df_kl)
df_kl$Dismissal <- as.factor(df_kl$Dismissal)

df_kl$s.bowled<- ifelse(df_kl$Dismissal=="bowled",1,0)
df_kl$s.caught<- ifelse(df_kl$Dismissal=="caught",2,0)
df_kl$s.lbw<- ifelse(df_kl$Dismissal=="lbw",3,0)
df_kl$status<- df_kl$s.bowled+df_kl$s.caught+df_kl$s.lbw

df_kl.test<- subset(df_kl, Type=="Test")
df_kl.odi<- subset(df_kl, Type=="ODI")
#test
ci<- Cuminc(time = df_kl.test$Runs, status = df_kl.test$status)
#ci
names(ci)
bo<- ci$CI.1
ca<- ci$CI.2
lb<- ci$CI.3
times<- ci$time
plot(ca~times, type="s", col="purple", xlab="Runs",ylab="Cumulative Incidence function", main="KL Rahul in Test", ylim=c(0,max(bo,ca,lb)), )
lines(bo~times, type="s", col="red")
lines(lb~times, type="s",col="orange4")
legend("topleft", legend = c("Bowled","Caught","lbw"), 
       col=c("red","purple","orange4"),bty="n",lty=1, cex=0.6)

# odi
ci<- Cuminc(time = df_kl.odi$Runs, status = df_kl.odi$status)
#ci
names(ci)
bo<- ci$CI.1
ca<- ci$CI.2
lb<- ci$CI.3
times<- ci$time
plot(ca~times, type="s", col="purple", xlab="Runs",ylab="Cumulative Incidence function", main="KL Rahul in odi", ylim=c(0,max(bo,ca,lb)), )
lines(bo~times, type="s", col="red")
lines(lb~times, type="s",col="orange4")
legend("topleft", legend = c("Bowled","Caught","lbw"), 
       col=c("red","purple","orange4"),bty="n",lty=1, cex=0.6)


# for rohit 
df_rs$Dismissal <- as.factor(df_rs$Dismissal)

df_rs$s.bowled<- ifelse(df_rs$Dismissal=="bowled",1,0)
df_rs$s.caught<- ifelse(df_rs$Dismissal=="caught",2,0)
df_rs$s.lbw<- ifelse(df_rs$Dismissal=="lbw",3,0)
df_rs$status<- df_rs$s.bowled+df_rs$s.caught+df_rs$s.lbw

df_rs.test<- subset(df_rs, Type=="Test")
df_rs.odi<- subset(df_rs, Type=="ODI")
#test
ci<- Cuminc(time = df_rs.test$Runs, status = df_rs.test$status)
#ci
names(ci)
bo<- ci$CI.1
ca<- ci$CI.2
lb<- ci$CI.3
times<- ci$time
plot(ca~times, type="s", col="purple", xlab="Runs",ylab="Cumulative Incidence function", main="Rohit in Test", ylim=c(0,max(bo,ca,lb)), )
lines(bo~times, type="s", col="red")
lines(lb~times, type="s",col="orange4")
legend("topleft", legend = c("Bowled","Caught","lbw"), 
       col=c("red","purple","orange4"),bty="n",lty=1, cex=0.6)

# odi
ci<- Cuminc(time = df_rs.odi$Runs, status = df_rs.odi$status)
#ci
names(ci)
bo<- ci$CI.1
ca<- ci$CI.2
lb<- ci$CI.3
times<- ci$time
plot(ca~times, type="s", col="purple", xlab="Runs",ylab="Cumulative Incidence function", main="Rohit in odi", ylim=c(0,max(bo,ca,lb)), )
lines(bo~times, type="s", col="red")
lines(lb~times, type="s",col="orange4")
legend("topleft", legend = c("Bowled","Caught","lbw"), 
       col=c("red","purple","orange4"),bty="n",lty=1, cex=0.6)



# for shikhar
df_sd$Dismissal <- as.factor(df_sd$Dismissal)

df_sd$s.bowled<- ifelse(df_sd$Dismissal=="bowled",1,0)
df_sd$s.caught<- ifelse(df_sd$Dismissal=="caught",2,0)
df_sd$s.lbw<- ifelse(df_sd$Dismissal=="lbw",3,0)
df_sd$status<- df_sd$s.bowled+df_sd$s.caught+df_sd$s.lbw

df_sd.test<- subset(df_sd, Type=="Test")
df_sd.odi<- subset(df_sd, Type=="ODI")
#test
ci<- Cuminc(time = df_sd.test$Runs, status = df_sd.test$status)
#ci
names(ci)
bo<- ci$CI.1
ca<- ci$CI.2
lb<- ci$CI.3
times<- ci$time
plot(ca~times, type="s", col="purple", xlab="Runs",ylab="Cumulative Incidence function", main="Shikhar in Test", ylim=c(0,max(bo,ca,lb)), )
lines(bo~times, type="s", col="red")
lines(lb~times, type="s",col="orange4")
legend("topleft", legend = c("Bowled","Caught","lbw"), 
       col=c("red","purple","orange4"),bty="n",lty=1, cex=0.6)

# odi
ci<- Cuminc(time = df_sd.odi$Runs, status = df_sd.odi$status)
#ci
names(ci)
bo<- ci$CI.1
ca<- ci$CI.2
lb<- ci$CI.3
times<- ci$time
plot(ca~times, type="s", col="purple", xlab="Runs",ylab="Cumulative Incidence function", main="Shikhar in odi", ylim=c(0,max(bo,ca,lb)), )
lines(bo~times, type="s", col="red")
lines(lb~times, type="s",col="orange4")
legend("topleft", legend = c("Bowled","Caught","lbw"), 
       col=c("red","purple","orange4"),bty="n",lty=1, cex=0.6)


# for David
df_dw$Dismissal <- as.factor(df_dw$Dismissal)

df_dw$s.bowled<- ifelse(df_dw$Dismissal=="bowled",1,0)
df_dw$s.caught<- ifelse(df_dw$Dismissal=="caught",2,0)
df_dw$s.lbw<- ifelse(df_dw$Dismissal=="lbw",3,0)
df_dw$status<- df_dw$s.bowled+df_dw$s.caught+df_dw$s.lbw

df_dw.test<- subset(df_dw, Type=="Test")
df_dw.odi<- subset(df_dw, Type=="ODI")
#test
ci<- Cuminc(time = df_dw.test$Runs, status = df_dw.test$status)
#ci
names(ci)
bo<- ci$CI.1
ca<- ci$CI.2
lb<- ci$CI.3
times<- ci$time
plot(ca~times, type="s", col="purple", xlab="Runs",ylab="Cumulative Incidence function", main="Warner in Test", ylim=c(0,max(bo,ca,lb)), )
lines(bo~times, type="s", col="red")
lines(lb~times, type="s",col="orange4")
legend("topleft", legend = c("Bowled","Caught","lbw"), 
       col=c("red","purple","orange4"),bty="n",lty=1, cex=0.6)

# odi
ci<- Cuminc(time = df_dw.odi$Runs, status = df_dw.odi$status)
#ci
names(ci)
bo<- ci$CI.1
ca<- ci$CI.2
lb<- ci$CI.3
times<- ci$time
plot(ca~times, type="s", col="purple", xlab="Runs",ylab="Cumulative Incidence function", main="Warner in odi", ylim=c(0,max(bo,ca,lb)), )
lines(bo~times, type="s", col="red")
lines(lb~times, type="s",col="orange4")
legend("topleft", legend = c("Bowled","Caught","lbw"), 
       col=c("red","purple","orange4"),bty="n",lty=1, cex=0.6)




# for labuschagne
df_ml$Dismissal <- as.factor(df_ml$Dismissal)

df_ml$s.bowled<- ifelse(df_ml$Dismissal=="bowled",1,0)
df_ml$s.caught<- ifelse(df_ml$Dismissal=="caught",2,0)
df_ml$s.lbw<- ifelse(df_ml$Dismissal=="lbw",3,0)
df_ml$status<- df_ml$s.bowled+df_ml$s.caught+df_ml$s.lbw

df_ml.test<- subset(df_ml, Type=="Test")
df_ml.odi<- subset(df_ml, Type=="ODI")
#test
ci<- Cuminc(time = df_ml.test$Runs, status = df_ml.test$status)
#ci
names(ci)
bo<- ci$CI.1
ca<- ci$CI.2
lb<- ci$CI.3
times<- ci$time
plot(ca~times, type="s", col="purple", xlab="Runs",ylab="Cumulative Incidence function", main="Labuschgne in Test", ylim=c(0,max(bo,ca,lb)), )
lines(bo~times, type="s", col="red")
lines(lb~times, type="s",col="orange4")
legend("topleft", legend = c("Bowled","Caught","lbw"), 
       col=c("red","purple","orange4"),bty="n",lty=1, cex=0.6)

# odi
ci<- Cuminc(time = df_ml.odi$Runs, status = df_ml.odi$status)
#ci
names(ci)
bo<- ci$CI.1
ca<- ci$CI.2
lb<- ci$CI.3
times<- ci$time
plot(ca~times, type="s", col="purple", xlab="Runs",ylab="Cumulative Incidence function", main="labuschagne in odi", ylim=c(0,max(bo,ca,lb)), )
lines(bo~times, type="s", col="red")
lines(lb~times, type="s",col="orange4")
legend("topleft", legend = c("Bowled","Caught","lbw"), 
       col=c("red","purple","orange4"),bty="n",lty=1, cex=0.6)










