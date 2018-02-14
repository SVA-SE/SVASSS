
#rm(list=ls())
#library(devtools)
#install_github("vetsyn",username="nandadorea")
require(vetsyn)


###################data in (load)

#load("SJV_syndromic.RData")
load("history_files\\SJV_syndromic.RData")

cd <- read.csv2("classified_djursjukdata.csv")
head(cd)
dates <- as.Date(cd$DATUM, format="%m/%d/%y")
cd <- cd[dates<=Sys.Date(),]







########## saving data for apps -------
load("sjv.data.RData")

sjv.data.dates <- as.Date(sjv.data$DATUM, format="%m/%d/%y")

#svala.data.dates
#current.data.dates
#min.date.current
#max.data.current

keep.sjv <- which((sjv.data.dates<min(dates))&(sjv.data.dates>(max(dates)-550)))

sjv.data <- rbind(sjv.data[keep.sjv,],
                    cd)

save(sjv.data,file="sjv.data.RData")
############################################## ###







not <- cd[cd$DJURSLAG=="Nöt",]
equ <- cd[cd$DJURSLAG=="Häst",]
swi <- cd[cd$DJURSLAG=="Gris (animalieprod)",]
sru <- cd[cd$DJURSLAG=="Får"|cd$DJURSLAG=="Get",]
avi <-cd[cd$DJURSLAG=="Anka/gås"|cd$DJURSLAG=="Kalkon"|
           cd$DJURSLAG=="Höns, inkl slaktkyckling"|cd$DJURSLAG=="Strutsdjur",]

SJV_data <- list(not, equ, swi, sru, avi)



new.data.window <- as.numeric(max(as.Date(cd$DATUM, format="%m/%d/%y")) - 
                    min(as.Date(cd$DATUM, format="%m/%d/%y")) + 1)

############### update syndromic objects

SJV_daily[[1]]<- update_syndromic(x=SJV_daily[[1]],
                                  id=PPN,
                                  syndromes.var=prediction.Syndrome., 
                                  add.syndromes=FALSE,
                                  dates.var=DATUM, 
                                  date.format="%m/%d/%y", 
                                  replace.dates=TRUE,
                                  data=not)

SJV_daily[[2]]<- update_syndromic(x=SJV_daily[[2]],
                                  id=DJURREF,
                                  syndromes.var=prediction.Syndrome., 
                                  add.syndromes=FALSE,
                                  dates.var=DATUM, 
                                  date.format="%m/%d/%y", 
                                  replace.dates=TRUE,
                                  data=equ)


SJV_daily[[3]]<- update_syndromic(x=SJV_daily[[3]],
                                  id=PPN,
                                  syndromes.var=prediction.Syndrome., 
                                  add.syndromes=FALSE,
                                  dates.var=DATUM, 
                                  date.format="%m/%d/%y", 
                                  replace.dates=TRUE,
                                  data=swi)

SJV_daily[[4]]<- update_syndromic(x=SJV_daily[[4]],
                                  id=PPN,
                                  syndromes.var=prediction.Syndrome., 
                                  add.syndromes=FALSE,
                                  dates.var=DATUM, 
                                  date.format="%m/%d/%y", 
                                  replace.dates=TRUE,
                                  data=sru)


SJV_weekly[[1]]<- update_syndromic(x=SJV_weekly[[1]],
                                  id=PPN,
                                  syndromes.var=prediction.Syndrome., 
                                  add.syndromes=FALSE,
                                  dates.var=DATUM, 
                                  date.format="%m/%d/%y", 
                                  replace.dates=TRUE,
                                  data=not)

SJV_weekly[[2]]<- update_syndromic(x=SJV_weekly[[2]],
                                  id=DJURREF,
                                  syndromes.var=prediction.Syndrome., 
                                  add.syndromes=FALSE,
                                  dates.var=DATUM, 
                                  date.format="%m/%d/%y", 
                                  replace.dates=TRUE,
                                  data=equ)


SJV_weekly[[3]]<- update_syndromic(x=SJV_weekly[[3]],
                                  id=PPN,
                                  syndromes.var=prediction.Syndrome., 
                                  add.syndromes=FALSE,
                                  dates.var=DATUM, 
                                  date.format="%m/%d/%y", 
                                  replace.dates=TRUE,
                                  data=swi)

SJV_weekly[[4]]<- update_syndromic(x=SJV_weekly[[4]],
                                  id=PPN,
                                  syndromes.var=prediction.Syndrome., 
                                  add.syndromes=FALSE,
                                  dates.var=DATUM, 
                                  date.format="%m/%d/%y", 
                                  replace.dates=TRUE,
                                  data=sru)

SJV_weekly[[5]]<- update_syndromic(x=SJV_weekly[[5]],
                                   id=PPN,
                                   syndromes.var=DJURSLAG, 
                                   add.syndromes=FALSE,
                                   dates.var=DATUM, 
                                   date.format="%m/%d/%y", 
                                   replace.dates=TRUE,
                                   data=avi)


########################################## detection
not.days  <- c(1,2,4,7,8,9,10,11,12,13,15,16,17,20,21,22)
not.weeks <- c(5,6,14,19)

equ.days  <- c(1,4,6,7,8,9,10,11,15,17,20,21)
equ.weeks <- c(2,5,12,14,16,18,19)

swi.days  <- c(7,8,9,11,12,20,21)
swi.weeks <- c(1,10,14,15,16,17,19,22)

sru.days  <- c(2,7,8,11,12,20)
sru.weeks <- c(1,5,6,9,10,14,16,19,21,22)

avi.weeks <- c(1,2,3,4)
  
SJV_daily_syndromes  <- list(not.days,equ.days,swi.days,sru.days)
SJV_weekly_syndromes <- list(not.weeks,equ.weeks,swi.weeks,sru.weeks,avi.weeks)

shew.limits <- c(2.75,3,3.25,3.75,4.25)
ewma.limits <- c(2.75,3,3.25,3.75,4.25)
hw.limits <- c(2.5,2.75,3,3.5,4)


for (l in 1:length(SJV_daily)){
  SJV_daily[[l]]<- holt_winters_synd(SJV_daily[[l]],                    
                                     syndromes=SJV_daily_syndromes[[l]],       
                                     evaluate.window=new.data.window,    
                                     frequency=7,          
                                     baseline.window=365,  
                                     limit.sd=hw.limits,
                                     nahead=7,             
                                     alpha=0.4,            
                                     beta=0,               
                                     gamma=0.15,           
                                     seasonal="additive",  
                                     correct.baseline=1,   
                                     alarm.dim=1,          
                                     UCL=3)  
  
  SJV_daily[[l]]<-ewma_synd(SJV_daily[[l]],                    
                            syndromes=SJV_daily_syndromes[[l]],       
                            evaluate.window=new.data.window,                                     
                            baseline.window=365,                                   
                            lambda=0.2,                                            
                            limit.sd=ewma.limits,                                 
                            guard.band=7,                                          
                            correct.baseline=FALSE,                                
                            alarm.dim=2,                                           
                            UCL=3,                                                 
                            LCL=FALSE,                                             
                            pre.process="glm",                                     
                            diff.window=7,                                         
                            family="nbinom",                                      
                            formula="dow+sin+cos+AR1+AR2+AR3+AR4+AR5+AR6+AR7",
                            frequency=365)                                                                 
  
  SJV_daily[[l]]<-shew_synd(SJV_daily[[l]],                    
                            syndromes=SJV_daily_syndromes[[l]],       
                            evaluate.window=new.data.window,                                     
                            baseline.window=365,                                   
                            limit.sd=ewma.limits,                                 
                            guard.band=7,                                          
                            correct.baseline=FALSE,                                
                            alarm.dim=3,                                           
                            UCL=3,                                                 
                            LCL=FALSE,                                             
                            pre.process="glm",                                     
                            diff.window=7,                                         
                            family="nbinom",                                      
                            formula="dow+sin+cos+AR1+AR2+AR3+AR4+AR5+AR6+AR7",
                            frequency=365)                                                                 
  
}



for (l in 1:length(SJV_weekly)){
  
  SJV_weekly[[l]]<-ewma_synd(SJV_weekly[[l]],                    
                            syndromes=SJV_weekly_syndromes[[l]],       
                            evaluate.window=ceiling(new.data.window/7),                                     
                            baseline.window=52,                                   
                            lambda=0.2,                                            
                            limit.sd=ewma.limits,                                 
                            guard.band=3,                                          
                            correct.baseline=FALSE,                                
                            alarm.dim=2,                                           
                            UCL=3,                                                 
                            LCL=FALSE,                                             
                            pre.process="glm",                                     
                            diff.window=7,                                         
                            family="nbinom",                                      
                            formula="sin+cos",
                            frequency=52)                                                                 
  
  SJV_weekly[[l]]<-shew_synd(SJV_weekly[[l]],                    
                            syndromes=SJV_weekly_syndromes[[l]],       
                            evaluate.window=ceiling(new.data.window/7),                                     
                            baseline.window=52,                                   
                            limit.sd=ewma.limits,                                 
                            guard.band=3,                                          
                            correct.baseline=FALSE,                                
                            alarm.dim=3,                                           
                            UCL=3,                                                 
                            LCL=FALSE,                                             
                            pre.process="glm",                                     
                            diff.window=7,                                         
                            family="nbinom",                                      
                            formula="sin+cos",
                            frequency=52)                                                                 
  
}







save(SJV_daily,SJV_weekly, file="history_files\\SJV_syndromic.rda")





################## assign daily and weekly syndromes
##########original objects kept as all syndromes so that later we can always change


#SJV_daily_syndromes  <- list(not.days,equ.days,swi.days,sru.days)
#SJV_weekly_syndromes <- list(not.weeks,equ.weeks,swi.weeks,sru.weeks,avi.weeks)


SJV_daily_only <- list()

for (l in 1:length(SJV_daily)){
  SJV_daily_only[[l]] <-syndromicD(observed=SJV_daily[[l]]@observed[,SJV_daily_syndromes[[l]]],
                                       dates=SJV_daily[[l]]@dates,
                                       baseline=SJV_daily[[l]]@baseline[,SJV_daily_syndromes[[l]]],
                                       alarms=SJV_daily[[l]]@alarms[,SJV_daily_syndromes[[l]],],
                                       UCL=SJV_daily[[l]]@UCL[,SJV_daily_syndromes[[l]],])  
}

SJV_weekly_only <- list()

for (l in 1:length(SJV_weekly)){
  SJV_weekly_only[[l]] <-syndromicW(observed=SJV_weekly[[l]]@observed[,SJV_weekly_syndromes[[l]]],
                                   dates=SJV_weekly[[l]]@dates,
                                   baseline=SJV_weekly[[l]]@baseline[,SJV_weekly_syndromes[[l]]],
                                   alarms=SJV_weekly[[l]]@alarms[,SJV_weekly_syndromes[[l]],],
                                   UCL=SJV_weekly[[l]]@UCL[,SJV_weekly_syndromes[[l]],])  
}

file.name_SJV_daily  = c("SJV_notD","SJV_equD","SJV_swiD","SJV_sruD")
file.name_SJV_weekly = c("SJV_notW","SJV_equW","SJV_swiW","SJV_sruW","SJV_aviW")

title_SJV_daily  = c("Djursjukdata - Cattle (daily syndromes)",
                     "Djursjukdata - Horses (daily syndromes)",
                     "Djursjukdata - Pigs (daily syndromes)",
                     "Djursjukdata - Small Ruminants (daily syndromes)")
title_SJV_weekly  = c("Djursjukdata - Cattle (weekly syndromes)",
                     "Djursjukdata - Horses (weekly syndromes)",
                     "Djursjukdata - Pigs (weekly syndromes)",
                     "Djursjukdata - Small Ruminants (weekly syndromes)",
                     "Djursjukdata - Poultry")




for (l in 1:length(SJV_daily)){
syndromic_page (x=SJV_daily_only[[l]],                            
                tpoints.display=7,                         
                file.name=file.name_SJV_daily[l],                      
                title=title_SJV_daily[l],      
                data.page=TRUE,                            
                data=SJV_data[[l]],                            
                date.format="%m/%d/%y",                    
                dates.var="DATUM",              
                syndromes.var="prediction.Syndrome.",                  
                scale=15) 
}


for (l in 1:length(SJV_weekly)){
syndromic_page (x=SJV_weekly_only[[l]],                            
                tpoints.display=4,                         
                file.name=file.name_SJV_weekly[l],                      
                title=title_SJV_weekly[l],      
                data.page=TRUE,                            
                data=SJV_data[[l]],                            
                date.format="%m/%d/%y",                    
                dates.var="DATUM",              
                syndromes.var="prediction.Syndrome.",                  
                scale=10) 
}



###############################################


