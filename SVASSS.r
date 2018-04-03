list.of.packages <- c("gdata", "timeDate","surveillance","abind","qcc","caTools","tseries","np",
                      "fitdistrplus","pscl","sendmailR","rgdal","ggmap","mapproj","maps","stringr","ISOweek", "devtools",
                      "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)




require(gdata)          #install.packages("gdata")
require(timeDate)       #install.packages("timeDate")
require(surveillance)   #install.packages("surveillance")
require(abind)          #install.packages("abind")
require(qcc)            #install.packages("qcc")
require(caTools)        #install.packages("caTools")
require(tseries)        #install.packages("tseries")
require(np)             #install.packages("np")
require(fitdistrplus)   #install.packages("fitdistrplus")
require(pscl)           #install.packages("pscl")
require(sendmailR)      #install.packages("mail")
require(rgdal)
require(ggmap)
require(mapproj)
require(maps)
require(stringr)
require(ISOweek)
require(devtools)
require(lubridate)

rm(list.of.packages)
rm(new.packages)


install_github("nandadorea/vetsyn")
require(vetsyn)


#rm(list=ls())


source("Functions.r",local=TRUE,encoding="native.enc")
source("Definitions.r",local=TRUE,encoding="native.enc")



# new data in ----

current.SVA.data  <- read.csv(paste0(wd.working,"\\today.data.csv"), as.is=TRUE, header=T, sep=";")
  source("SVA.data_classification_fixes.r",local=TRUE,encoding="native.enc")
  current.SVA.data <- current.SVA.data[which(current.SVA.data$active2==0),]

current.CDB.data  <- read.csv(paste0(wd.working,"\\CDB.csv"), as.is=TRUE, header=T, sep=";")
current.SJV.data  <- read.csv2(paste0(wd.working,"\\classified_djursjukdata.csv"))

PPN.database <- read.csv2(paste0(wd.working,"\\ppn_alla.csv"), as.is=TRUE)
PPN.database$X <- as.numeric(PPN.database$X)
PPN.database$Y <- as.numeric(PPN.database$Y)


    
# historical data in ----
    
    for(species in species.acronyms){
      eval(parse(text=paste0("load('history_files/",species,".RData')")))
    }
    
    daily.object <- list(CAT=CAT.daily,
                         BOV=BOV.daily,
                         DOG=DOG.daily,
                         ENV=ENV.daily,
                         FOD=FOD.daily,
                         FSK=FSK.daily,
                         EQU=EQU.daily,
                         AVI=AVI.daily,
                         SRU=SRU.daily,
                         SWI=SWI.daily,
                         VLT=VLT.daily)
    
    
    weekly.object <- list(CAT=CAT.weekly,
                         BOV=BOV.weekly,
                         DOG=DOG.weekly,
                         ENV=ENV.weekly,
                         FOD=FOD.weekly,
                         FSK=FSK.weekly,
                         EQU=EQU.weekly,
                         AVI=AVI.weekly,
                         SRU=SRU.weekly,
                         SWI=SWI.weekly,
                         VLT=VLT.weekly)


# define border between historical and new data ----
    
    new.data.start <- min(as.Date(current.SVA.data$ANKOMSTDATUM,format="%d/%m/%Y"))  
    new.data.end   <- max(as.Date(current.SVA.data$ANKOMSTDATUM,format="%d/%m/%Y"))
    next.monday <- nextweekday(new.data.start,2)
    
    last.historical.row.5days <- (which(BOV.daily@dates[,1]==new.data.start)-1)
    last.historical.row.week  <- (which(as.character(BOV.weekly@dates[,1])==as.character(date2ISOweek(next.monday)))-1)
    
    today <- strptime(as.character(new.data.end), format = "%Y-%m-%d")
    weekly <- FALSE
    if(today$wday==5|PlotDaily==1)(weekly<-TRUE)
    
    
    
# create alarm objects ----
    
    true.alarms.daily <- list(
      CAT=NA,
      BOV=NA,
      DOG=NA,
      ENV=NA,
      FOD=NA,
      FSK=NA,
      EQU=NA,
      AVI=NA,
      SRU=NA,
      SWI=NA,
      VLT=NA
    )
    
    scnd.alarms.daily  <- true.alarms.daily
    true.alarms.weekly <- true.alarms.daily
    scnd.alarms.weekly <- true.alarms.daily
    
    
# species loops ----
    source("r_files\\BOV.r",local=TRUE,encoding="native.enc")
    setwd(wd.working)
    source("r_files\\AVI.r",local=TRUE,encoding="native.enc")
    setwd(wd.working)
    source("r_files\\CAT.r",local=TRUE,encoding="native.enc")
    setwd(wd.working)
    source("r_files\\EQU.r",local=TRUE,encoding="native.enc")
    setwd(wd.working)
    source("r_files\\DOG.r",local=TRUE,encoding="native.enc")
    setwd(wd.working)
    source("r_files\\SWI.r",local=TRUE,encoding="native.enc")
    setwd(wd.working)
    source("r_files\\SRU.r",local=TRUE,encoding="native.enc")
    setwd(wd.working)
    source("r_files\\VLT.r",local=TRUE,encoding="native.enc")
    setwd(wd.working)
    source("r_files\\ENV.r",local=TRUE,encoding="native.enc")
    setwd(wd.working)
    source("r_files\\FOD.r",local=TRUE,encoding="native.enc")
    setwd(wd.working)
    source("r_files\\FSK.r",local=TRUE,encoding="native.enc")
    setwd(wd.working)
 
       
# saving data for apps ----
 load("svala.data.RData")
 #svala.data.dates
 #current.data.dates
 #min.date.current
 #max.data.current
 
 keep.svala <- which((svala.data.dates<min(as.Date(current.data$ANKOMSTDATUM,format="%d/%m/%Y")))&(svala.data.dates>(max(as.Date(current.data$ANKOMSTDATUM,format="%d/%m/%Y"))-550)))
  svala.data <- rbind(svala.data[keep.svala,],
                     current.data)
 svala.data.dates <- as.Date(svala.data$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
  save(svala.data,svala.data.dates,file="svala.data.RData")
 
 
 
  
  # emails ----    
  
status.true <- sapply(true.alarms.daily,sum,na.rm=TRUE)
  status.scnd <- sapply(scnd.alarms.daily,sum,na.rm=TRUE)
  
  if(weekly){
    status.true <- status.true + sapply(true.alarms.weekly,sum,na.rm=TRUE)
    status.scnd <- status.scnd + sapply(scnd.alarms.weekly,sum,na.rm=TRUE)
  }
  
  
body <- list("please visit <http://webutv/ESS/SVASSS/>")


#Emaillist<-paste ("<fernanda.dorea@sva.se>", "<ESS-alla@sva.se>", sep=",")
Emaillist<-"<fernanda.dorea@sva.se>"
#Emaillist<-EmailRecipient1


if (sum(status.true,na.rm=TRUE)>0)({
  from <- "<fernanda.dorea@sva.se>"
  to <- Emaillist
  subject <- paste((Sys.Date()-1),"TRUE-ALARM: There are TRUE alarms today",sep=",")
  sendmail(from, to, subject, body,control=list(smtpServer="smtp1.sva.se"))
})     


if (sum(status.true,na.rm=TRUE)==0&sum(status.scnd,na.rm=TRUE)>0)({
  from <- "<fernanda.dorea@sva.se>"
  to <- Emaillist
  subject <- paste((Sys.Date()-1),"sec.alarm: There are secondary alarms today",sep=",")
  sendmail(from, to, subject, body,control=list(smtpServer="smtp1.sva.se"))
})


if (sum(status.true,na.rm=TRUE)==0&sum(status.scnd,na.rm=TRUE)==0)({
  from <- "<fernanda.dorea@sva.se>"
  to <- "<fernanda.dorea@sva.se>"
  subject <- paste((Sys.Date()-1),"calculations finished",sep=",")
  msg="No alarms today"
   sendmail(from, to, subject,msg, control=list(smtpServer="smtp1.sva.se"))
})



