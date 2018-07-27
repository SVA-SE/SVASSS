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
require(data.table)

rm(list.of.packages)
rm(new.packages)


install_github("nandadorea/vetsyn")
require(vetsyn)


#rm(list=ls())
wd.running     <- "I:/ESS/SVASSS2/SVASSS/"
setwd(wd.running)

source("Functions.r",local=TRUE,encoding="native.enc")
source("Definitions.r",local=TRUE,encoding="native.enc")




# new data in ----

current.SVA.data  <- read.csv(paste0(wd.sourcefiles,"classified_data_SVA.csv"), as.is=TRUE, header=T, sep=";")
  source("SVA.data_classification_fixes.r",local=TRUE,encoding="native.enc")
  current.SVA.data <- current.SVA.data[which(current.SVA.data$active2==0),]

#current.CDB.data  <- read.csv(paste0(wd.sourcefiles,"classified_data_CDB.csv"), as.is=TRUE, header=T, sep=";")
#current.SJV.data  <- read.csv2(paste0(wd.sourcefiles,"classified_data_SJV.csv"))

PPN.database <- read.csv2(paste0(wd.sourcefiles,"ppn_alla.csv"), as.is=TRUE)
PPN.database$X <- as.numeric(PPN.database$X)
PPN.database$Y <- as.numeric(PPN.database$Y)


    
# historical data in ----
    
    for(species in species.acronyms){
      eval(parse(text=paste0("load('",wd.history,species,".RData')")))
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
    
    svaga.object <- list(CAT=CAT.svaga,
                         BOV=BOV.svaga,
                         DOG=DOG.svaga,
                         ENV=ENV.svaga,
                         FOD=FOD.svaga,
                         FSK=FSK.svaga,
                         EQU=EQU.svaga,
                         AVI=AVI.svaga,
                         SRU=SRU.svaga,
                         SWI=SWI.svaga,
                         VLT=VLT.svaga)
    
    
    non.svaga.object <- list(CAT=CAT.non.svaga,
                          BOV=BOV.non.svaga,
                          DOG=DOG.non.svaga,
                          ENV=ENV.non.svaga,
                          FOD=FOD.non.svaga,
                          FSK=FSK.non.svaga,
                          EQU=EQU.non.svaga,
                          AVI=AVI.non.svaga,
                          SRU=SRU.non.svaga,
                          SWI=SWI.non.svaga,
                          VLT=VLT.non.svaga)


# define border between historical and new data ----
    
    new.data.start <- min(as.Date(current.SVA.data$ANKOMSTDATUM,format="%d/%m/%Y"))  
    new.data.end   <- max(as.Date(current.SVA.data$ANKOMSTDATUM,format="%d/%m/%Y"))
    next.monday <- nextweekday(new.data.start,2)
    
    #make sure the new data always starts on a Monday so that it has full weeks:
    current.SVA.data <- current.SVA.data[as.Date(current.SVA.data$ANKOMSTDATUM,format="%d/%m/%Y")>=next.monday,]
    new.data.start <- next.monday  
    
    last.historical.row.5days <- (which(BOV.daily@dates[,1]==new.data.start)-1)
    last.historical.row.week  <- (which(as.character(BOV.weekly@dates[,1])==as.character(date2ISOweek(new.data.start)))-1)
    
    today <- strptime(as.character(new.data.end), format = "%Y-%m-%d")
    
    #on 2018-05-23 SVASSS was converted to WEEKLY only and always
    #daily analysis are still ran and recorded, so that we can always revert,
    #but they generate no emails or html output
              weekly <- TRUE
              #weekly <- FALSE
              #if(today$wday==5|PlotDaily==1)(weekly<-TRUE)
    
    
              
    
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
    source("BOV.r",local=TRUE,encoding="native.enc")
    source("AVI.r",local=TRUE,encoding="native.enc")
    source("CAT.r",local=TRUE,encoding="native.enc")
    source("EQU.r",local=TRUE,encoding="native.enc")
    source("DOG.r",local=TRUE,encoding="native.enc")
    source("SWI.r",local=TRUE,encoding="native.enc")
    source("SRU.r",local=TRUE,encoding="native.enc")
    source("VLT.r",local=TRUE,encoding="native.enc")
    source("ENV.r",local=TRUE,encoding="native.enc")
    source("FOD.r",local=TRUE,encoding="native.enc")
    source("FSK.r",local=TRUE,encoding="native.enc")
    
       
# saving data for apps ----
 load(paste0(wd.sourcefiles,"svala.data.RData"))
 #svala.data.dates
 #current.data.dates
 #min.date.current
 #max.data.current
 
 keep.svala <- which((svala.data.dates<min(as.Date(current.SVA.data$ANKOMSTDATUM,format="%d/%m/%Y")))&(svala.data.dates>(max(as.Date(current.SVA.data$ANKOMSTDATUM,format="%d/%m/%Y"))-550)))
  svala.data <- rbind(svala.data[keep.svala,],
                      current.SVA.data)
 svala.data.dates <- as.Date(svala.data$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
  save(svala.data,svala.data.dates,file=paste0(wd.sourcefiles,"/svala.data.RData"))
 
 
 
  
  # emails ----    

  #on 2018-05-23 SVASSS was converted to WEEKLY only and always
  #daily analysis are still ran and recorded, so that we can always revert,
  #but they generate no emails or html output
  
          # status.true <- sapply(true.alarms.daily,sum,na.rm=TRUE)
          #   status.scnd <- sapply(scnd.alarms.daily,sum,na.rm=TRUE)
          #   
          #   if(weekly){
          #     status.true <- status.true + sapply(true.alarms.weekly,sum,na.rm=TRUE)
          #     status.scnd <- status.scnd + sapply(scnd.alarms.weekly,sum,na.rm=TRUE)
          #   }
                 status.true <- sapply(true.alarms.weekly,sum,na.rm=TRUE)
                 status.scnd <- sapply(scnd.alarms.weekly,sum,na.rm=TRUE)
  
  
body <- list("please visit <http://webutv/ESS/SVASSS/>")


Emaillist<-"<ESS-alla@sva.se>"
#Emaillist<-"<fernanda.dorea@sva.se>"
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



# index/main html ---- 


setwd(wd.html)

html <- file("index.html", "w+")

cat("<html>\n", file=html)
cat("<head>\n", file=html)
cat(sprintf("<title>%s</title>\n", "SVASS"), file=html)
cat("</head>\n", file=html)

cat("<frameset cols=\"150px,*\">\n", file=html)
cat("<frame noresize=\"noresize\" src=\"nav.html\" name=\"nav\"/>\n", file=html)
cat("<frame noresize=\"noresize\" src=\"main.html\" name=\"main\"/>\n", file=html)
cat("</frameset>\n", file=html)

cat("</html>\n", file=html)

close(html)



html <- file("main.html", "w+")

cat("<html>\n", file=html)
cat("<head>\n", file=html)
cat(sprintf("<title>%s</title>\n", "SVASS main page"), file=html)
cat("</head>\n", file=html)

cat("<body>\n", file=html)

cat(sprintf('<h1 align="center">%s</h1>\n', "Syndromic surveillance at SVA"), file=html)
cat(sprintf('<h1 align="center">%s</h1>\n', Sys.Date()), file=html)
cat(sprintf('<h2 align="center">%s</h2>\n', "Select a group on the navigation menu to the left to see results"), file=html)

cat("<TABLE border=\"0\" align=\"center\">\n", file=html)
cat("<tr>\n", file=html)
cat("<td>System outputs are based on data up to the end of the PREVIOUS WEEK</td>\n", file=html)
cat("</tr>\n", file=html)

cat("<tr>\n", file=html)
cat("<td>Number of events per week correspond to the number of laboratory submissions, classified into syndromic groups by a computer system. </td>\n", file=html)
cat("</tr>\n", file=html)

cat("<tr>\n", file=html)
cat("<td> They reflect the number of ANIMALS tested per day for CATS and DOGS, and the number of HERDS otherwise</td>\n", file=html)
cat("</tr>\n", file=html)


cat("<tr>\n", file=html)
cat("<td>For questions contact Fernanda Dorea (fernanda.dorea@sva.se)</td>\n", file=html)
cat("</tr>\n", file=html)


cat("</table>  \n", file=html)
cat("</p>\n", file=html)



cat("<hr/>\n", file=html)

#    cat("<p align=\"center\">\n", file=html)
#    cat("<img src=\"summary.png\"/>\n", file=html)
#    cat("</p>\n", file=html)
#
cat("</body>\n", file=html)

cat("</html>\n", file=html)

close(html)





html <- file('nav.html', "w+")

cat("<html>\n", file=html)
cat("<head>\n", file=html)
cat(sprintf("<title>%s</title>\n", "SVASS menu"), file=html)
cat("</head>\n", file=html)

cat("<body>\n", file=html)

# Create navigation.
cat("<table border=0>\n", file=html)

cat("<tr>", file=html)
cat("<td colspan=3><a href=\"main.html\" target=\"main\">Main page</a></td>\n", file=html)
cat("</tr>\n", file=html)
cat("<tr>", file=html)
cat("<td colspan=3><a href=\"help.pdf\" target=\"main\">HELP</a></td>\n", file=html)
cat("</tr>\n", file=html)


cat("<tr><td colspan=3>&nbsp;</td></tr>\n", file=html)


#on 2018-05-23 SVASSS was converted to WEEKLY only and always
#daily analysis are still ran and recorded, so that we can always revert,
#but they generate no emails or html output
# for(species in 1:length(species.acronyms)) {
#   cat("<tr>", file=html)
#   
#   cat("<td>&nbsp;</td>", file=html)
#   
#   if(sapply(true.alarms.daily,sum,na.rm=TRUE)[species]>0) {
#     cat("<td bgcolor='red'>", file=html)
#   } else {
#     if(sapply(scnd.alarms.daily,sum,na.rm=TRUE)[species]>0){
#       cat("<td bgcolor='yellow'>", file=html)
#     } else{
#       cat("<td bgcolor='springgreen'>", file=html)
#     }
#   }
#   
#   cat("&nbsp;&nbsp;&nbsp;&nbsp;</td>", file=html)
#   
#   cat(sprintf("<td><a href=\"%s.html\" target=\"main\">%s</a></td>", 
#               paste0("html/",species.acronyms[species]), species.names[species]), file=html)
#   
#   cat("</tr>\n", file=html)
#   
# }



for(species in 1:length(species.acronyms)) {
  cat("<tr>", file=html)
  
  cat("<td>&nbsp;</td>", file=html)
  
  
  if(sapply(true.alarms.weekly,sum,na.rm=TRUE)[species]>0) {
    cat("<td bgcolor='red'>", file=html)
  } else {
    if(sapply(scnd.alarms.weekly,sum,na.rm=TRUE)[species]>0){
      cat("<td bgcolor='yellow'>", file=html)
    } else{
      cat("<td bgcolor='springgreen'>", file=html)
    }
  }
  
  cat("&nbsp;&nbsp;&nbsp;&nbsp;</td>", file=html)
  
  cat(sprintf("<td><a href=\"%s.html\" target=\"main\">%s</a></td>", 
              paste0("html/",species.acronyms[species]), 
              species.names[species]), file=html)
  
  cat("</tr>\n", file=html)
  
}



cat("</table>\n\n", file=html)

cat("</body>\n", file=html)

cat("</html>\n", file=html)

close(html)






