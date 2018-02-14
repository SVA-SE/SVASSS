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
setwd("I:\\ESS\\SVASyndromicSurveillanceSystem")


source("r_files\\Functions_2.0.r",local=TRUE,encoding="native.enc")
source("r_files\\Definitions_2.0.r",local=TRUE,encoding="native.enc")



# new data in ----

current.data      <- read.csv("today.data.csv", as.is=TRUE, header=T, sep=";")
current.CDB.data  <- read.csv("CDB.csv", as.is=TRUE, header=T, sep=";")
current.SJV.data  <- read.csv2("classified_djursjukdata.csv")

PPN.database <- read.csv2("ppn_alla.csv", as.is=TRUE)
PPN.database$X <- as.numeric(PPN.database$X)
PPN.database$Y <- as.numeric(PPN.database$Y)



#establishing a unique identification for animals and PPN, even if that information
#is missing in a given submission
DJURINDIVID = (current.data$DJURINDIVID+2)
PPN = (current.data$PPN+2)

DJURINDIVID[which(is.na(DJURINDIVID)==TRUE)]<- -1
PPN[which(is.na(PPN)==TRUE)]<- -1

d <- ave(DJURINDIVID,current.data$UPPDRAG,FUN=max,na.rm=TRUE)
p <- ave(PPN,current.data$UPPDRAG,FUN=max,na.rm=TRUE)

PPN_original=current.data$PPN
current.data=cbind(current.data,PPN_original)

current.data$DJURINDIVID[which(is.na(current.data$DJURINDIVID)==TRUE)]<- 
                          abs(d[which(is.na(current.data$DJURINDIVID)==TRUE)])
current.data$PPN[which(is.na(current.data$PPN)==TRUE)]<- 
                          abs(p[which(is.na(current.data$PPN)==TRUE)])

rm(DJURINDIVID, PPN, d, p, PPN_original)


# polish classification ----
# all fixes in the current data that are needed to adjust the syndromes from
#  "raw classified data"



current.data$SPECIES = as.vector(current.data$SPECIES)
current.data$Species2 = as.vector(current.data$Species2)
current.data$SPECIES [which(current.data$SPECIES=="?")] <- "Undefined"
current.data$SPECIES [which(current.data$SPECIES=="Unknown")] <- "Undefined"
current.data$SPECIES [which(current.data$SPECIES=="NONMAPPED")] <- "Undefined"

current.data$SPECIES [which(current.data$SPECIES=="Ruminant")] <- current.data$Species2[which(current.data$SPECIES=="Ruminant")]
current.data$SPECIES [which(current.data$SPECIES=="NONMAPPED")] <- "Cattle"
current.data$SPECIES [which(current.data$SPECIES=="?")] <- "Cattle"
current.data$SPECIES [which(current.data$SPECIES=="Canid/Felid")] <- current.data$Species2[which(current.data$SPECIES=="Canid/Felid")]
current.data$SPECIES [which(current.data$SPECIES=="Wild Canid")] <- "Dog"
current.data$SPECIES [which(current.data$SPECIES=="Wild Felid")] <- "Cat"
current.data$SPECIES [which(current.data$SPECIES=="Wild Ruminant")] <- "VLT"



    current.data$SYNDROMIC = as.vector(current.data$SYNDROMIC)
    current.data$SYNDROMIC [which(current.data$SYNDROMIC=="Eyes-Ears")] <- "EEO"
    current.data$SYNDROMIC [which(current.data$SYNDROMIC=="Oral Cavity")] <- "EEO"

    current.data$SYNDROMIC [which(current.data$SYNDROMIC=="Circulatory")] <- "CHH"
    current.data$SYNDROMIC [which(current.data$SYNDROMIC=="Haematopoietic")] <- "CHH"
    current.data$SYNDROMIC [which(current.data$SYNDROMIC=="Hepatic")] <- "CHH"


    
    current.data <- current.data[which(current.data$active2==0),]
    
    
    
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


# define merging parameters    
    new.data.start <- min(as.Date(current.data$ANKOMSTDATUM,format="%d/%m/%Y"))  
    new.data.end   <- max(as.Date(current.data$ANKOMSTDATUM,format="%d/%m/%Y"))
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


# index/main htmal ---- 


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
cat("<td>System outputs are based on data up to the end of the PREVIOUS day.</td>\n", file=html)
cat("</tr>\n", file=html)

cat("<tr>\n", file=html)
cat("<td>Number of events per day correspond to the number of laboratory submissions, classified into syndromic groups by a computer system. </td>\n", file=html)
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

  for(species in 1:length(species.acronyms)) {
    cat("<tr>", file=html)

    cat("<td>&nbsp;</td>", file=html)

    if(sapply(true.alarms.daily,sum,na.rm=TRUE)[species]>0) {
      cat("<td bgcolor='red'>", file=html)
    } else {
      if(sapply(scnd.alarms.daily,sum,na.rm=TRUE)[species]>0){
        cat("<td bgcolor='yellow'>", file=html)
      } else{
        cat("<td bgcolor='springgreen'>", file=html)
      }
          }

    cat("&nbsp;&nbsp;&nbsp;&nbsp;</td>", file=html)

    cat(sprintf("<td><a href=\"%s.html\" target=\"main\">%s</a></td>", 
                paste0("html/",species.acronyms[species]), species.names[species]), file=html)

    cat("</tr>\n", file=html)
    
  }
  
  
  
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
                paste0("html/",species.acronyms[species],"w"), 
                paste0(species.names[species],"-WEEKLY")), file=html)
    
    cat("</tr>\n", file=html)
    
  }
  
  

  cat("</table>\n\n", file=html)

  cat("</body>\n", file=html)

  cat("</html>\n", file=html)

  close(html)
  
  
  
  
  
  
  ####################################
  


