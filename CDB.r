setwd(wd.files)

species.label="CDB"

load("CDB.RData")
load("CDB.baseline.RData")
load("CDB.alarms.RData")



CDB.current = count.cases.day(case.variable=list(ppn,individnummer),syndrome.variable=KOD,syndrome.name=c("0","6","7","8","10"),date.variable=händelsedatum, date.format="%m/%d/%y",data=current.CDB.data,min.date=min.date.current,max.date=max.date.current)

                  sjalvdod = CDB.current[,4] +  CDB.current[,5]
                  CDB.current = cbind(CDB.current[,c(1,2,3)], sjalvdod)
                  colnames(CDB.current) = c("birth", "stillbirth","hemslakt","fallenstock")

CDB.baseline <- rbind(CDB.baseline[1:end.rows.historical.7days,],matrix(0,ncol=dim(CDB.baseline)[2],nrow=dim(CDB.current)[1]))
CDB <- rbind(CDB[1:end.rows.historical.7days,],CDB.current)

CDB.add.alarms  <- array(0,dim=c(dim(CDB.current)[1],dim(CDB.alarms.ewma)[2],dim(CDB.alarms.ewma)[3]))
CDB.alarms.ewma <- abind(CDB.alarms.ewma[1:end.rows.historical.7days,,],CDB.add.alarms,along=1)
CDB.alarms.csum <- abind(CDB.alarms.csum[1:end.rows.historical.7days,,],CDB.add.alarms,along=1)
CDB.alarms.shew <- abind(CDB.alarms.shew[1:end.rows.historical.7days,,],CDB.add.alarms,along=1)
CDB.alarms.hw   <- abind(CDB.alarms.hw  [1:end.rows.historical.7days,,],CDB.add.alarms,along=1)




########## saving data for apps -------
load("I:\\ESS\\SVASyndromicSurveillanceSystem\\CDB.data.RData")
#svala.data.dates
#current.data.dates
#min.date.current
#max.data.current

CDB.data.dates <- as.Date(CDB.data$händelsedatum, format="%m/%d/%y")
CDB.dates.current <- as.Date(current.CDB.data$händelsedatum, format="%m/%d/%y")


keep.CDB <- which((CDB.data.dates<min(CDB.dates.current))&(CDB.data.dates>(max(CDB.dates.current)-550)))

CDB.data <- rbind(CDB.data[keep.CDB,],
                  current.CDB.data)

save(CDB.data,file="CDB.data.RData")
#CDB.data <- current.CDB.data
############################################## ###




#####################################

start.time =  end.rows.historical.7days+1
limits.order <- c(3,1,2,4,5)



for (syndrome in           cdb.days.syndromes)  {      #syndrome=5
         for (uCI in        limits.order){        #uCI=limits.order[1]


result=     hw (data=CDB[,syndrome],
            baseline=CDB.baseline [1:end.rows.historical.7days,syndrome],
            year=dates.matrix$year,
            baseline.source=CDB.baseline[,syndrome],
            alarms=CDB.alarms.hw[,syndrome,uCI],
            start.time=start.time,uCI=uCI,nahead=cdb.nahead,
            baseline.window=cdb.baseline.window)

     CDB.baseline[,syndrome] = result[[1]]
     CDB.alarms.hw[,syndrome,uCI] = result[[2]]

                     }}



for (syndrome in           cdb.days.syndromes)  {      #syndrome=5
         for (uCI in        limits.order){        #uCI=limits.order[1]

 CDB.alarms.ewma[,syndrome,uCI] =     NB.CS.ewma (data=CDB[,syndrome],
                                    baseline=CDB.baseline[1:end.rows.historical.7days,syndrome],
                                    dow.source=as.factor(dates.matrix$dow),
                                    baseline.source=CDB.baseline[,syndrome],
                                    alarms=CDB.alarms.ewma[,syndrome,uCI],
                                    start.time=start.time,uCI=uCI,
                                    baseline.window=cdb.baseline.window,
                                    guard.band=cdb.guard.band,lambda=lambda,
                                    end.rows.historical=end.rows.historical.7days
                                    )

 CDB.alarms.shew[,syndrome,uCI] =     NB.CS.shew (data=CDB[,syndrome],
                                    baseline=CDB.baseline[1:end.rows.historical.7days,syndrome],
                                    dow.source=as.factor(dates.matrix$dow),
                                    baseline.source=CDB.baseline[,syndrome],
                                    alarms=CDB.alarms.shew[,syndrome,uCI],
                                    start.time=start.time,uCI=uCI,
                                    baseline.window=cdb.baseline.window,
                                    guard.band=cdb.guard.band,
                                    end.rows.historical=end.rows.historical.7days
                                    )
        }}
        

#############################################################################################
###############################reporting
#############################################################################################



CDB.alarms.ewma[,,6] <- CDB.alarms.ewma[,,1]+CDB.alarms.ewma[,,2]+CDB.alarms.ewma[,,3]+CDB.alarms.ewma[,,4]+CDB.alarms.ewma[,,5]
CDB.alarms.shew[,,6] <- CDB.alarms.shew[,,1]+CDB.alarms.shew[,,2]+CDB.alarms.shew[,,3]+CDB.alarms.shew[,,4]+CDB.alarms.shew[,,5]
CDB.alarms.hw[,,6] <- CDB.alarms.hw[,,1]+CDB.alarms.hw[,,2]+CDB.alarms.hw[,,3]+CDB.alarms.hw[,,4]+CDB.alarms.hw[,,5]


#SWI.alarm.total <- SWI.alarms.ewma[,,6]+SWI.alarms.shew[,,6]+SWI.alarms.hw[,,6]
CDB.alarm.total <- CDB.alarms.hw[,,6]


CDB.report.today <- which(CDB.alarm.total[dim(CDB.alarm.total)[1],]>=cdb.scores)



#############################################################################################
###############################FRIDAY REPORTS or if PLOTALLDAILY=1
#############################################################################################

#setwd("C:\\Users\\fernanda.dorea\\Documents\\Repository\\5-Implementation\\reports")



if (dates.matrix.5days[dim(dates.matrix.5days)[1],"mday"]==25|
      (dates.matrix.5days[dim(dates.matrix.5days)[1],"mday"]==26&dates.matrix.5days[dim(dates.matrix.5days)[1],"dow"]==2)|
      (dates.matrix.5days[dim(dates.matrix.5days)[1],"mday"]==27&dates.matrix.5days[dim(dates.matrix.5days)[1],"dow"]==2)){
  setwd("I:\\ESS\\SVASyndromicSurveillanceSystem\\CDBreports")
  PlotWindow.report <-425
  
pdf(file=paste((max.date.current),"CDB-RegularReport", "pdf", sep="."), height=7.5, width=10.5)

for (j in cdb.days.syndromes){
RegularReport.day (data= CDB[,j],
                    baseline= CDB.baseline[,j],
                    dates=dates.matrix[,1],
                    alarms.shew=CDB.alarms.shew[,j,6],
                    alarms.ewma=CDB.alarms.ewma[,j,6],
                    alarms.hw=CDB.alarms.hw[,j,6],
                    syndrome=j,
                    scores=cdb.scores,
                    label=cdb.syndromes,
                    main.title=TRUE,
                    endday=endday.cdb)
                          }


          dev.off()
                   graphics.off()


}


#############################################################################################
###############################DAILY: IF ALARM
#############################################################################################

setwd(wd.alarm.reports)


pdf(file=paste((max.date.current),"CDB-ALARM-REPORT", "pdf", sep="."), paper="letter", height=10)

for (j in CDB.report.today){


AlarmReport.day (data= CDB[,j],
                    baseline= CDB.baseline[,j],
                    dates=dates.matrix[,1],
                    alarms.shew=CDB.alarms.shew[,j,6],
                    alarms.ewma=CDB.alarms.ewma[,j,6],
                    alarms.hw=CDB.alarms.hw[,j,6],
                    syndrome=j,
                    scores=cdb.scores,
                    label=cdb.syndromes)
 }



              dev.off()
       graphics.off()

if (length(CDB.report.today)==0) (file.remove(paste((max.date.current),"CDB-ALARM-REPORT", "pdf", sep=".")))




#################saving data back
setwd(wd.files)

save(CDB, file="CDB.RData")
save(CDB.baseline, file="CDB.baseline.RData")
save(list=c("CDB.alarms.shew","CDB.alarms.ewma","CDB.alarms.csum","CDB.alarms.hw"), file="CDB.alarms.RData")





###########################################################################
###########################################################################

##current data tables

CDB.data.tables1 = list()
CDB.data.tables2 = list()
CDB.data.tables3 = list()

CDB.data.tables1[[1]] =  current.CDB.data[which(current.CDB.data$KOD=="0"&(as.Date(current.CDB.data$händelsedatum, format = "%m/%d/%y", origin="01/01/1970")==(max.date.current))),]
CDB.data.tables1[[2]] =  current.CDB.data[which(current.CDB.data$KOD=="10"&(as.Date(current.CDB.data$händelsedatum, format = "%m/%d/%y", origin="01/01/1970")==(max.date.current))),]
CDB.data.tables1[[3]] =  current.CDB.data[which(current.CDB.data$KOD=="6"&(as.Date(current.CDB.data$händelsedatum, format = "%m/%d/%y", origin="01/01/1970")==(max.date.current))),]
CDB.data.tables1[[4]] =  current.CDB.data[which((current.CDB.data$KOD=="7"|current.CDB.data$KOD=="8")&(as.Date(current.CDB.data$händelsedatum, format = "%m/%d/%y", origin="01/01/1970")==(max.date.current))),]

CDB.data.tables2[[1]] =  current.CDB.data[which(current.CDB.data$KOD=="0"&(as.Date(current.CDB.data$händelsedatum, format = "%m/%d/%y", origin="01/01/1970")==(max.date.current-5))),]
CDB.data.tables2[[2]] =  current.CDB.data[which(current.CDB.data$KOD=="10"&(as.Date(current.CDB.data$händelsedatum, format = "%m/%d/%y", origin="01/01/1970")==(max.date.current-5))),]
CDB.data.tables2[[3]] =  current.CDB.data[which(current.CDB.data$KOD=="6"&(as.Date(current.CDB.data$händelsedatum, format = "%m/%d/%y", origin="01/01/1970")==(max.date.current-5))),]
CDB.data.tables2[[4]] =  current.CDB.data[which((current.CDB.data$KOD=="7"|current.CDB.data$KOD=="8")&(as.Date(current.CDB.data$händelsedatum, format = "%m/%d/%y", origin="01/01/1970")==(max.date.current-5))),]

CDB.data.tables3[[1]] =  current.CDB.data[which(current.CDB.data$KOD=="0"),]
CDB.data.tables3[[2]] =  current.CDB.data[which(current.CDB.data$KOD=="10"),]
CDB.data.tables3[[3]] =  current.CDB.data[which(current.CDB.data$KOD=="6"),]
CDB.data.tables3[[4]] =  current.CDB.data[which(current.CDB.data$KOD=="7"|current.CDB.data$KOD=="8"),]


coordinates1 = list()
cdb.mapped1  = vector()

for (syndrome in 1:4){
  
  coordinates1[[syndrome]] = as.data.frame(CDB.data.tables2[[syndrome]][,"ppn"])
  coordinates1[[syndrome]] = as.data.frame(coordinates1[[syndrome]][(duplicated(coordinates1[[syndrome]])==FALSE),])
  X = rep(NA,dim(coordinates1[[syndrome]])[1])
  Y = rep(NA,dim(coordinates1[[syndrome]])[1])
  coordinates1[[syndrome]] = cbind(coordinates1[[syndrome]],X,Y)
    colnames(coordinates1[[syndrome]])=c("ppn","X","Y")
  
  if(dim(coordinates1[[syndrome]])[1]>0)({
    for (l in 1:dim(coordinates1[[syndrome]])[1]){
      if  (is.na(coordinates1[[syndrome]][l,"ppn"])==FALSE)    ({
        
        PPN.row <- match(coordinates1[[syndrome]][l,"ppn"],as.numeric(PPN.database$Ppn))
        coordinates1[[syndrome]][l,"X"] = PPN.database$X[PPN.row]
        coordinates1[[syndrome]][l,"Y"] = PPN.database$Y[PPN.row]
        
        
      })
    }
  })
  
  cdb.mapped1[syndrome] = length(which(is.na(coordinates1[[syndrome]][,"X"])==TRUE))
  coordinates1[[syndrome]]= coordinates1[[syndrome]][complete.cases(coordinates1[[syndrome]]),]
}




coordinates2 = list()
cdb.mapped2  = vector()

for (syndrome in 1:4){
  
  coordinates2[[syndrome]] = as.data.frame(CDB.data.tables3[[syndrome]][,"ppn"])
  coordinates2[[syndrome]] = as.data.frame(coordinates2[[syndrome]][(duplicated(coordinates2[[syndrome]])==FALSE),])
  X = rep(NA,dim(coordinates2[[syndrome]])[1])
  Y = rep(NA,dim(coordinates2[[syndrome]])[1])
  coordinates2[[syndrome]] = cbind(coordinates2[[syndrome]],X,Y)
      colnames(coordinates2[[syndrome]])=c("ppn","X","Y")
      
  if(dim(coordinates2[[syndrome]])[1]>0)({
    for (l in 1:dim(coordinates2[[syndrome]])[1]){
      if  (is.na(coordinates2[[syndrome]][l,"ppn"])==FALSE)    ({
        
        PPN.row <- match(coordinates2[[syndrome]][l,"ppn"],as.numeric(PPN.database$Ppn))
        coordinates2[[syndrome]][l,"X"] = PPN.database$X[PPN.row]
        coordinates2[[syndrome]][l,"Y"] = PPN.database$Y[PPN.row]
        
        
      })
    }
  })
  
  cdb.mapped2[syndrome] = length(which(is.na(coordinates2[[syndrome]][,"X"])==TRUE))
  coordinates2[[syndrome]]= coordinates2[[syndrome]][complete.cases(coordinates2[[syndrome]]),]
}




for (l in 1:length(coordinates1)){
  if (dim(coordinates1[[l]])[1]>0){
    coordinates1[[l]][which((coordinates1[[l]]["X"]>1900000|coordinates1[[l]]["X"]<1200000)),"X"]<- NA
    coordinates1[[l]][which((coordinates1[[l]]["Y"]>7700000|coordinates1[[l]]["Y"]<6100000)),"Y"]<- NA
    
  }
}

for (l in 1:length(coordinates2)){
  if (dim(coordinates2[[l]])[1]>0){
    coordinates2[[l]][which((coordinates2[[l]]["X"]>1900000|coordinates2[[l]]["X"]<1200000)),"X"]<- NA
    coordinates2[[l]][which((coordinates2[[l]]["Y"]>7700000|coordinates2[[l]]["Y"]<6100000)),"Y"]<- NA
    
  }
}






for (l in 1:length(coordinates1)){
  coordinates1[[l]] <- coordinates1[[l]][complete.cases(coordinates1[[l]]),]
}


for (l in 1:length(coordinates2)){
  coordinates2[[l]] <- coordinates2[[l]][complete.cases(coordinates2[[l]]),]
}

###########################################################################
###########################################################################



setwd(wd.html)

html <- file('CDB.html', "w+")

cat("<html>\n", file=html)
cat("<head>\n", file=html)
cat(sprintf("<title>%s</title>\n", paste(species.label, Sys.Date(),sep="-")), file=html)
cat("</head>\n", file=html)

cat("<body>\n", file=html)


cat(sprintf('<h1 align="center">%s</h1>\n', paste("Daily report of syndromes in",species.label, (max.date.current),sep=" ")), file=html)
cat('<a name="top"></a>\n', file=html)

cat(sprintf('<h3 align="center">%s</h3>\n', "Syndromes monitored DAILY - Number of SUBMISSIONS"), file=html)
alarms.table<-rep(0,length(cdb.days.syndromes)*7)
dim(alarms.table)<-c(length(cdb.days.syndromes),7)
#colnames(alarms.table)<-c("D-4","D-3","D-2","D-1","today","score")
rownames(alarms.table)<- cdb.syndromes[cdb.days.syndromes]
for (j in 1:length(cdb.days.syndromes)){
  #alarms.table[j,1:5] <- (SWI.alarms.shew[((endday-4):endday),swi.days.syndromes[j],6]+SWI.alarms.ewma[((endday-4):endday),swi.days.syndromes[j],6]+SWI.alarms.hw[((endday-4):endday),swi.days.syndromes[j],6])
  alarms.table[j,1:5] <- (CDB.alarms.hw[((endday.cdb-4):endday.cdb),cdb.days.syndromes[j],6])
  
  alarms.table[j,7]   <- (CDB.alarms.shew[endday.cdb,cdb.days.syndromes[j],6]+CDB.alarms.ewma[endday.cdb,cdb.days.syndromes[j],6])
  alarms.table[j,6]   <- cdb.scores[cdb.days.syndromes[j]]
  
}


counts.table<-rep(0,length(cdb.days.syndromes)*5)
dim(counts.table)<-c(length(cdb.days.syndromes),5)
for (j in 1:length(cdb.days.syndromes)){
  counts.table[j,1:5] <- (CDB[((endday.cdb-4):endday.cdb),cdb.days.syndromes[j]])
}


colors.table = rep ("F8F8FF",length(alarms.table))
dim(colors.table)<-c(length(cdb.days.syndromes),7)

for (r in 1:dim(colors.table)[1]){
  colors.table[r,5]<-"E5E5FF"
}


for (r in 1:dim(colors.table)[1]){
  for (c in 1:(dim(colors.table)[2]-1)){
    if (alarms.table[r,c]>0) (colors.table[r,c]<-"ADFF2F")
    if (alarms.table[r,c]>=alarms.table[r,6]) (colors.table[r,c]<-"FF0000")
  }}




cat("<TABLE border=\"1\" align=\"center\">\n", file=html)
cat("<tr>\n", file=html)
cat("<td></td>\n<td colspan=\"3\"><center><b>Today</b></center></td>\n<td>____</td>\n<td colspan=\"4\"><center><b>Previous Days History</b></center></td>\n", file=html)
cat("</tr>\n", file=html)

cat("<tr>\n", file=html)
cat("<td></td>\n<td>Events</td>\n<td><b>Main Alarm Today</b></td>\n<td>Secondary alarms</td>\n<td></td>\n<td>_D-1_</td>\n<td>_D-2_</td>\n<td>_D-3_</td>\n<td>_D-4_</td>\n", file=html)
cat("</tr>\n", file=html)

for (r in 1:dim(alarms.table)[1]){
  cat("<tr>\n", file=html)
  cat("<td><a href='#",rownames(alarms.table)[r],"'>",rownames(alarms.table)[r],"</a></td>\n<td BGCOLOR='",colors.table[r,5],"'><center>",counts.table[r,5],
      "</center></td>\n<td BGCOLOR=\"E5E5FF\"><center><img src=\"",paste("figures//meterA",alarms.table[r,5],".png",sep=""),
      "\" width=\"100\" height=\"60\" /></center></td>\n<td BGCOLOR=\"E5E5FF\"><center><img src=\"",paste("figures//meterB",alarms.table[r,7],".png",sep=""),
      "\" width=\"100\" height=\"60\" /></center></td>\n<td></td>\n<td BGCOLOR='",colors.table[r,4],"'><center>",counts.table[r,4],
      "</center></td>\n<td BGCOLOR='",colors.table[r,3],"'><center>",counts.table[r,3],
      "</center></td>\n<td BGCOLOR='",colors.table[r,2],"'><center>",counts.table[r,2],
      "</center></td>\n<td BGCOLOR='",colors.table[r,1],"'><center>",counts.table[r,1],
      "</center></td>\n", file=html)
  cat("</tr>\n", file=html)
}

cat("</table>  \n", file=html)






##########################################################



setwd(wd.html.figures)
par(mfrow=c(length(cdb.days.syndromes), 1))
png(filename = "CDB.D.%03d.png", width = 800, height = 600)   # numbers are the 10 syndromes

for (j in cdb.days.syndromes){
  
  
  RegularReport.day (data= CDB[,j]              ,
                     baseline= CDB.baseline[,j]  ,
                     dates=dates.matrix[,1]      ,
                     alarms.shew=CDB.alarms.shew[,j,6]     ,
                     alarms.ewma=CDB.alarms.ewma[,j,6]     ,
                     alarms.hw=CDB.alarms.hw[,j,6]        ,
                     syndrome=j                        ,
                     scores=cdb.scores              ,
                     label=cdb.syndromes            ,
                     main.title=FALSE,
                     endday=endday.cdb
  )
}
dev.off()
graphics.off()



for (s in 1:length(cdb.syndromes)){
  
  if (dim(coordinates2[[s]])[1]>0)({
    
    
    map.data2 = coordinates2[[s]][,2:3]
    names(map.data2) <- c('x', 'y')
    coordinates(map.data2)=~x+y
    
    proj4string(map.data2) <- CRS("+init=epsg:3021")
    map.data2 <- spTransform(map.data2, CRS("+init=epsg:4326"))
    
    
    map.data2 <- as.data.frame(map.data2)
    names(map.data2) <- c('lon', 'lat')     #names(map.data) <- c('lat', 'lon') 
    
    
    if (dim(coordinates1[[s]])[1]>0)({
      map.data1 = coordinates1[[s]][,2:3]
      names(map.data1) <- c('x', 'y')
      coordinates(map.data1)=~x+y
      
      proj4string(map.data1) <- CRS("+init=epsg:3021")
      map.data1 <- spTransform(map.data1, CRS("+init=epsg:4326"))
      
      
      map.data1 <- as.data.frame(map.data1)
      names(map.data1) <- c('lon', 'lat')     #names(map.data) <- c('lat', 'lon')
      
      map <- get_map(location = c(long=mean(map.data2$lon),lat=mean(map.data2$lat)), zoom = 6)
      p <- ggmap(map) + geom_point(colour="black", size = 4,aes(Y=lat, X=lon), data=map.data2) + geom_point(colour="red", size = 4,aes(Y=lat, X=lon), data=map.data1)  
      png(filename = paste0("CDB.maps.",s,".png"), width = 800, height = 800)   # numbers are the 10 syndromes
      print(p)
      dev.off()
      
    }) else ({  
      map <- get_map(location = c(long=mean(map.data2$lon),lat=mean(map.data2$lat)), zoom = 6)
      p <- ggmap(map) + geom_point(colour="black", size = 4,aes(Y=lat, X=lon), data=map.data2)
      png(filename = paste0("CDB.maps.",s,".png"), width = 800, height = 800)   # numbers are the 10 syndromes
      print(p)
      dev.off()
    })
    
  })  
}


cat("<br></br>", file=html)
cat("<br></br>", file=html)
cat("<br></br>", file=html)



setwd(wd.html)

for (p in 1:length(cdb.days.syndromes)){
  anchor = paste('<a name=" ',cdb.syndromes[cdb.days.syndromes[p]],'"></a>\n',sep="")
  cat(anchor, file=html)
  cat(sprintf('<h3 align="center">%s</h3>\n', paste(species.label, cdb.syndromes[cdb.days.syndromes[p]],"Regular Report",(max.date.current), sep=" ")), file=html)
  cat("<p align=\"center\">\n", file=html)
  cat("<img src=\"",paste("figures//CDB.D.",sprintf("%03d", p),".png",sep=""),"\"/>\n", file=html)
  cat("</p>\n", file=html)
  
  if (dim(coordinates2[[cdb.days.syndromes[p]]])[1]>0)({
    cat(sprintf('<h3 align="center">Location of submissions in the last 30 days (highlighting in RED those in the last 5 days)</h3>\n'), file=html)
    cat("<p align=\"center\">\n", file=html)
    cat("<img src=\"",paste0("figures//CDB.maps.",cdb.days.syndromes[p],".png"),"\"/>\n", file=html)
    cat(sprintf('<h3 align="center">Number of submissions without geo-coordinate: %s</h3>\n', paste(cdb.mapped1[cdb.days.syndromes[p]],"in the last 5 days, and ",cdb.mapped2[cdb.days.syndromes[p]], "in the last 30 days", sep=" ")), file=html)
    cat("</p>\n", file=html)
  })
  
  cat("<TABLE border=\"0\">\n", file=html)
  cat("<tr>\n", file=html)
  cat(sprintf("<td>\n<a href=\"#top\">Go back to top</a>\n</td>\n<td>&nbsp;&nbsp;&nbsp;&nbsp;</td>\n<td>\n<a href=\"html2/CDB%s.html\">SEE THE SUBMISSIONS FOR THIS SYNDROME</a>\n</td>\n",cdb.days.syndromes[p]), file=html)
  cat("</tr>\n", file=html)
  cat("</table>\n", file=html)
  
  cat("<br></br>", file=html)
  cat("<br></br>", file=html)
}

cat("</body>\n", file=html)
cat("</html>\n", file=html)

close(html)










for (syndrome in 1:length(cdb.syndromes)){
  html <- file(paste0("html2\\CDB",syndrome,".html"), "w+")
  
  
  cat("<html>\n", file=html)
  cat("<head>\n", file=html)
  cat(sprintf("<title>%s</title>\n", paste("CDB",cdb.syndromes[syndrome],sep=" ")), file=html)
  cat("</head>\n", file=html)
  
  cat("<body>\n", file=html)
  
  cat(sprintf('<h1 align="center">%s</h1>\n', paste("CDB",cdb.syndromes[syndrome],Sys.Date(),sep=" - ")), file=html)
  
  cat(sprintf("<a href=\"../CDB.html\">Go back to %s main page</a>\n",species.label), file=html)
  
  cat(sprintf('<h3 align="center">%s</h3>\n', "Data from last day (yesterday)"), file=html)
  cat(print(xtable(CDB.data.tables1[[syndrome]],digits=0), type="html"), file=html)
  
  cat("<TABLE border=\"0\">\n", file=html)
  cat("<tr>\n", file=html)
  cat(sprintf("<td>\n<a href=\"#top\">Go back to top</a>\n</td>\n<td>&nbsp;&nbsp;&nbsp;&nbsp;</td>\n<td>\n<a href=\"../CDB.html\">Go back to %s main page</a>\n</td>\n",species.label), file=html)
  cat("</tr>\n", file=html)
  cat("</table>\n", file=html)
  
  
  cat(sprintf('<h3 align="center">%s</h3>\n', "Data from the last 5 days"), file=html)
  cat(print(xtable(CDB.data.tables2[[syndrome]],digits=0), type="html"), file=html)
  
  cat("<TABLE border=\"0\">\n", file=html)
  cat("<tr>\n", file=html)
  cat(sprintf("<td>\n<a href=\"#top\">Go back to top</a>\n</td>\n<td>&nbsp;&nbsp;&nbsp;&nbsp;</td>\n<td>\n<a href=\"../CDB.html\">Go back to %s main page</a>\n</td>\n",species.label), file=html)
  cat("</tr>\n", file=html)
  cat("</table>\n", file=html)
  
  
  cat("</body>\n", file=html)
  
  cat("</html>\n", file=html)
  
  close(html)
  
}






