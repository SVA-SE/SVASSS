svaga_data <- svaga_data[,c("UPPDRAG","Active","SYNDROMIC",
                            "ÖVERORDNATUPPDRAG","PROVTAGNINGSORSAK",
                            "Department","INSÄNTMATERIAL","DIAGNOSER",
                            "RESULTATUNDERSÖKNING","RESULTATANALYS",
                            "AGENS","PÅVISAD","ANALYSBESKRIVNING",
                            "UNDERSÖKNINGBESKRIVNING","SPECIES","ANKOMSTDATUM")]


##if unique on everything but SVAGA, delete 
#(those are duplicated because I appended svaga data to regular data)
#but make sure to delete the one with AGENS empty, not the contrary

discard <- duplicated(svaga_data[,c("UPPDRAG","Active","SYNDROMIC",
                             "ÖVERORDNATUPPDRAG","PROVTAGNINGSORSAK",
                             "Department","INSÄNTMATERIAL","DIAGNOSER",
                             "RESULTATUNDERSÖKNING","RESULTATANALYS",
                             "ANALYSBESKRIVNING",
                             "UNDERSÖKNINGBESKRIVNING","SPECIES","ANKOMSTDATUM")],
                      fromLast=TRUE)
svaga_data <- svaga_data[which(discard==FALSE),]



date <- as.Date (svaga_data$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
date <- strptime (as.character(date), format = "%Y-%m-%d")
year <- (date$year+1900)
month <- date$mon; month <- month+1
mday <- date$mday

svaga_data <- cbind(svaga_data,year,month,mday)

current.date <- strptime (as.character(Sys.Date()), format = "%Y-%m-%d")
current.month <- current.date$mon+1


svaga_data <- svaga_data[svaga_data$month==current.month,]


##########

##1 - only those on svaga
svaga1 <- svaga_data[,c("UPPDRAG","Active","SYNDROMIC",
                       "ÖVERORDNATUPPDRAG","PROVTAGNINGSORSAK",
                       "Department","INSÄNTMATERIAL",
                       "AGENS","PÅVISAD","SPECIES","ANKOMSTDATUM",
                       "year","month","mday")]
svaga1$PÅVISAD <- as.character(svaga1$PÅVISAD)
svaga1$AGENS <- as.character(svaga1$AGENS)
  svaga1$AGENS <-str_replace_all(svaga1$AGENS,"[(+)]","")

svaga1 <- unique(svaga1)
dim1 = dim(svaga1)[1]

svaga1 <- svaga1[which(svaga1$AGENS!=""),]
dim2 = dim(svaga1)[1]

ej.svaga <- dim1-dim2


svaga1.table <- table(svaga1$AGENS,svaga1$PÅVISAD)
svaga1.table <- rbind (svaga1.table,c(sum(svaga1.table[,1]),sum(svaga1.table[,2])))
rownames(svaga1.table)[dim(svaga1.table)[1]]<- "TOTAL IN SVAGA"

Percentage.positive <- round(svaga1.table[,2]/(svaga1.table[,1]+svaga1.table[,2])*100,2)
svaga1.table <- cbind(svaga1.table, Percentage.positive)

svaga1.table <- rbind (svaga1.table,c(ej.svaga,0,0))
rownames(svaga1.table)[dim(svaga1.table)[1]]<- "NOT IN SVAGA"




library(shiny)
runApp("I:/ESS/SVASyndromicSurveillanceSystem/shiny")
barplot(summary(svaga_data$PÅVISAD))
