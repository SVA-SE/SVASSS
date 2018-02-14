
setwd(wd.html)


###########header
html <- file('Quality.html', "w+")

cat("<html>\n", file=html)
cat("<head>\n", file=html)
cat(sprintf("<title>%s</title>\n", paste("Classification Contol", Sys.Date(),sep="-")), file=html)
cat("</head>\n", file=html)

cat("<body>\n", file=html)





###### data in

current.data$Species3 <- as.character(current.data$Species3)
current.data$DJURSLAG <- as.character(current.data$DJURSLAG)
current.data$UNDERSÖKNING <- as.character(current.data$UNDERSÖKNING)
current.data$Specific <- as.character(current.data$Specific)
    current.data$Specific <- tolower(current.data$Specific)
current.data$General <- as.character(current.data$General)
  current.data$General <- tolower(current.data$General)





##############Analysis by SPECIES


###plot species
total.species <- unique(current.data[,c("UPPDRAG","Species3")])



setwd(wd.html.figures)
png(filename = "QUAL_species.png", width = 800, height = 400)   # numbers are the 10 syndromes
table.sp <- sort(table(total.species$Species3),decreasing=TRUE)
bar1=barplot(table.sp, ylim=c(0,(max(table.sp)+200)))
text(bar1,table.sp+100,labels=table.sp)
dev.off()
graphics.off()
setwd(wd.html)



cat(sprintf('<h2 align="center">%s</h2>\n', 
            "SPECIES"), file=html)
cat("<p align=\"center\">\n", file=html)
cat("<img src=\"",paste("figures//","QUAL_species.png",sep=""),"\"/>\n", file=html)
cat("</p>\n", file=html)



## lacking species
no.species <- current.data[which(current.data$Species3==""|current.data$Species3=="Unknown"),columns]

no.species.IM <- unique(no.species[,c("INSÄNTMATERIAL","DJURSLAG")])
no.species.UD <- unique(no.species[,c("UPPDRAG","DJURSLAG")])
no.species.US <- unique(no.species[,c("INSÄNTMATERIAL","UNDERSÖKNING")])


table1 <- as.data.frame(table(no.species.IM$DJURSLAG))
colnames(table1) <- c("DJURSLAG","Number of unique INSÄNTMATERIAL")

table2 <- as.data.frame(table(no.species.UD$DJURSLAG))
colnames(table2) <- c("DJURSLAG","Number of unique UPPDRAG")

table3 <- as.data.frame(sort(table(no.species.US$UNDERSÖKNING),decreasing=TRUE))
colnames(table3) <- "Number of unique INSÄNTMATERIAL"


cat(sprintf('<h3 align="center">%s</h3>\n', 
            "Number of tests missed from SVASSS due to lacking species"), file=html)
cat(print(xtable(table2), type="html"), file=html)
cat("<br></br>", file=html)

cat(print(xtable(table1), type="html"), file=html)
cat("<br></br>", file=html)

cat(sprintf('<h3 align="center">%s</h3>\n',"UNDERSÖKNING"), file=html)
cat(print(xtable(table3), type="html"), file=html)




########################## analysys by syndromes


###plot syndromes
total.syndromes <- unique(current.data[,c("UPPDRAG","SYNDROMIC")])



setwd(wd.html.figures)
png(filename = "QUAL_syndromes.png", width = 800, height = 400)   # numbers are the 10 syndromes
par(las=2,mar = c(8, 4, 2, 2))
table.synd <- sort(table(total.syndromes$SYNDROMIC),decreasing=TRUE)
bar1=barplot(table.synd, ylim=c(0,(max(table.synd)+200)))
text(bar1,table.synd+100,labels=table.synd)
dev.off()
graphics.off()
setwd(wd.html)



cat(sprintf('<h2 align="center">%s</h2>\n', 
            "SYNDROMES"), file=html)
cat("<p align=\"center\">\n", file=html)
cat("<img src=\"",paste("figures//","QUAL_syndromes.png",sep=""),"\"/>\n", file=html)
cat("</p>\n", file=html)



## syndrome NONSPECIFIC
nonspecific <- current.data[which(current.data$SYNDROMIC=="Nonspecific"),]

nonspecific.Spe <- unique(nonspecific[,c("Species3","INSÄNTMATERIAL","Specific")])
nonspecific.Gen <- unique(nonspecific[,c("Species3","INSÄNTMATERIAL","General")])
nonspecific.US  <- unique(nonspecific[,c("Species3","INSÄNTMATERIAL","UNDERSÖKNING")])


table4 <- table(nonspecific.Spe$Specific,nonspecific.Spe$Species3)

table5 <- table(nonspecific.Gen$General,nonspecific.Gen$Species3)

table6 <- table(nonspecific.US$UNDERSÖKNING,nonspecific.US$Species3)


cat(sprintf('<h3 align="center">%s</h3>\n', 
            "Details of individual samples not classified into syndromes"), file=html)
cat(print(xtable(table5), type="html"), file=html)
cat("<br></br>", file=html)

cat(print(xtable(table4), type="html"), file=html)
cat("<br></br>", file=html)

cat(print(xtable(table6), type="html"), file=html)










######################
cat(sprintf('<h2 align="center">%s</h2>\n', 
            "Detailed submissions missing species"), file=html)
cat(print(xtable(no.species), type="html"), file=html)


close(html)

