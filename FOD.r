sp.label="FOD"
sp.acron="FOD"
sp.position=which(species.acronyms==sp.acron)



# SVA data: ----

# refining syndromic classification for species specific syndromes ----

CD.species <-current.SVA.data[current.SVA.data$SPECIES==sp.label,] 

CD.species$SYNDROMIC[CD.species$General=="Mycotic"] <- "Mycotic"
CD.species$SYNDROMIC[CD.species$General=="Bacterial"] <- "Bacterial"
CD.species$SYNDROMIC[(!CD.species$General=="Mycotic")&(!CD.species$General=="Bacterial")] <- "Rest"


classified.species.data <- rbind(classified.species.data,CD.species)

# running vetsyn-based-engine ----
#(running the codes that are the same for every syndromic object, namely:
#   1. update ths yndromic object;
#   2. run detection algorithms;
#   3. report alarms)

if(dim(CD.species)[1]>0){

source("vetsyn-based-engine.r",local=TRUE,encoding="native.enc")}





# svaga.data.filtering ----

#2018-08-14: FOD was not doing svaga "by syndromes", but for all FOD The problem with that,
#is the way the structures of data are expected, in particular to plot things in Shiny
#revert back to syndrome specific summaries
source("svaga-data-filtering.r",local=TRUE,encoding="native.enc")

# 
# #for (s in c(sp.colnames[[sp.position]],"Nonspecific")){
# 
# #CD.syndrome <- CD.species[CD.species$SYNDROMIC==s,c("AGENS","P�VISAD","INS�NTMATERIAL","ANKOMSTDATUM")]
# CD.syndrome <- CD.species[,c("AGENS","P�VISAD","INS�NTMATERIAL","ANKOMSTDATUM")]
# 
# #non.svaga.object[[sp.position]][[s]]<-CD.species[CD.species$AGENS=="",]
# 
# 
# CD.syndrome<-CD.syndrome[CD.syndrome$AGENS!="",]
# 
# if(dim(CD.syndrome)[1]>0){
#   
#   svaga.object[[sp.position]][[s]] <- svaga.object[[sp.position]][[s]][1:last.historical.row.week,,,drop=FALSE]
#   
#   CD.syndrome$AGENS<- str_replace(CD.syndrome$AGENS, "\\(\\+\\)", "")
#   syndromes.name=unique(c(colnames(svaga.object[[sp.position]][[s]]),CD.syndrome$AGENS))
#   
#   a.all=raw_to_syndromicW(id=INS�NTMATERIAL,
#                           syndromes.var=AGENS,
#                           syndromes.name=syndromes.name,
#                           dates.var=ANKOMSTDATUM,
#                           date.format = "%d/%m/%Y",
#                           min.date=new.data.start,
#                           max.date=new.data.end,
#                           data=CD.syndrome)
#   
#   CD.syndrome.pos=CD.syndrome[CD.syndrome$P�VISAD=="P�visad",]
#   
#   a.pos=raw_to_syndromicW(id=INS�NTMATERIAL,
#                           syndromes.var=AGENS,
#                           syndromes.name=syndromes.name,
#                           dates.var=ANKOMSTDATUM,
#                           date.format = "%d/%m/%Y",
#                           min.date=new.data.start,
#                           max.date=new.data.end,
#                           data=CD.syndrome.pos)
#   
#   svaga.new.all <- rbindlist(list(as.data.frame(svaga.object[[sp.position]][[s]][,,1]),
#                                   as.data.frame(a.all@observed)),
#                              fill=TRUE)
#   svaga.new.pos <- rbindlist(list(as.data.frame(svaga.object[[sp.position]][[s]][,,2]),
#                                   as.data.frame(a.pos@observed)),
#                              fill=TRUE)
#   
#   
#   svaga.object[[sp.position]][[s]] <- abind(svaga.new.all,svaga.new.pos,along=3)
#   
# }


#}


assign(paste0(sp.acron,".svaga"),svaga.object[[sp.position]])
#assign(paste0(sp.acron,".non.svaga"),non.svaga.object[[sp.position]])
assign(paste0(sp.acron,".weekly"),weekly.object[[sp.position]])
assign(paste0(sp.acron,".daily"),daily.object[[sp.position]])


eval(parse(text=paste0("save(",
                       paste0(sp.acron,'.daily,'),
                       paste0(sp.acron,'.weekly,'),
                       paste0(sp.acron,'.svaga,'),
                       # paste0(sp.acron,'.non.svaga,'),
                       "file='",
                       paste0(wd.history,sp.acron,".RData'"),")")))


