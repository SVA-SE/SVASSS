sp.label="Avian"
sp.acron="AVI"
sp.position=which(species.acronyms==sp.acron)



# SVA data: ----

# refining syndromic classification for species specific syndromes ----

CD.species <-current.SVA.data[current.SVA.data$SPECIES==sp.label,] 
am <- CD.species[CD.species$General=="AntimResistance",]
if(dim(am)[1]>0)(am$SYNDROMIC <- "AntimResistance")
dd <- CD.species[CD.species$Doddes==1,]
if(dim(dd)[1]>0)(dd$SYNDROMIC <-  "Doddes")
  
  CD.species <- rbind(CD.species,am,dd)

  
  # running vetsyn-based-engine ----
  #(running the codes that are the same for every syndromic object, namely:
  #   1. update ths yndromic object;
  #   2. run detection algorithms;
  #   3. report alarms)
  
  source("vetsyn-based-engine.r",local=TRUE,encoding="native.enc")
  
  
  
  # svaga data ----
  
  source("svaga-data-filtering.r",local=TRUE,encoding="native.enc")
  
  
  
  
  # save data ----
  assign(paste0(sp.acron,".svaga"),svaga.object[[sp.position]])
  assign(paste0(sp.acron,".non.svaga"),non.svaga.object[[sp.position]])
  assign(paste0(sp.acron,".weekly"),weekly.object[[sp.position]])
  assign(paste0(sp.acron,".daily"),daily.object[[sp.position]])
  
  
  eval(parse(text=paste0("save(",
                         paste0(sp.acron,'.daily,'),
                         paste0(sp.acron,'.weekly,'),
                         paste0(sp.acron,'.svaga,'),
                         paste0(sp.acron,'.non.svaga,'),
                         "file='",
                         paste0(wd.history,sp.acron,".RData'"),")")))