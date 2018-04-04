sp.label="Cattle"
sp.acron="BOV"
sp.position=which(species.acronyms==sp.acron)



# SVA data: ----

# refining syndromic classification for species specific syndromes ----

CD.species <-current.SVA.data[current.SVA.data$SPECIES==sp.label,] 
am <- CD.species[CD.species$General=="AntimResistance",]
if(dim(am)[1]>0)(am$SYNDROMIC <- "AntimResistance")
dd <- CD.species[CD.species$Doddes==1,]
if(dim(dd)[1]>0)(dd$SYNDROMIC <-  "Doddes")
  
  CD.species <- rbind(CD.species,am,dd)

  CD.species$SYNDROMIC[CD.species$Specific=="BVD"] <- "BVD"
  CD.species$SYNDROMIC[CD.species$SYNDROMIC=="GIT"&
                         CD.species$General=="Macroparasites"] <- "Macropar"
  CD.species$SYNDROMIC[CD.species$SYNDROMIC=="GIT"&
                         CD.species$General=="Microparasites"] <- "Micropar"
  CD.species$SYNDROMIC[CD.species$SYNDROMIC=="GIT"&
                         CD.species$General=="Bacterial"] <- "Bacterial"
  CD.species$SYNDROMIC[CD.species$SYNDROMIC=="GIT"&
                         CD.species$General=="Viral"] <- "Viral"

  
  
# running vetsyn-based-engine ----
  #(running the codes that are the same for every syndromic object, namely:
  #   1. update ths yndromic object;
  #   2. run detection algorithms;
  #   3. report alarms)
  
  source("vetsyn-based-engine.r",local=TRUE,encoding="native.enc")
  
  
  
  