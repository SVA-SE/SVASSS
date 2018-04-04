sp.label="FSK"
sp.acron="FSK"
sp.position=which(species.acronyms==sp.acron)



# SVA data: ----

# refining syndromic classification for species specific syndromes ----

CD.species <-current.SVA.data[current.SVA.data$SPECIES==sp.label,] 

CD.species$SYNDROMIC[CD.species$General=="AntimResistance"] <- "AntimResistance"
CD.species$SYNDROMIC[(!CD.species$SYNDROMIC=="GIT")&(!CD.species$SYNDROMIC=="Systemic")
                     &(!CD.species$SYNDROMIC=="Urinary")&(!CD.species$General=="AntimResistance")] <- "Rest"

  

# running vetsyn-based-engine ----
#(running the codes that are the same for every syndromic object, namely:
#   1. update ths yndromic object;
#   2. run detection algorithms;
#   3. report alarms)

source("vetsyn-based-engine.r",local=TRUE,encoding="native.enc")

