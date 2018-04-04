sp.label="Environment"
sp.acron="ENV"
sp.position=which(species.acronyms==sp.acron)



# SVA data: ----

# refining syndromic classification for species specific syndromes ----

CD.species <-current.SVA.data[current.SVA.data$SPECIES==sp.label,] 

CD.species$SYNDROMIC[CD.species$Specific=="E.coli"] <- "E.coli"
CD.species$SYNDROMIC[CD.species$Specific=="Salmonella"] <- "Salmonella"
CD.species$SYNDROMIC[(!CD.species$Specific=="Salmonella")&(!CD.species$Specific=="E.coli")] <- "Rest"

# running vetsyn-based-engine ----
#(running the codes that are the same for every syndromic object, namely:
#   1. update ths yndromic object;
#   2. run detection algorithms;
#   3. report alarms)

source("vetsyn-based-engine.r",local=TRUE,encoding="native.enc")

