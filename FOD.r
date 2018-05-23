sp.label="FOD"
sp.acron="FOD"
sp.position=which(species.acronyms==sp.acron)



# SVA data: ----

# refining syndromic classification for species specific syndromes ----

CD.species <-current.SVA.data[current.SVA.data$SPECIES==sp.label,] 

CD.species$SYNDROMIC[CD.species$General=="Mycotic"] <- "Mycotic"
CD.species$SYNDROMIC[CD.species$General=="Bacterial"] <- "Bacterial"
CD.species$SYNDROMIC[(!CD.species$General=="Mycotic")&(!CD.species$General=="Bacterial")] <- "Rest"


  

# running vetsyn-based-engine ----
#(running the codes that are the same for every syndromic object, namely:
#   1. update ths yndromic object;
#   2. run detection algorithms;
#   3. report alarms)

if(dim(CD.species)[1]>0){

source("vetsyn-based-engine.r",local=TRUE,encoding="native.enc")}

