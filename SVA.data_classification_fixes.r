#establishing a unique identification for animals and PPN, even if that information
#is missing in a given submission
DJURINDIVID = (current.SVA.data$DJURINDIVID+2)
PPN = (current.SVA.data$PPN+2)

DJURINDIVID[which(is.na(DJURINDIVID)==TRUE)]<- -1
PPN[which(is.na(PPN)==TRUE)]<- -1

d <- ave(DJURINDIVID,current.SVA.data$UPPDRAG,FUN=max,na.rm=TRUE)
p <- ave(PPN,current.SVA.data$UPPDRAG,FUN=max,na.rm=TRUE)

PPN_original=current.SVA.data$PPN
current.SVA.data=cbind(current.SVA.data,PPN_original)

current.SVA.data$DJURINDIVID[which(is.na(current.SVA.data$DJURINDIVID)==TRUE)]<- 
  abs(d[which(is.na(current.SVA.data$DJURINDIVID)==TRUE)])
current.SVA.data$PPN[which(is.na(current.SVA.data$PPN)==TRUE)]<- 
  abs(p[which(is.na(current.SVA.data$PPN)==TRUE)])

rm(DJURINDIVID, PPN, d, p, PPN_original)


# polish classification ----
# all fixes in the current data that are needed to adjust the syndromes from
#  "raw classified data"



current.SVA.data$SPECIES = as.vector(current.SVA.data$SPECIES)
current.SVA.data$Species2 = as.vector(current.SVA.data$Species2)
current.SVA.data$SPECIES [which(current.SVA.data$SPECIES=="?")] <- "Undefined"
current.SVA.data$SPECIES [which(current.SVA.data$SPECIES=="Unknown")] <- "Undefined"
current.SVA.data$SPECIES [which(current.SVA.data$SPECIES=="NONMAPPED")] <- "Undefined"

current.SVA.data$SPECIES [which(current.SVA.data$SPECIES=="Ruminant")] <- current.SVA.data$Species2[which(current.SVA.data$SPECIES=="Ruminant")]
current.SVA.data$SPECIES [which(current.SVA.data$SPECIES=="NONMAPPED")] <- "Cattle"
current.SVA.data$SPECIES [which(current.SVA.data$SPECIES=="?")] <- "Cattle"
current.SVA.data$SPECIES [which(current.SVA.data$SPECIES=="Canid/Felid")] <- current.SVA.data$Species2[which(current.SVA.data$SPECIES=="Canid/Felid")]
current.SVA.data$SPECIES [which(current.SVA.data$SPECIES=="Wild Canid")] <- "Dog"
current.SVA.data$SPECIES [which(current.SVA.data$SPECIES=="Wild Felid")] <- "Cat"
current.SVA.data$SPECIES [which(current.SVA.data$SPECIES=="Wild Ruminant")] <- "VLT"



current.SVA.data$SYNDROMIC = as.vector(current.SVA.data$SYNDROMIC)
current.SVA.data$SYNDROMIC [which(current.SVA.data$SYNDROMIC=="Eyes-Ears")] <- "EEO"
current.SVA.data$SYNDROMIC [which(current.SVA.data$SYNDROMIC=="Oral Cavity")] <- "EEO"

current.SVA.data$SYNDROMIC [which(current.SVA.data$SYNDROMIC=="Circulatory")] <- "CHH"
current.SVA.data$SYNDROMIC [which(current.SVA.data$SYNDROMIC=="Haematopoietic")] <- "CHH"
current.SVA.data$SYNDROMIC [which(current.SVA.data$SYNDROMIC=="Hepatic")] <- "CHH"
