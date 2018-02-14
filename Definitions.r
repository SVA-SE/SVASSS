PlotDaily=1


wd.reports <- "I:\\ESS\\SVASyndromicSurveillanceSystem\\regular_reports_PDF"
wd.alarm.reports <- "I:\\ESS\\SVASyndromicSurveillanceSystem\\ALARM_reports_PDF"
wd.files   <- "I:\\ESS\\SVASyndromicSurveillanceSystem\\history_files"
wd.working <- "I:\\ESS\\SVASyndromicSurveillanceSystem"
#wd.html <- "I:\\ESS\\SVASyndromicSurveillanceSystem\\html"
wd.html <- "Z:\\SVASSS"
#wd.html.figures <- "I:\\ESS\\SVASyndromicSurveillanceSystem\\html\\figures"
wd.html.figures <- "Z:\\SVASSS\\figures"


#labelling and naming ----

#abreviations to use in file names
species.acronyms = c("CAT","BOV","DOG","ENV","FOD","FSK","EQU","AVI","SRU",
                     "SWI","VLT")#,"CDB","SJV_notD","SJV_notW","SJV_equD","SJV_equ","SJV_swiD","SJV_swiW","SJV_sruD","SJV_sruW","SJV_aviW","Quality")

#pretty labels to use in reports
species.names=c("Cats","Cattle","Dogs","Environm.","FOD","FSK","Horses","Poultry","Small Ruminants",
                "Swine","VLT")#,"CDB","SJV-Cattle(D)","SJV-Cattle(W)","SJV-Horses(D)","SJV-Horses(W)","SJV-Pigs(D)","SJV-Pigs(W)","SJV-S.Rumin(D)","SJV-S.Rumin(W)","SJV-Poultry","Quality")

#syndrome names as they appear in the result of classification algorithms (SYNDROMIC columns in SVASSS)
syndromes.labels = c("Abortion", "CHH","EEO","Integumentary", "GIT", 
                   "MBS", "Mastitis", "Musculoskeletal","Nervous","Reproductive",
                   "Respiratory","Systemic","Urinary","AntimResistance", "Doddes")
syndromes.names = c("Abortion", "Circ.Haematop.Hepatic","Eyes-Ears-Others","Integumentary", "GIT", 
                          "MBS", "Mastitis", "Musculoskeletal","Nervous","Reproductive",
                          "Respiratory","Systemic","Urinary","AntimResistance", "Fallen Stock")



### syndromes tailored to each species
sp.colnames<- list(
  CAT=c(syndromes.labels,"Macropar","Micropar","Bacterial","Viral","Sarcoptes"),
  BOV=c(syndromes.labels,"Macropar","Micropar","Bacterial","Viral","BVD"),
  DOG=c(syndromes.labels,"Macropar","Micropar","Bacterial","Viral","Sarcoptes"),
  ENV=c("E.coli","Salmonella","Rest"),
  FOD=c("Mycotic", "Bacterial","Rest"),
  FSK=c("GIT", "Systemic","Urinary","AntimResistance", "Rest"),
  EQU=c(syndromes.labels,"Macropar","Micropar","Bacterial","Viral","blodmaskar","Taylorella"),
  AVI=syndromes.labels,
  SRU=syndromes.labels,
  SWI=c(syndromes.labels,"Macropar","Micropar","Bacterial","Viral"),
  VLT=c(syndromes.labels,"Total")
)

sp.syndromes<- list(
  CAT=c(syndromes.names,"GIT-Macroparasites","GIT-Microparasites", "GIT-Bacterial tests","GIT-Viral Tests","Sarcoptes sp"),
  BOV=c(syndromes.names,"GIT-Macroparasites","GIT-Microparasites", "GIT-Bacterial tests","GIT-Viral Tests","BVD"),
  DOG=c(syndromes.names,"GIT-Macroparasites","GIT-Microparasites", "GIT-Bacterial tests","GIT-Viral Tests","Sarcoptes sp"),
  ENV=c("Ecoli","Salmonella","All Others"),
  FOD=c("Mycotic", "Bacterial","All Others"),
  FSK=c("GIT", "Systemic","Urinary","AntimResistance", "All Others"),
  EQU=c(syndromes.names,"GIT-Macroparasites","GIT-Microparasites", "GIT-Bacterial tests","GIT-Viral Tests","Blodmask","Taylorella"),
  AVI=syndromes.names,
  SRU=syndromes.names,
  SWI=c(syndromes.names,"GIT-Macroparasites","GIT-Microparasites", "GIT-Bacterial tests","GIT-Viral Tests"),
  VLT=c(syndromes.names,"Total")
)



# detect algorithms settings ----

shew.limits <- c(2.75,3,3.25,3.75,4.25)
ewma.limits <- c(2.75,3,3.25,3.75,4.25)
csum.limits <- c(3.25,3.5,3.75,4,4.25)
hw.limits   <- c(2.5,2.75,3,3.5,4)


guard.band <- 5
baseline.window <- 520
  #cdb.baseline.window <- 730
  #baseline.window.DF <- 65
  #cdb.baseline.window.DF <- 92
lambda=0.4
nahead <- 5
  cdb.nahead=7
value.if.alarm <-2

baseline.window.week <-104
guard.band.week <- 4

#formula settings ----
sp.all.syndromes <- list(
  CAT=c(1:6,8:20),
  BOV=1:length(sp.colnames$BOV),
  DOG=1:length(sp.colnames$DOG),
  ENV=1:length(sp.colnames$ENV),
  FOD=1:length(sp.colnames$FOD),
  FSK=1:length(sp.colnames$FSK),
  EQU=1:length(sp.colnames$EQU),
  AVI=c(2:6,8:15),
  SRU=1:length(sp.colnames$SRU),
  SWI=c(1:6,8:19),
  VLT=c(1:6,8:16)
)

sp.hw.daily <- list(
  CAT=c(3,4,10:19),
  BOV=c(7,9,11,12,15),
  DOG=c(3:5,11:18,20),
  ENV=NA, ### NO DAILY
  FOD=NA, ### NO DAILY
  FSK=NA, ### NO DAILY
  EQU=c(10:12,14:16,18,20,21),
  AVI=NA, ### NO DAILY FOR AVI!!!!!
  SRU=c(9,15),
  SWI=c(8),
  VLT=c(15,16)
)

sp.hw.weekly <- list(
  CAT=c(3,4,10:19),
  BOV=c(7,9,11,12,15,16,17,18,19),
  DOG=c(3:5,11:18,20),
  ENV=NA,
  FOD=NA,
  FSK=c(5),
  EQU=c(4,5,10:12,14:16,18,20,21),
  AVI=c(2,11,14,15),
  SRU=c(5,9,15),
  SWI=c(8,15,18),
  VLT=c(5,8,11,15,16)
)

sp.daily.formulas <- list(
  CAT=list(NA,NA,y~dow+sin+cos,y~dow+sin+cos,
           NA,NA,NA,NA,NA,
           y~dow+sin+cos,y~dow+sin+cos,y~dow+sin+cos,y~dow+sin+cos,y~dow+sin+cos,
           y~dow+sin+cos,y~dow+sin+cos,y~dow+sin+cos,y~dow+sin+cos,y~dow+sin+cos,
           NA),
  BOV=list(NA,NA,NA,NA,NA,NA, y~dow+sin+cos,
           NA,y~dow+sin+cos,NA,y~dow+sin+cos,y~dow+sin+cos,
           NA,NA,y~dow+sin+cos,NA,NA,NA,NA,NA),
  DOG=list(NA,NA,y~dow+sin+cos,y~dow+sin+cos,y~dow+sin+cos,NA,NA,NA,NA,NA,
           y~dow+sin+cos,y~dow+sin+cos,y~dow+sin+cos,y~dow+sin+cos,y~dow+sin+cos,
           y~dow+sin+cos,y~dow+sin+cos,y~dow+sin+cos,NA,y~dow+sin+cos),
  ENV=NA,
  FOD=NA,
  FSK=NA,
  EQU=list(NA,NA,NA,NA,NA,NA,NA,NA,NA,
           y~dow+sin+cos,y~dow+sin+cos,y~dow+sin+cos,
           NA,y~dow+sin+cos,y~dow+sin+cos,y~dow+sin+cos,
           NA,y~dow+sin+cos,NA,y~dow+sin+cos,y~dow+sin+cos),
  AVI=NA,
  SRU=list(NA,NA,NA,NA,NA,NA,NA,NA,y~dow+sin+cos,NA,
        NA,NA,NA,NA,y~dow+sin+cos),
  SWI=list(NA,NA,NA,NA,NA,NA,NA,y~dow+sin+cos,NA,NA,
        NA,NA,NA,NA,NA,NA,NA,NA,NA),
  VLT=list(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,y~dow+sin+cos,y~dow+sin+cos)
)

sp.pre.process.daily <- list(
  CAT=list(FALSE,FALSE,"glm","glm",FALSE,
           FALSE,FALSE,FALSE,FALSE,"glm",
           "glm","glm","glm","glm","glm",
           "glm","glm","glm","glm",FALSE),
  BOV=list(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,"glm",
           FALSE,"glm",FALSE,"glm","glm",
           FALSE,FALSE,"glm",FALSE,FALSE,FALSE,FALSE,FALSE),
  DOG=list(FALSE,FALSE,"glm","glm","glm",FALSE,FALSE,FALSE,FALSE,FALSE,
           "glm","glm","glm","glm","glm","glm","glm","glm",FALSE,"glm"),
  ENV=NA,
  FOD=NA,
  FSK=NA,
  EQU=list(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,"glm",
           "glm","glm",FALSE,"glm","glm","glm",FALSE,"glm",FALSE,"glm","glm"),
  AVI=NA,
  SRU=list(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,"glm",FALSE,
           FALSE,FALSE,FALSE,FALSE,"glm"),
  SWI=list(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,"glm",FALSE,FALSE,
           FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE),
  VLT=list(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
           FALSE,FALSE,FALSE,FALSE,"glm","glm")
)



# thresholds ----
sp.daily.hw.thresholds <- list(
  CAT=c(6,6,3,3,6,6,6,6,6,3,3,3,3,3,3,3,3,3,3,6),
  BOV=c(6,6,6,6,6,6,3,6,3,6,3,3,6,6,3,6,6,6,6,6),
  DOG=c(6,6,3,3,3,6,6,6,6,6,3,3,3,3,3,3,3,3,6,3),
  ENV=NA,
  FOD=NA,
  FSK=NA,
  EQU=c(6,6,6,6,6,6,6,6,6,3,3,3,6,3,3,3,6,3,6,3,3),
  AVI=NA,
  SRU=c(6,6,6,6,6,6,6,6,3,6,6,6,6,6,3),
  SWI=c(6,6,6,6,6,6,6,3,6,6,6,6,6,6,6,6,6,6,6),
  VLT=c(6,6,6,6,6,6,6,6,6,6,6,6,6,6,3,3)
)

sp.daily.ewma.thresholds <- list(
  CAT=c(6,6,3,3,3,6,6,6,6,3,3,3,5,3,6,3,3,3,3,6),
  BOV=c(6,6,6,6,6,6,3,6,3,6,3,3,6,6,3,6,6,6,6,6),
  DOG=c(6,6,3,3,5,6,6,6,6,6,3,3,6,3,6,3,3,3,6,6),
  ENV=NA,
  FOD=NA,
  FSK=NA,
  EQU=c(6,3,6,4,6,6,6,6,6,3,3,3,6,3,6,6,6,6,6,6,6),
  AVI=NA,
  SRU=c(6,6,6,6,6,6,6,6,3,6,6,6,6,6,3),
  SWI=c(6,6,6,6,6,6,6,3,6,6,6,6,6,6,6,6,6,6,6),
  VLT=c(6,6,6,6,6,6,6,6,6,6,6,6,6,6,3,3)
)

sp.weekly.hw.thresholds <- list(
  CAT=c(6,6,3,3,6,6,6,6,6,3,3,3,3,3,3,3,3,3,3,6),
  BOV=c(6,6,6,6,6,6,3,6,3,6,3,3,6,6,3,3,3,3,3,6),
  DOG=c(6,6,3,3,3,6,6,6,6,6,3,3,3,3,3,3,3,3,6,3),
  ENV=c(6,6,6),
  FOD=c(6,6,6),
  FSK=c(6,6,6,6,3),
  EQU=c(6,6,6,3,3,6,6,6,6,3,3,3,6,3,3,3,6,3,6,3,3),
  AVI=c(6,3,6,6,6,6,6,6,6,6,3,6,6,3,3),
  SRU=c(6,6,6,6,3,6,6,6,3,6,6,6,6,6,3),
  SWI=c(6,6,6,6,6,6,6,3,6,6,6,6,6,6,3,6,6,3,6),
  VLT=c(6,6,6,6,3,6,6,3,6,6,3,6,6,6,3,3)
)

sp.weekly.ewma.thresholds <- list(
  CAT=c(6,6,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3),
  BOV=c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3),
  DOG=c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3),
  ENV=c(3,3,3),
  FOD=c(3,3,3),
  FSK=c(3,3,3,3,3),
  EQU=c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3),
  AVI=c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3),
  SRU=c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3),
  SWI=c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3),
  VLT=c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3)
)


