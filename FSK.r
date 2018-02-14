sp.label="FSK"
sp.acron="FSK"
sp.position=which(species.acronyms==sp.acron)



# refining syndromic classification for species specific syndromes ----

CD.species <-current.data[current.data$SPECIES==sp.label,] 

CD.species$SYNDROMIC[CD.species$General=="AntimResistance"] <- "AntimResistance"
CD.species$SYNDROMIC[(!CD.species$SYNDROMIC=="GIT")&(!CD.species$SYNDROMIC=="Systemic")
                     &(!CD.species$SYNDROMIC=="Urinary")&(!CD.species$General=="AntimResistance")] <- "Rest"

  
# update syndromic ----

daily.object[[sp.position]] <- update_syndromic(x=daily.object[[sp.position]],
                                                id=list(UPPDRAG,PPN),
                                                syndromes.var=SYNDROMIC,
                                                add.syndromes=FALSE,
                                                dates.var=ANKOMSTDATUM,
                                                date.format="%d/%m/%Y",
                                                remove.dow=c(6,0),
                                                add.to=c(2,1),
                                                replace.dates=TRUE,
                                                data=CD.species)



if(weekly){ 
  weekly.object[[sp.position]] <- update_syndromic(x=weekly.object[[sp.position]],
                                                   id=list(UPPDRAG,PPN),
                                                   syndromes.var=SYNDROMIC,
                                                   add.syndromes=FALSE,
                                                   dates.var=ANKOMSTDATUM,
                                                   date.format="%d/%m/%Y",
                                                   replace.dates=TRUE,
                                                   data=CD.species)
  
}

daily.evaluate.window  <- max(dim(daily.object[[sp.position]]@observed)[1]-last.historical.row.5days,1)

if(weekly){ 
  weekly.evaluate.window <- max(dim(weekly.object[[sp.position]]@observed)[1]-last.historical.row.week,1)
}

# apply detection ----

#hw ----

# daily.object[[sp.position]] <- holt_winters_synd(x=daily.object[[sp.position]],
#                               syndromes=sp.hw.daily[[sp.position]],
#                               evaluate.window=daily.evaluate.window,
#                               frequency=5,
#                               baseline.window=baseline.window,
#                               limit.sd=hw.limits,
#                               nahead=nahead,
#                               alpha=0.4,
#                               beta=0,
#                               gamma=0.15,
#                               seasonal="additive",
#                               correct.baseline=3,
#                               alarm.dim=1,
#                               UCL=3)

if(weekly){
  weekly.object[[sp.position]] <- holt_winters_synd(x=weekly.object[[sp.position]],
                                                    syndromes=sp.hw.weekly[[sp.position]],
                                                    evaluate.window=weekly.evaluate.window,
                                                    frequency=52,
                                                    baseline.window=baseline.window.week,
                                                    limit.sd=hw.limits,
                                                    nahead=guard.band.week,
                                                    alpha=0.4,
                                                    beta=0,
                                                    gamma=0.15,
                                                    seasonal="additive",
                                                    correct.baseline=3,
                                                    alarm.dim=1,
                                                    UCL=3)
}

# ewma ----

#run ewma for all, but only correct baseline on those NOT subjected to HW


ewma.loop.synd.d <- list(sp.hw.daily[[sp.position]],
                         sp.all.syndromes[[sp.position]][!sp.all.syndromes[[sp.position]]%in%sp.hw.daily[[sp.position]]])
ewma.loop.synd.w <- list(sp.hw.weekly[[sp.position]],
                         sp.all.syndromes[[sp.position]][!sp.all.syndromes[[sp.position]]%in%sp.hw.weekly[[sp.position]]])

ewma.loop.correct <- c(FALSE,TRUE)

for (ewma.loop in 1:2){
  # daily.object[[sp.position]] <- ewma_synd(x=daily.object[[sp.position]],
  #             syndromes=ewma.loop.synd.d[[ewma.loop]],
  #             evaluate.window=daily.evaluate.window,
  #             baseline.window=baseline.window,
  #             lambda=lambda,
  #             limit.sd=ewma.limits,
  #             guard.band=guard.band,
  #             correct.baseline=ewma.loop.correct[ewma.loop],
  #             alarm.dim=2,
  #             UCL=3,
  #             LCL=FALSE,
  #             pre.process=sp.pre.process.daily[[sp.position]],
  #             diff.window=NULL,
  #             family="poisson",
  #             formula=sp.daily.formulas[[sp.position]],
  #             frequency=260)
  
  if(weekly){ 
    weekly.object[[sp.position]] <- ewma_synd(x=weekly.object[[sp.position]],
                                              syndromes=ewma.loop.synd.w[[ewma.loop]],
                                              evaluate.window=weekly.evaluate.window,
                                              baseline.window=baseline.window.week,
                                              lambda=lambda,
                                              limit.sd=ewma.limits,
                                              guard.band=guard.band.week,
                                              correct.baseline=ewma.loop.correct[ewma.loop],
                                              alarm.dim=2,
                                              UCL=3,
                                              LCL=FALSE,
                                              pre.process=FALSE,
                                              formula=list(y~x),
                                              frequency=52)
  }
}

#s=8
#BOV.daily@observed[2000:2480,s]
#BOV.daily@baseline[2000:2480,s]
#BOV.daily@UCL[2000:2480,s,2]
#BOV.daily@alarms[2000:2480,s,2]


# shew ----
#run shew for all, and never correct baseline
# daily.object[[sp.position]] <- shew_synd(x=daily.object[[sp.position]],
#                        syndromes=NULL,
#                        evaluate.window=daily.evaluate.window,
#                        baseline.window=baseline.window,
#                        limit.sd=shew.limits,
#                        guard.band=guard.band,
#                        correct.baseline=FALSE,
#                        alarm.dim=3,
#                        UCL=3,
#                        LCL=FALSE,
#                        pre.process=sp.pre.process.daily[[sp.position]],
#                        diff.window=NULL,
#                        family="poisson",
#                        formula=sp.daily.formulas[[sp.position]],
#                        frequency=260)

if(weekly){ 
  weekly.object[[sp.position]] <- shew_synd(x=weekly.object[[sp.position]],
                                            syndromes=NULL,
                                            evaluate.window=weekly.evaluate.window,
                                            baseline.window=baseline.window.week,
                                            limit.sd=shew.limits,
                                            guard.band=guard.band.week,
                                            correct.baseline=FALSE,
                                            alarm.dim=3,
                                            UCL=3,
                                            LCL=FALSE,
                                            pre.process=FALSE,
                                            formula=list(y~x),
                                            frequency=52)
}

# cusum ----
#run cusum for all, and never correct baseline
# daily.object[[sp.position]] <- cusum_synd(x=daily.object[[sp.position]],
#                        syndromes=NULL,
#                        evaluate.window=daily.evaluate.window,
#                        baseline.window=baseline.window,
#                        limit.sd=csum.limits,
#                        guard.band=guard.band,
#                        correct.baseline=FALSE,
#                        alarm.dim=4,
#                        UCL=3,
#                        LCL=FALSE,
#                        pre.process=sp.pre.process.daily[[sp.position]],
#                        diff.window=NULL,
#                        family="poisson",
#                        formula=sp.daily.formulas[[sp.position]],
#                        frequency=260)

if(weekly){ 
  weekly.object[[sp.position]] <- cusum_synd(x=weekly.object[[sp.position]],
                                             syndromes=NULL,
                                             evaluate.window=weekly.evaluate.window,
                                             baseline.window=baseline.window.week,
                                             limit.sd=csum.limits,
                                             guard.band=guard.band.week,
                                             correct.baseline=FALSE,
                                             alarm.dim=4,
                                             UCL=3,
                                             LCL=FALSE,
                                             pre.process=FALSE,
                                             formula=list(y~x),
                                             frequency=52)
}

# save data ----
setwd(wd.files)


assign(paste0(sp.acron,".daily"),daily.object[[sp.position]])
assign(paste0(sp.acron,".weekly"),weekly.object[[sp.position]])


eval(parse(text=paste0("save(",paste0(sp.acron,'.daily,'),
                       paste0(sp.acron,'.weekly,'),"file='",
                       paste0(sp.acron,".RData'"),")")))


## reporting ----


true.alarms.daily[[sp.position]] <- daily.object[[sp.position]]@alarms[dim(daily.object[[sp.position]]@alarms)[1],,1]>=
  sp.daily.hw.thresholds[[sp.position]]
scnd.alarms.daily[[sp.position]] <- daily.object[[sp.position]]@alarms[dim(daily.object[[sp.position]]@alarms)[1],,2]>=
  sp.daily.ewma.thresholds[[sp.position]]

if(weekly){ 
  true.alarms.weekly[[sp.position]] <- weekly.object[[sp.position]]@alarms[dim(weekly.object[[sp.position]]@alarms)[1],,1]>=
    sp.weekly.hw.thresholds[[sp.position]]
  scnd.alarms.weekly[[sp.position]] <- weekly.object[[sp.position]]@alarms[dim(weekly.object[[sp.position]]@alarms)[1],,2]>=
    sp.weekly.ewma.thresholds[[sp.position]]
}


# report alarms ----

setwd(wd.alarm.reports)

if(sum(c(true.alarms.daily[[sp.position]],scnd.alarms.daily[[sp.position]]),na.rm=TRUE)>0){
  pdf(file=paste((new.data.end),sp.label,"ALARM-REPORT", "pdf", sep="."), paper="a4r", width=10)
  
  for (j in unique(c(which(true.alarms.daily[[sp.position]]==TRUE),
                     which(scnd.alarms.daily[[sp.position]]==TRUE)))){
    
    
    plot_syndromic(x=daily.object[[sp.position]],
                   syndromes=j,
                   window=300,
                   baseline=TRUE,
                   UCL=3,
                   algorithms=NULL,
                   limit=sp.daily.hw.thresholds[[sp.position]][j])
  }
  
  
  dev.off()
  graphics.off()
}



if (weekly&
    sum(c(true.alarms.weekly[[sp.position]],scnd.alarms.weekly[[sp.position]]),na.rm=TRUE)>0){
  
  pdf(file=paste((new.data.end),sp.label,"Weekly-ALARM-REPORT", "pdf", sep="."), paper="a4r", width = 10)
  
  for (w in unique(c(which(true.alarms.weekly[[sp.position]]==TRUE),
                     which(scnd.alarms.weekly[[sp.position]]==TRUE)))){
    
    plot_syndromic(x=weekly.object[[sp.position]],
                   syndromes=w,
                   window=60,
                   baseline=TRUE,
                   UCL=3,
                   algorithms=NULL,
                   limit=sp.weekly.hw.thresholds[[sp.position]][w])
    
  }
  dev.off()
  graphics.off()
}


# html page ----

setwd(wd.html)
syndromic_page(x=daily.object[[sp.position]],
               tpoints.display=5,
               syndromes=sp.all.syndromes[[sp.position]],
               pretty.labels=sp.syndromes[[sp.position]][sp.all.syndromes[[sp.position]]],
               window=300,
               baseline=TRUE,
               UCL=1,
               algorithms=c(1,2),
               limit=3,
               file.name=sp.acron,
               title=paste("Daily report of syndromes in",sp.label, (new.data.end),sep=" "),
               data.page=TRUE,
               data=CD.species,
               date.format="%d/%m/%Y",
               dates.var="ANKOMSTDATUM",
               syndromes.var="SYNDROMIC",
               color.null="F8F8FF",
               color.low="F8FF2F",
               color.alarm="FF0000",
               scale=10, 
               fill.colors=c("yellow2","orange","tomato"),
               arrow.colors=c("green","orange","tomato","red"))

if(weekly){ 
  syndromic_page(x=weekly.object[[sp.position]],
                 tpoints.display=5,
                 syndromes=sp.all.syndromes[[sp.position]],
                 pretty.labels=sp.syndromes[[sp.position]][sp.all.syndromes[[sp.position]]],
                 window=60,
                 baseline=TRUE,
                 UCL=1,
                 algorithms=c(1,2),
                 limit=3,
                 file.name=paste0(sp.acron,"w"),
                 title=paste("Weekly report of syndromes in",sp.label, (new.data.end),sep=" "),
                 data.page=FALSE,
                 data=CD.cattle,
                 date.format="%d/%m/%Y",
                 dates.var="ANKOMSTDATUM",
                 syndromes.var="SYNDROMIC",
                 color.null="F8F8FF",
                 color.low="F8FF2F",
                 color.alarm="FF0000",
                 scale=10, 
                 fill.colors=c("yellow2","orange","tomato"),
                 arrow.colors=c("green","orange","tomato","red"))
}
