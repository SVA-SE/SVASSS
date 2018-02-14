load("history_files\\SVASSS.alarms.data.RData")

###########SVALA data and alarms
##SVASSS.alarms.data
  #first.date.current
#end.rows.historical
#endday
#dates.matrix.5days


#SVASSS.alarms.data
#SVASSS.CDB.alarms.data
#SVASSS.SJV.alarms.data




keep.rows <- which(as.Date(SVASSS.alarms.data$ANKOMSTDATUM_original, 
                           format = "%m/%d/%y", origin="01/01/1970")
                   <(min.date.current))
SVASSS.alarms.data <- SVASSS.alarms.data[keep.rows,]
#SVASSS.alarms.data <- SVASSS.alarms.data[1,]




for (date.row in (end.rows.historical+1):endday){
  
  week <- date2ISOweek(as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))
  week.character <- substr(as.character(week),1,8)
  week.row <- which(as.character(dates.matrix.week[,1])==week.character)

  
  
#############NÖT
  for (synd in not.days.syndromes){
   if (NOT.alarm.total[date.row,synd]>=not.scores[synd]){
     
     if (synd<14){
     SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                 current.data2[which(current.data2$SPECIES=="Cattle"&
                                                       current.data2$SYNDROMIC==syndrome.names[synd]&
                                                     (as.Date(current.data2$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                       ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
     }else{
       if (synd==14){
         SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                     not.antres.set[which(as.Date(not.antres.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                            ==as.Date(dates.matrix.5days[date.row,1], format= "%Y-%m-%d")),columns])
         
       }
       if (synd==15){
         SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                     not.doddes.set[which(as.Date(not.doddes.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                            ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                     
       }
       if (synd==16){
         SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                     not.git.set[which(not.git.set$General=="Macroparasites"&
                                                         as.Date(not.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                            ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                     
       }
       if (synd==17){
         SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                     not.git.set[which(not.git.set$General=="Microparasites"&
                                                         as.Date(not.git.set$ANKOMSTDATUM, 
                                                                 format = "%d/%m/%Y", origin="01/01/1970")
                                                         ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                     
       }
       if (synd==18){
         SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                     not.git.set[which(not.git.set$General=="Bacterial"&
                                                         as.Date(not.git.set$ANKOMSTDATUM, 
                                                                 format = "%d/%m/%Y", origin="01/01/1970")
                                                         ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                     
       }
       if (synd==19){
         SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                     not.git.set[which(not.git.set$General=="Viral"&
                                                         as.Date(not.git.set$ANKOMSTDATUM, 
                                                                 format = "%d/%m/%Y", origin="01/01/1970")
                                                         ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                     
       }
       if (synd==20){
         SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                     not.bvd.set[which(as.Date(not.bvd.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                            ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                     
       }
       
     }#else
   } #if > score
  }  #for (synd in not.days.syndromes){
  
  if (length(week.row)>0){
  for (synd in not.week.syndromes){
    NOT.week.alarm.total <- NOT.week.alarms.ewma[,,6]+NOT.week.alarms.shew[,,6]
    
    if (NOT.week.alarm.total[week.row,synd]>=not.scores[synd]){
      
      if (synd<14){
      SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                  current.data2[which(current.data2$SPECIES=="Cattle"&
                                                        current.data2$SYNDROMIC==syndrome.names[synd]&
                                                        (as.Date(current.data2$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                         ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
      }else{
        if (synd==14){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      not.antres.set[which(as.Date(not.antres.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==15){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      not.doddes.set[which(as.Date(not.doddes.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==16){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      not.git.set[which(not.git.set$General=="Macroparasites"&
                                                          as.Date(not.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==17){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      not.git.set[which(not.git.set$General=="Microparasites"&
                                                          as.Date(not.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==18){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      not.git.set[which(not.git.set$General=="Bacterial"&
                                                          as.Date(not.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==19){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      not.git.set[which(not.git.set$General=="Viral"&
                                                          as.Date(not.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==20){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      not.bvd.set[which(as.Date(not.bvd.set$ANKOMSTDATUM, 
                                                                format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        
      }#else
    } #if (NOT.week.alarm.total[week.row,synd]>=not.scores[synd])
  }  #for (synd in not.week.syndromes){
  }#if (length(week.row)>0){
  
  for (synd in not.cbe.syndromes){
    if (length(which(NOT.events.dates[[synd]]==
                as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")
    ))>0){
    if (max(NOT.alarms.cbe.total[[synd]][which(NOT.events.dates[[synd]]==
                                             as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")
                                           )]) >= not.scores[synd]){
      
      if (synd<14){
      SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                  current.data2[which(current.data2$SPECIES=="Cattle"&
                                                        current.data2$SYNDROMIC==syndrome.names[synd]&
                                                        (as.Date(current.data2$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                         ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
      }else{
        if (synd==14){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      not.antres.set[which(as.Date(not.antres.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==15){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      not.doddes.set[which(as.Date(not.doddes.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==16){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      not.git.set[which(not.git.set$General=="Macroparasites"&
                                                          as.Date(not.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==17){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      not.git.set[which(not.git.set$General=="Microparasites"&
                                                          as.Date(not.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==18){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      not.git.set[which(not.git.set$General=="Bacterial"&
                                                          as.Date(not.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==19){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      not.git.set[which(not.git.set$General=="Viral"&
                                                          as.Date(not.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==20){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      not.bvd.set[which(as.Date(not.bvd.set$ANKOMSTDATUM, 
                                                                format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        
      }#else
      
      } # if alarms
  } #if events on that date 
  } #for all syndromes
  
  
  
  
  





#############EQU
for (synd in equ.days.syndromes){
  if (EQU.alarm.total[date.row,synd]>=equ.scores[synd]){
    
    if (synd<14){
      SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                  current.data3[which(current.data3$SPECIES=="Equidae"&
                                                        current.data3$SYNDROMIC==syndrome.names[synd]&
                                                        (as.Date(current.data3$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                         ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
    }else{
      if (synd==14){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    equ.antres.set[which(as.Date(equ.antres.set$ANKOMSTDATUM, 
                                                                 format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==15){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    equ.doddes.set[which(as.Date(equ.doddes.set$ANKOMSTDATUM, 
                                                                 format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==16){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    equ.git.set[which(equ.git.set$General=="Macroparasites"&
                                                        as.Date(equ.git.set$ANKOMSTDATUM, 
                                                                format = "%d/%m/%Y", origin="01/01/1970")
                                                        ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==17){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    equ.git.set[which(equ.git.set$General=="Microparasites"&
                                                        as.Date(equ.git.set$ANKOMSTDATUM, 
                                                                format = "%d/%m/%Y", origin="01/01/1970")
                                                        ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==18){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    equ.git.set[which(equ.git.set$General=="Bacterial"&
                                                        as.Date(equ.git.set$ANKOMSTDATUM, 
                                                                format = "%d/%m/%Y", origin="01/01/1970")
                                                        ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==19){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    equ.git.set[which(equ.git.set$General=="Viral"&
                                                        as.Date(equ.git.set$ANKOMSTDATUM, 
                                                                format = "%d/%m/%Y", origin="01/01/1970")
                                                        ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==20){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    equ.blodm.set[which(as.Date(equ.blodm.set$ANKOMSTDATUM, 
                                                              format = "%d/%m/%Y", origin="01/01/1970")
                                                        ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==21){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    equ.tayl.set[which(as.Date(equ.tayl.set$ANKOMSTDATUM, 
                                                                format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      
    }#else
  } #if > score
}  #for (synd in equ.days.syndromes){

if (length(week.row)>0){
  for (synd in equ.week.syndromes){
    EQU.week.alarm.total <- EQU.week.alarms.ewma[,,6]+EQU.week.alarms.shew[,,6]
    
    if (EQU.week.alarm.total[week.row,synd]>=equ.scores[synd]){
      
      if (synd<14){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                    current.data3[which(current.data3$SPECIES=="Equidae"&
                                                          current.data3$SYNDROMIC==syndrome.names[synd]&
                                                          (as.Date(current.data3$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
      }else{
        if (synd==14){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      equ.antres.set[which(as.Date(equ.antres.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==15){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      equ.doddes.set[which(as.Date(equ.doddes.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==16){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      equ.git.set[which(equ.git.set$General=="Macroparasites"&
                                                          as.Date(equ.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==17){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      equ.git.set[which(equ.git.set$General=="Microparasites"&
                                                          as.Date(equ.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==18){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      equ.git.set[which(equ.git.set$General=="Bacterial"&
                                                          as.Date(equ.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==19){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      equ.git.set[which(equ.git.set$General=="Viral"&
                                                          as.Date(equ.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==20){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      equ.blodm.set[which(as.Date(equ.blodm.set$ANKOMSTDATUM, 
                                                                format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==21){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      equ.tayl.set[which(as.Date(equ.tayl.set$ANKOMSTDATUM, 
                                                                 format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        
      }#else
    } #if (EQU.week.alarm.total[week.row,synd]>=equ.scores[synd])
  }  #for (synd in equ.week.syndromes){
}#if (length(week.row)>0){

for (synd in equ.cbe.syndromes){
  if (length(which(EQU.events.dates[[synd]]==
                     as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")
  ))>0){
    if (max(EQU.alarms.cbe.total[[synd]][which(EQU.events.dates[[synd]]==
                                                 as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")
    )]) >= equ.scores[synd]){
      
      if (synd<14){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                    current.data3[which(current.data3$SPECIES=="Equidae"&
                                                          current.data3$SYNDROMIC==syndrome.names[synd]&
                                                          (as.Date(current.data3$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
      }else{
        if (synd==14){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      equ.antres.set[which(as.Date(equ.antres.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==15){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      equ.doddes.set[which(as.Date(equ.doddes.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==16){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      equ.git.set[which(equ.git.set$General=="Macroparasites"&
                                                          as.Date(equ.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==17){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      equ.git.set[which(equ.git.set$General=="Microparasites"&
                                                          as.Date(equ.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==18){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      equ.git.set[which(equ.git.set$General=="Bacterial"&
                                                          as.Date(equ.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==19){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      equ.git.set[which(equ.git.set$General=="Viral"&
                                                          as.Date(equ.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==20){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      equ.blodm.set[which(as.Date(equ.blodm.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                            ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==21){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      equ.tayl.set[which(as.Date(equ.tayl.set$ANKOMSTDATUM, 
                                                                 format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        
      }#else
      
    } # if alarms
  } #if events on that date 
} #for all syndromes





  
  



#############SWI
for (synd in swi.days.syndromes){
  if (SWI.alarm.total[date.row,synd]>=swi.scores[synd]){
    
    if (synd<14){
      SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                  current.data4[which(current.data4$SPECIES=="Swine"&
                                                        current.data4$SYNDROMIC==syndrome.names[synd]&
                                                        (as.Date(current.data4$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                         ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
    }else{
      if (synd==14){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    swi.antres.set[which(as.Date(swi.antres.set$ANKOMSTDATUM, 
                                                                 format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==15){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    swi.doddes.set[which(as.Date(swi.doddes.set$ANKOMSTDATUM, 
                                                                 format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==16){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    swi.git.set[which(swi.git.set$General=="Macroparasites"&
                                                        as.Date(swi.git.set$ANKOMSTDATUM, 
                                                                format = "%d/%m/%Y", origin="01/01/1970")
                                                        ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==17){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    swi.git.set[which(swi.git.set$General=="Microparasites"&
                                                        as.Date(swi.git.set$ANKOMSTDATUM, 
                                                                format = "%d/%m/%Y", origin="01/01/1970")
                                                        ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==18){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    swi.git.set[which(swi.git.set$General=="Bacterial"&
                                                        as.Date(swi.git.set$ANKOMSTDATUM, 
                                                                format = "%d/%m/%Y", origin="01/01/1970")
                                                        ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==19){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    swi.git.set[which(swi.git.set$General=="Viral"&
                                                        as.Date(swi.git.set$ANKOMSTDATUM, 
                                                                format = "%d/%m/%Y", origin="01/01/1970")
                                                        ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      
      
    }#else
  } #if > score
}  #for (synd in swi.days.syndromes){

if (length(week.row)>0){
  for (synd in swi.week.syndromes){
    SWI.week.alarm.total <- SWI.week.alarms.ewma[,,6]+SWI.week.alarms.shew[,,6]
    
    if (SWI.week.alarm.total[week.row,synd]>=swi.scores[synd]){
      
      if (synd<14){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                    current.data4[which(current.data4$SPECIES=="Swine"&
                                                          current.data4$SYNDROMIC==syndrome.names[synd]&
                                                          (as.Date(current.data4$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
      }else{
        if (synd==14){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      swi.antres.set[which(as.Date(swi.antres.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==15){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      swi.doddes.set[which(as.Date(swi.doddes.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==16){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      swi.git.set[which(swi.git.set$General=="Macroparasites"&
                                                          as.Date(swi.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==17){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      swi.git.set[which(swi.git.set$General=="Microparasites"&
                                                          as.Date(swi.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==18){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      swi.git.set[which(swi.git.set$General=="Bacterial"&
                                                          as.Date(swi.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==19){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      swi.git.set[which(swi.git.set$General=="Viral"&
                                                          as.Date(swi.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
 
        
      }#else
    } #if (SWI.week.alarm.total[week.row,synd]>=swi.scores[synd])
  }  #for (synd in swi.week.syndromes){
}#if (length(week.row)>0){

for (synd in swi.cbe.syndromes){
  if (length(which(SWI.events.dates[[synd]]==
                     as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")
  ))>0){
    if (max(SWI.alarms.cbe.total[[synd]][which(SWI.events.dates[[synd]]==
                                                 as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")
    )]) >= swi.scores[synd]){
      
      if (synd<14){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                    current.data4[which(current.data4$SPECIES=="Swine"&
                                                          current.data4$SYNDROMIC==syndrome.names[synd]&
                                                          (as.Date(current.data4$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
      }else{
        if (synd==14){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      swi.antres.set[which(as.Date(swi.antres.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==15){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      swi.doddes.set[which(as.Date(swi.doddes.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==16){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      swi.git.set[which(swi.git.set$General=="Macroparasites"&
                                                          as.Date(swi.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==17){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      swi.git.set[which(swi.git.set$General=="Microparasites"&
                                                          as.Date(swi.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==18){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      swi.git.set[which(swi.git.set$General=="Bacterial"&
                                                          as.Date(swi.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==19){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      swi.git.set[which(swi.git.set$General=="Viral"&
                                                          as.Date(swi.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }

        
      }#else
      
    } # if alarms
  } #if events on that date 
} #for all syndromes











#############AVI
for (synd in avi.days.syndromes){
  if (AVI.alarm.total[date.row,synd]>=avi.scores[synd]){
    
    if (synd<14){
      SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                  current.data[which(current.data$SPECIES=="Avian"&
                                                        current.data$SYNDROMIC==syndrome.names[synd]&
                                                        (as.Date(current.data$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                         ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
    }else{
      if (synd==14){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    avi.antres.set[which(as.Date(avi.antres.set$ANKOMSTDATUM, 
                                                                 format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==15){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    avi.doddes.set[which(as.Date(avi.doddes.set$ANKOMSTDATUM, 
                                                                 format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }    
      
    }#else
  } #if > score
}  #for (synd in avi.days.syndromes){

if (length(week.row)>0){
  for (synd in avi.week.syndromes){
    AVI.week.alarm.total <- AVI.week.alarms.ewma[,,6]+AVI.week.alarms.shew[,,6]
    
    if (AVI.week.alarm.total[week.row,synd]>=avi.scores[synd]){
      
      if (synd<14){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                    current.data[which(current.data$SPECIES=="Avian"&
                                                          current.data$SYNDROMIC==syndrome.names[synd]&
                                                          (as.Date(current.data$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
      }else{
        if (synd==14){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      avi.antres.set[which(as.Date(avi.antres.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==15){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      avi.doddes.set[which(as.Date(avi.doddes.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }        
        
      }#else
    } #if (AVI.week.alarm.total[week.row,synd]>=avi.scores[synd])
  }  #for (synd in avi.week.syndromes){
}#if (length(week.row)>0){

for (synd in avi.cbe.syndromes){
  if (length(which(AVI.events.dates[[synd]]==
                     as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")
  ))>0){
    if (max(AVI.alarms.cbe.total[[synd]][which(AVI.events.dates[[synd]]==
                                                 as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")
    )]) >= avi.scores[synd]){
      
      if (synd<14){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                    current.data[which(current.data$SPECIES=="Avian"&
                                                          current.data$SYNDROMIC==syndrome.names[synd]&
                                                          (as.Date(current.data$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
      }else{
        if (synd==14){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      avi.antres.set[which(as.Date(avi.antres.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==15){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      avi.doddes.set[which(as.Date(avi.doddes.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }

      }#else
      
    } # if alarms
  } #if events on that date 
} #for all syndromes





  





#############DOG
for (synd in dog.days.syndromes){
  if (DOG.alarm.total[date.row,synd]>=dog.scores[synd]){
    
    if (synd<14){
      SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                  current.data5[which(current.data5$SPECIES=="Dog"&
                                                        current.data5$SYNDROMIC==syndrome.names[synd]&
                                                        (as.Date(current.data5$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                         ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
    }else{
      if (synd==14){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    dog.antres.set[which(as.Date(dog.antres.set$ANKOMSTDATUM, 
                                                                 format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==15){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    dog.doddes.set[which(as.Date(dog.doddes.set$ANKOMSTDATUM, 
                                                                 format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==16){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    dog.git.set[which(dog.git.set$General=="Macroparasites"&
                                                        as.Date(dog.git.set$ANKOMSTDATUM, 
                                                                format = "%d/%m/%Y", origin="01/01/1970")
                                                        ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==17){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    dog.git.set[which(dog.git.set$General=="Microparasites"&
                                                        as.Date(dog.git.set$ANKOMSTDATUM, 
                                                                format = "%d/%m/%Y", origin="01/01/1970")
                                                        ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==18){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    dog.git.set[which(dog.git.set$General=="Bacterial"&
                                                        as.Date(dog.git.set$ANKOMSTDATUM, 
                                                                format = "%d/%m/%Y", origin="01/01/1970")
                                                        ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==19){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    dog.git.set[which(dog.git.set$General=="Viral"&
                                                        as.Date(dog.git.set$ANKOMSTDATUM, 
                                                                format = "%d/%m/%Y", origin="01/01/1970")
                                                        ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==20){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    dog.sarc.set[which(as.Date(dog.sarc.set$ANKOMSTDATUM, 
                                                              format = "%d/%m/%Y", origin="01/01/1970")
                                                        ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      
    }#else
  } #if > score
}  #for (synd in dog.days.syndromes){

if (length(week.row)>0){
  for (synd in dog.week.syndromes){
    DOG.week.alarm.total <- DOG.week.alarms.ewma[,,6]+DOG.week.alarms.shew[,,6]
    
    if (DOG.week.alarm.total[week.row,synd]>=dog.scores[synd]){
      
      if (synd<14){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                    current.data5[which(current.data5$SPECIES=="Dog"&
                                                          current.data5$SYNDROMIC==syndrome.names[synd]&
                                                          (as.Date(current.data5$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
      }else{
        if (synd==14){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      dog.antres.set[which(as.Date(dog.antres.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==15){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      dog.doddes.set[which(as.Date(dog.doddes.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==16){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      dog.git.set[which(dog.git.set$General=="Macroparasites"&
                                                          as.Date(dog.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==17){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      dog.git.set[which(dog.git.set$General=="Microparasites"&
                                                          as.Date(dog.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==18){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      dog.git.set[which(dog.git.set$General=="Bacterial"&
                                                          as.Date(dog.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==19){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      dog.git.set[which(dog.git.set$General=="Viral"&
                                                          as.Date(dog.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==20){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      dog.sarc.set[which(as.Date(dog.sarc.set$ANKOMSTDATUM, 
                                                                format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        
      }#else
    } #if (DOG.week.alarm.total[week.row,synd]>=dog.scores[synd])
  }  #for (synd in dog.week.syndromes){
}#if (length(week.row)>0){

for (synd in dog.cbe.syndromes){
  if (length(which(DOG.events.dates[[synd]]==
                     as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")
  ))>0){
    if (max(DOG.alarms.cbe.total[[synd]][which(DOG.events.dates[[synd]]==
                                                 as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")
    )]) >= dog.scores[synd]){
      
      if (synd<14){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                    current.data5[which(current.data5$SPECIES=="Dog"&
                                                          current.data5$SYNDROMIC==syndrome.names[synd]&
                                                          (as.Date(current.data5$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
      }else{
        if (synd==14){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      dog.antres.set[which(as.Date(dog.antres.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==15){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      dog.doddes.set[which(as.Date(dog.doddes.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==16){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      dog.git.set[which(dog.git.set$General=="Macroparasites"&
                                                          as.Date(dog.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==17){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      dog.git.set[which(dog.git.set$General=="Microparasites"&
                                                          as.Date(dog.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==18){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      dog.git.set[which(dog.git.set$General=="Bacterial"&
                                                          as.Date(dog.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==19){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      dog.git.set[which(dog.git.set$General=="Viral"&
                                                          as.Date(dog.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==20){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      dog.sarc.set[which(as.Date(dog.sarc.set$ANKOMSTDATUM, 
                                                                format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        
      }#else
      
    } # if alarms
  } #if events on that date 
} #for all syndromes










#############CAT
for (synd in cat.days.syndromes){
  if (CAT.alarm.total[date.row,synd]>=cat.scores[synd]){
    
    if (synd<14){
      SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                  current.data6[which(current.data6$SPECIES=="Cat"&
                                                        current.data6$SYNDROMIC==syndrome.names[synd]&
                                                        (as.Date(current.data6$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                         ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
    }else{
      if (synd==14){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    cat.antres.set[which(as.Date(cat.antres.set$ANKOMSTDATUM, 
                                                                 format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==15){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    cat.doddes.set[which(as.Date(cat.doddes.set$ANKOMSTDATUM, 
                                                                 format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==16){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    cat.git.set[which(cat.git.set$General=="Macroparasites"&
                                                        as.Date(cat.git.set$ANKOMSTDATUM, 
                                                                format = "%d/%m/%Y", origin="01/01/1970")
                                                        ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==17){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    cat.git.set[which(cat.git.set$General=="Microparasites"&
                                                        as.Date(cat.git.set$ANKOMSTDATUM, 
                                                                format = "%d/%m/%Y", origin="01/01/1970")
                                                        ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==18){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    cat.git.set[which(cat.git.set$General=="Bacterial"&
                                                        as.Date(cat.git.set$ANKOMSTDATUM, 
                                                                format = "%d/%m/%Y", origin="01/01/1970")
                                                        ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==19){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    cat.git.set[which(cat.git.set$General=="Viral"&
                                                        as.Date(cat.git.set$ANKOMSTDATUM, 
                                                                format = "%d/%m/%Y", origin="01/01/1970")
                                                        ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==20){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    cat.sarc.set[which(as.Date(cat.sarc.set$ANKOMSTDATUM, 
                                                               format = "%d/%m/%Y", origin="01/01/1970")
                                                         ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      
    }#else
  } #if > score
}  #for (synd in cat.days.syndromes){

if (length(week.row)>0){
  for (synd in cat.week.syndromes){
    CAT.week.alarm.total <- CAT.week.alarms.ewma[,,6]+CAT.week.alarms.shew[,,6]
    
    if (CAT.week.alarm.total[week.row,synd]>=cat.scores[synd]){
      
      if (synd<14){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                    current.data6[which(current.data6$SPECIES=="Cat"&
                                                          current.data6$SYNDROMIC==syndrome.names[synd]&
                                                          (as.Date(current.data6$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
      }else{
        if (synd==14){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      cat.antres.set[which(as.Date(cat.antres.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==15){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      cat.doddes.set[which(as.Date(cat.doddes.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==16){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      cat.git.set[which(cat.git.set$General=="Macroparasites"&
                                                          as.Date(cat.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==17){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      cat.git.set[which(cat.git.set$General=="Microparasites"&
                                                          as.Date(cat.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==18){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      cat.git.set[which(cat.git.set$General=="Bacterial"&
                                                          as.Date(cat.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==19){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      cat.git.set[which(cat.git.set$General=="Viral"&
                                                          as.Date(cat.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==20){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      cat.sarc.set[which(as.Date(cat.sarc.set$ANKOMSTDATUM, 
                                                                 format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        
      }#else
    } #if (CAT.week.alarm.total[week.row,synd]>=cat.scores[synd])
  }  #for (synd in cat.week.syndromes){
}#if (length(week.row)>0){

for (synd in cat.cbe.syndromes){
  if (length(which(CAT.events.dates[[synd]]==
                     as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")
  ))>0){
    if (max(CAT.alarms.cbe.total[[synd]][which(CAT.events.dates[[synd]]==
                                                 as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")
    )]) >= cat.scores[synd]){
      
      if (synd<14){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                    current.data6[which(current.data6$SPECIES=="Cat"&
                                                          current.data6$SYNDROMIC==syndrome.names[synd]&
                                                          (as.Date(current.data6$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
      }else{
        if (synd==14){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      cat.antres.set[which(as.Date(cat.antres.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==15){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      cat.doddes.set[which(as.Date(cat.doddes.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==16){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      cat.git.set[which(cat.git.set$General=="Macroparasites"&
                                                          as.Date(cat.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==17){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      cat.git.set[which(cat.git.set$General=="Microparasites"&
                                                          as.Date(cat.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==18){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      cat.git.set[which(cat.git.set$General=="Bacterial"&
                                                          as.Date(cat.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==19){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      cat.git.set[which(cat.git.set$General=="Viral"&
                                                          as.Date(cat.git.set$ANKOMSTDATUM, 
                                                                  format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==20){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      cat.sarc.set[which(as.Date(cat.sarc.set$ANKOMSTDATUM, 
                                                                 format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        
      }#else
      
    } # if alarms
  } #if events on that date 
} #for all syndromes








#############FSK
for (synd in fsk.days.syndromes){
  if (FSK.alarm.total[date.row,synd]>=fsk.scores[synd]){
    
    if (synd==3){
      SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                  current.data[which(current.data$SPECIES=="FSK"&
                                                       current.data$SYNDROMIC==syndrome.names[syndromes.fish[synd]]&
                                                       (as.Date(current.data$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                        ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
      
    }
    
    if (synd==4){
      SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                  fsk.antres.set[which(as.Date(fsk.antres.set$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                        ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
      
    }    
    
if (synd==5){
  SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                              fsk.rest.set[which(as.Date(fsk.rest.set$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])

}    

  } #if > score
}  #for (synd in fsk.days.syndromes){



for (synd in fsk.cbe.syndromes){
  if (length(which(FSK.events.dates[[synd]]==
                     as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")
  ))>0){
    if (max(FSK.alarms.cbe.total[[synd]][which(FSK.events.dates[[synd]]==
                                                 as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")
    )]) >= fsk.scores[synd]){
      
      
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                    current.data[which(current.data$SPECIES=="FSK"&
                                                         current.data$SYNDROMIC==syndrome.names[syndromes.fish[synd]]&
                                                         (as.Date(current.data$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
      
    } # if alarms
  } #if events on that date 
} #for all syndromes












#############VLT
for (synd in vlt.days.syndromes){
  if (VLT.alarm.total[date.row,synd]>=vlt.scores[synd]){
    
    if (synd<14){
      SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                  current.data[which(current.data$SPECIES=="VLT"&
                                                        current.data$SYNDROMIC==syndrome.names[synd]&
                                                        (as.Date(current.data$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                         ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
    }else{
      if (synd==14){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    vlt.antres.set[which(as.Date(vlt.antres.set$ANKOMSTDATUM, 
                                                                 format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==15){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    vlt.doddes.set[which(as.Date(vlt.doddes.set$ANKOMSTDATUM, 
                                                                 format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==16){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    vlt.total.set[which(as.Date(vlt.total.set$ANKOMSTDATUM, 
                                                              format = "%d/%m/%Y", origin="01/01/1970")
                                                        ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      
    }#else
  } #if > score
}  #for (synd in vlt.days.syndromes){

if (length(week.row)>0){
  for (synd in vlt.week.syndromes){
    VLT.week.alarm.total <- VLT.week.alarms.ewma[,,6]+VLT.week.alarms.shew[,,6]
    
    if (VLT.week.alarm.total[week.row,synd]>=vlt.scores[synd]){
      
      if (synd<14){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                    current.data[which(current.data$SPECIES=="VLT"&
                                                          current.data$SYNDROMIC==syndrome.names[synd]&
                                                          (as.Date(current.data$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
      }else{
        if (synd==14){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      vlt.antres.set[which(as.Date(vlt.antres.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==15){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      vlt.doddes.set[which(as.Date(vlt.doddes.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==16){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      vlt.total.set[which(as.Date(vlt.total.set$ANKOMSTDATUM, 
                                                                format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        
      }#else
    } #if (VLT.week.alarm.total[week.row,synd]>=vlt.scores[synd])
  }  #for (synd in vlt.week.syndromes){
}#if (length(week.row)>0){

for (synd in vlt.cbe.syndromes){
  if (length(which(VLT.events.dates[[synd]]==
                     as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")
  ))>0){
    if (max(VLT.alarms.cbe.total[[synd]][which(VLT.events.dates[[synd]]==
                                                 as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")
    )]) >= vlt.scores[synd]){
      
      if (synd<14){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                    current.data[which(current.data$SPECIES=="VLT"&
                                                          current.data$SYNDROMIC==syndrome.names[synd]&
                                                          (as.Date(current.data$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
      }else{
        if (synd==14){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      vlt.antres.set[which(as.Date(vlt.antres.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==15){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      vlt.doddes.set[which(as.Date(vlt.doddes.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==16){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      vlt.total.set[which(as.Date(vlt.total.set$ANKOMSTDATUM, 
                                                                format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        
      }#else
      
    } # if alarms
  } #if events on that date 
} #for all syndromes









#############ENV


if (length(week.row)>0){

  
    ENV.week.alarm.total <- ENV.week.alarms.ewma[,,6]+ENV.week.alarms.shew[,,6]
    
    if (ENV.week.alarm.total[week.row,3]>=env.scores){
      
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      env.rest.set[which(as.Date(env.rest.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
    } #if (ENV.week.alarm.total[week.row,synd]>=env.scores[synd])
}#if (length(week.row)>0){





#############FOD
if (length(week.row)>0){
  
  FOD.week.alarm.total <- FOD.week.alarms.ewma[,,6]+FOD.week.alarms.shew[,,6]
  
  for (synd in fod.week.syndromes){
    
  if (FOD.week.alarm.total[week.row,synd]>=fod.scores[synd]){
    
    if (synd==2){
    SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                fod.bact.set[which(as.Date(fod.bact.set$ANKOMSTDATUM, 
                                                           format = "%d/%m/%Y", origin="01/01/1970")
                                                     ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
    }
    
    if (synd==3){
      SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                  fod.rest.set[which(as.Date(fod.rest.set$ANKOMSTDATUM, 
                                                             format = "%d/%m/%Y", origin="01/01/1970")
                                                       ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
    }
                                
  } #if (FOD.week.alarm.total[week.row,synd]>=fod.scores[synd])
  }
}#if (length(week.row)>0){








#############SRU
for (synd in sru.days.syndromes){
  if (SRU.alarm.total[date.row,synd]>=sru.scores[synd]){
    
    if (synd<14){
      SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                  current.data[which(current.data$SPECIES=="Small Ruminant"&
                                                       current.data$SYNDROMIC==syndrome.names[synd]&
                                                       (as.Date(current.data$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                        ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
    }else{
      if (synd==14){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    sru.antres.set[which(as.Date(sru.antres.set$ANKOMSTDATUM, 
                                                                 format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }
      if (synd==15){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                    sru.doddes.set[which(as.Date(sru.doddes.set$ANKOMSTDATUM, 
                                                                 format = "%d/%m/%Y", origin="01/01/1970")
                                                           ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                    
      }    
      
    }#else
  } #if > score
}  #for (synd in sru.days.syndromes){

if (length(week.row)>0){
  for (synd in sru.week.syndromes){
    SRU.week.alarm.total <- SRU.week.alarms.ewma[,,6]+SRU.week.alarms.shew[,,6]
    
    if (SRU.week.alarm.total[week.row,synd]>=sru.scores[synd]){
      
      if (synd<14){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                    current.data[which(current.data$SPECIES=="Small Ruminant"&
                                                         current.data$SYNDROMIC==syndrome.names[synd]&
                                                         (as.Date(current.data$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
      }else{
        if (synd==14){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      sru.antres.set[which(as.Date(sru.antres.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==15){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      sru.doddes.set[which(as.Date(sru.doddes.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }        
        
      }#else
    } #if (SRU.week.alarm.total[week.row,synd]>=sru.scores[synd])
  }  #for (synd in sru.week.syndromes){
}#if (length(week.row)>0){

for (synd in sru.cbe.syndromes){
  if (length(which(SRU.events.dates[[synd]]==
                     as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")
  ))>0){
    if (max(SRU.alarms.cbe.total[[synd]][which(SRU.events.dates[[synd]]==
                                                 as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")
    )]) >= sru.scores[synd]){
      
      if (synd<14){
        SVASSS.alarms.data <- rbind(SVASSS.alarms.data, 
                                    current.data[which(current.data$SPECIES=="Small Ruminant"&
                                                         current.data$SYNDROMIC==syndrome.names[synd]&
                                                         (as.Date(current.data$ANKOMSTDATUM, format = "%d/%m/%Y", origin="01/01/1970")
                                                          ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),columns])
      }else{
        if (synd==14){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      sru.antres.set[which(as.Date(sru.antres.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        if (synd==15){
          SVASSS.alarms.data <- rbind(SVASSS.alarms.data,
                                      sru.doddes.set[which(as.Date(sru.doddes.set$ANKOMSTDATUM, 
                                                                   format = "%d/%m/%Y", origin="01/01/1970")
                                                             ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d")),columns])
                                      
        }
        
      }#else
      
    } # if alarms
  } #if events on that date 
} #for all syndromes







} #for all days being evaluated





###########CDB data

##SVASSS.CDB.alarms.data <- cbind(current.CDB.data,SYNDROMIC=NA)
##SVASSS.CDB.alarms.data <- SVASSS.CDB.alarms.data[-(1:dim(SVASSS.CDB.alarms.data)[1]),]

keep.rows2 <- which(as.Date(SVASSS.CDB.alarms.data$händelsedatum, 
                            format = "%m/%d/%y", origin="01/01/1970")
                    <(min.date.current))

SVASSS.CDB.alarms.data <- SVASSS.CDB.alarms.data[keep.rows2,]
#SVASSS.CDB.alarms.data <- cbind(SVASSS.CDB.alarms.data,SYNDROMIC=vector())

for (date.row in (end.rows.historical.7days+1):endday.cdb){
  
for (synd in cdb.days.syndromes){
  if (CDB.alarm.total[date.row,synd]>=cdb.scores[synd]){
    
      if (synd==1){
        synd1 <- rbind(SVASSS.CDB.alarms.data,
                       current.CDB.data[which(current.CDB.data$KOD=="0"&
                                                (as.Date(current.CDB.data$händelsedatum, format = "%m/%d/%y", origin="01/01/1970")
                                                 ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),])
        if (dim(synd1)[1]>0){
          SVASSS.CDB.alarms.data <- cbind(synd1,
                                          SYNDROMIC="birth")
        }
      }
      
      if (synd==2){
        synd2 <-rbind(SVASSS.CDB.alarms.data,
                      current.CDB.data[which(current.CDB.data$KOD=="10"&
                                               (as.Date(current.CDB.data$händelsedatum, format = "%m/%d/%y", origin="01/01/1970")
                                                ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),]) 
        if (dim(synd2)[1]>0){
          SVASSS.CDB.alarms.data <- cbind(synd2,
                                          SYNDROMIC="stillbirth")
        }
      }
      
      if (synd==3){
        synd3 <- rbind(SVASSS.CDB.alarms.data,
                      current.CDB.data[which(current.CDB.data$KOD=="6"&
                                               (as.Date(current.CDB.data$händelsedatum, format = "%m/%d/%y", origin="01/01/1970")
                                                ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),])
        if (dim(synd3)[1]>0){
          SVASSS.CDB.alarms.data <- cbind(synd3,
                                          SYNDROMIC="hemslakt")
        }
      }
      
      if (synd==4){
        synd4 <- rbind(SVASSS.CDB.alarms.data,
                       current.CDB.data[which((current.CDB.data$KOD=="7"|current.CDB.data$KOD=="8")&
                                                (as.Date(current.CDB.data$händelsedatum, format = "%m/%d/%y", origin="01/01/1970")
                                                 ==as.Date(dates.matrix.5days[date.row,1], format = "%Y-%m-%d"))),])
        if (dim(synd4)[1]>0){
          SVASSS.CDB.alarms.data <- cbind(synd4,
                                          SYNDROMIC="fallenstock")
        }
      }

  } #if > score
}  #for (synd in not.days.syndromes){

}



###########SJV
#not 
#equ 
#swi 
#sru 
#avi

#SVASSS.SJV.alarms.data <- not
#SVASSS.SJV.alarms.data <- SVASSS.SJV.alarms.data[0,]

min.date.SJV <- min(as.Date(cd$DATUM, 
                            format = "%m/%d/%y", origin="01/01/1970"))
max.date.SJV <- max(as.Date(cd$DATUM, 
                            format = "%m/%d/%y", origin="01/01/1970"))

keep.rows3 <- which(as.Date(SVASSS.SJV.alarms.data$DATUM, 
                            format = "%m/%d/%y", origin="01/01/1970")
                    <(min.date.SJV))

SVASSS.SJV.alarms.data <- SVASSS.SJV.alarms.data[keep.rows3,]


for (sp in 1:length(SJV_daily_only)){
  
  first.row <- min(which(SJV_daily_only[[sp]]@dates[,1]>=min.date.SJV))
  last.row <- length(SJV_daily_only[[sp]]@dates[,1])
  
  for (synd in 1:dim(SJV_daily_only[[sp]]@observed)[2]){
    
    for (r in first.row:last.row){
      
    if(SJV_daily_only[[sp]]@alarms[r,synd,1]>2){
      
      data.rows <- which(as.Date(SJV_data[[sp]]$DATUM, 
                                 format = "%m/%d/%y", origin="01/01/1970")
                         ==SJV_daily_only[[sp]]@dates[r,1]&
                           as.character(SJV_data[[sp]]$prediction.Syndrome.)==
                           colnames(SJV_daily_only[[sp]]@observed)[synd])
            
      SVASSS.SJV.alarms.data <- rbind(SVASSS.SJV.alarms.data,
                                      SJV_data[[sp]][data.rows,])
    }#if
    
      }#rows
  }#syndromes
}#species






for (sp in 1:length(SJV_weekly_only)){
  
  time.points <- as.numeric(max.date.SJV-min.date.SJV)+1
  #date.seq <- min.date.SJV:max.date.SJV
  
  #dates <- ISOweek2date(as.character(SJV_weekly_only[[sp]]@dates[,1]))
  #first.row <- min(which(dates>=min.date.SJV))
  #last.row <- length(dates)
  
  for (synd in 1:dim(SJV_weekly_only[[sp]]@observed)[2]){
    
    for (t in 1:time.points){
      
      date.ISOweek <- date2ISOweek(min.date.SJV+t-1)
      row <- which(substr(as.character(SJV_weekly_only[[sp]]@dates[,1]),1,8)==
                     substr(as.character(date.ISOweek),1,8)) 
      
      if(length(row)>0){
        if (is.na(SJV_weekly_only[[sp]]@alarms[row,synd,2])==FALSE&
          SJV_weekly_only[[sp]]@alarms[row,synd,2]>2){
        
        data.rows <- which(as.Date(SJV_data[[sp]]$DATUM, 
                                   format = "%m/%d/%y", origin="01/01/1970")
                           ==(min.date.SJV+t-1)&
                             as.character(SJV_data[[sp]]$prediction.Syndrome.)==
                             colnames(SJV_weekly_only[[sp]]@observed)[synd])
        
        SVASSS.SJV.alarms.data <- rbind(SVASSS.SJV.alarms.data,
                                        SJV_data[[sp]][data.rows,])
      }#if
      }
      
    }#rows
  }#syndromes
}#species







save(SVASSS.alarms.data,SVASSS.CDB.alarms.data,SVASSS.SJV.alarms.data, file="history_files\\SVASSS.alarms.data.RData")
#save(SVASSS.alarms.data,SVASSS.CDB.alarms.data,SVASSS.SJV.alarms.data, file="I:\\ESS\\GIS_Reports\\SVASSS\\SVASSS.alarms.data.RData")
save(SVASSS.alarms.data,SVASSS.CDB.alarms.data,SVASSS.SJV.alarms.data, file="//UBUNTU1/share/SVASSS.alarms.data.RData")
