


for (s in c(sp.colnames[[sp.position]],"Nonspecific")){
  
  
  #non.svaga.object[[sp.position]][[s]]<-CD.species[CD.species$SYNDROMIC==s&CD.species$AGENS=="",]
  
  
  CD.syndrome <- CD.species[CD.species$SYNDROMIC==s&CD.species$AGENS!="",
                            c("AGENS","PÅVISAD","INSÄNTMATERIAL","ANKOMSTDATUM")]
  
  
  if(dim(CD.syndrome)[1]>0){
    
    if(is.null(dim(svaga.object[[sp.position]][[s]])[1])){
      
      CD.syndrome$AGENS<- str_replace(CD.syndrome$AGENS, "\\(\\+\\)", "")
      syndromes.name=unique(c(colnames(svaga.object[[sp.position]][[s]]),CD.syndrome$AGENS))
      
      a.all=raw_to_syndromicW(id=INSÄNTMATERIAL,
                              syndromes.var=AGENS,
                              syndromes.name=syndromes.name,
                              dates.var=ANKOMSTDATUM,
                              date.format = "%d/%m/%Y",
                              min.date=ISOweek2date(weekly.object[[sp.position]]@dates[1,1]),
                              max.date=new.data.end,
                              data=CD.syndrome)
      
      CD.syndrome.pos=CD.syndrome[CD.syndrome$PÅVISAD=="Påvisad",]
      
      a.pos=raw_to_syndromicW(id=INSÄNTMATERIAL,
                              syndromes.var=AGENS,
                              syndromes.name=syndromes.name,
                              dates.var=ANKOMSTDATUM,
                              date.format = "%d/%m/%Y",
                              min.date=ISOweek2date(weekly.object[[sp.position]]@dates[1,1]),
                              max.date=new.data.end,
                              data=CD.syndrome.pos)
      
      
      svaga.object[[sp.position]][[s]] <- abind(a.all@observed,a.pos@observed,along=3)
    }else{
      
      
      if(dim(svaga.object[[sp.position]][[s]])[1]>=last.historical.row.week){
        
        svaga.object[[sp.position]][[s]] <- svaga.object[[sp.position]][[s]][1:last.historical.row.week,,,drop=FALSE]
        
        
        CD.syndrome$AGENS<- str_replace(CD.syndrome$AGENS, "\\(\\+\\)", "")
        syndromes.name=unique(c(colnames(svaga.object[[sp.position]][[s]]),CD.syndrome$AGENS))
        
        a.all=raw_to_syndromicW(id=INSÄNTMATERIAL,
                                syndromes.var=AGENS,
                                syndromes.name=syndromes.name,
                                dates.var=ANKOMSTDATUM,
                                date.format = "%d/%m/%Y",
                                min.date=new.data.start,
                                max.date=new.data.end,
                                data=CD.syndrome)
        
        CD.syndrome.pos=CD.syndrome[CD.syndrome$PÅVISAD=="Påvisad",]
        
        a.pos=raw_to_syndromicW(id=INSÄNTMATERIAL,
                                syndromes.var=AGENS,
                                syndromes.name=syndromes.name,
                                dates.var=ANKOMSTDATUM,
                                date.format = "%d/%m/%Y",
                                min.date=new.data.start,
                                max.date=new.data.end,
                                data=CD.syndrome.pos)
        
        svaga.new.all <- rbindlist(list(as.data.frame(svaga.object[[sp.position]][[s]][,,1]),
                                        as.data.frame(a.all@observed)),
                                   fill=TRUE)
        svaga.new.pos <- rbindlist(list(as.data.frame(svaga.object[[sp.position]][[s]][,,2]),
                                        as.data.frame(a.pos@observed)),
                                   fill=TRUE)
        
        
        svaga.object[[sp.position]][[s]] <- abind(svaga.new.all,svaga.new.pos,along=3)
        
      }else{
        
        
        #svaga.object[[sp.position]][[s]] 
        svaga.object.ends <- ISOweek2date(weekly.object[[sp.position]]@dates[dim(svaga.object[[sp.position]][[s]])[1],1])+6
        
        CD.syndrome$AGENS<- str_replace(CD.syndrome$AGENS, "\\(\\+\\)", "")
        syndromes.name=unique(c(colnames(svaga.object[[sp.position]][[s]]),CD.syndrome$AGENS))
        
        a.all=raw_to_syndromicW(id=INSÄNTMATERIAL,
                                syndromes.var=AGENS,
                                syndromes.name=syndromes.name,
                                dates.var=ANKOMSTDATUM,
                                date.format = "%d/%m/%Y",
                                min.date=svaga.object.ends+1,
                                max.date=new.data.end,
                                data=CD.syndrome)
        
        CD.syndrome.pos=CD.syndrome[CD.syndrome$PÅVISAD=="Påvisad",]
        
        a.pos=raw_to_syndromicW(id=INSÄNTMATERIAL,
                                syndromes.var=AGENS,
                                syndromes.name=syndromes.name,
                                dates.var=ANKOMSTDATUM,
                                date.format = "%d/%m/%Y",
                                min.date=svaga.object.ends+1,
                                max.date=new.data.end,
                                data=CD.syndrome.pos)
        
        svaga.new.all <- rbindlist(list(as.data.frame(svaga.object[[sp.position]][[s]][,,1]),
                                        as.data.frame(a.all@observed)),
                                   fill=TRUE)
        svaga.new.pos <- rbindlist(list(as.data.frame(svaga.object[[sp.position]][[s]][,,2]),
                                        as.data.frame(a.pos@observed)),
                                   fill=TRUE)
        
        
        svaga.object[[sp.position]][[s]] <- abind(svaga.new.all,svaga.new.pos,along=3)
        
        
      }
      
      
      
      
      }
    
    }
    
  }
  






