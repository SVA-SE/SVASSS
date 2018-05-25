





for (s in c(sp.colnames[[sp.position]],"Nonspecific")){
  
  
  non.svaga.object[[sp.position]][[s]]<-CD.species[CD.species$SYNDROMIC==s&CD.species$AGENS=="",]
  
  
  CD.syndrome <- CD.species[CD.species$SYNDROMIC==s&CD.species$AGENS!="",
                            c("AGENS","PÅVISAD","INSÄNTMATERIAL","ANKOMSTDATUM")]
  
  
  if(dim(CD.syndrome)[1]>0){
    
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
    
  }
  
  
}





