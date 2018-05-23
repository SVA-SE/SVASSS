

library(shiny)




shinyServer(function(input, output, session) {
  
  #data imported from outside ----
  source("Definitions.r",local=TRUE,encoding="native.enc")
  
  for(species in species.acronyms){
    eval(parse(text=paste0("load('",wd.history,species,".RData')")))
  }
  
  sp.syndromes.names <- lapply(1:length(sp.syndromes), function(x){
    synd.list <- as.list(sp.syndromes[[x]])
    names(synd.list) <- sp.syndromes[[x]]
    return(synd.list)
  })
 
  weekly.object <- list(CAT=CAT.weekly,
                        BOV=BOV.weekly,
                        DOG=DOG.weekly,
                        ENV=ENV.weekly,
                        FOD=FOD.weekly,
                        FSK=FSK.weekly,
                        EQU=EQU.weekly,
                        AVI=AVI.weekly,
                        SRU=SRU.weekly,
                        SWI=SWI.weekly,
                        VLT=VLT.weekly)
  
  
    #select syndromes checkboxes ----
  
    output$syndromes <- renderUI({
      input$species
          checkboxGroupInput("syndromes.list", "Select syndromes", 
                                               choices = sp.syndromes.names[[as.numeric(input$species)]],
                                               selected = sp.syndromes[[as.numeric(input$species)]]     )
      
  })

  #this plot works but only renders if few enough syndromes are chosen
  #need to generate an individual plot for each syndrome, and do that on the fly based on the
  #selected syndromes
  output$plot.alarms.svala <- renderPlot({
    input$species
    plot(x=weekly.object[[as.numeric(input$species)]],
                   syndromes=input$syndromes.list,
                   window=baseline.window.week,
                   baseline=TRUE,
                   UCL=1,
                   algorithms=1:2,
                   limit=3
                   )
  })
  
  
    
    
    
  
})
