

library(shiny)
library(plotly)
library(ISOweek)




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
      radioButtons("syndromes.list", "Select syndromes", 
                                               choices = sp.syndromes.names[[as.numeric(input$species)]],
                                               selected = sp.syndromes[[as.numeric(input$species)]][1]     )
      
  })

  
  # renderPlot for syndromic data ----
  
  #input$species <- 2
  #input$syndromes.list <- "Respiratory"
  #--> can't be used for plotly -->input$syndromes.list <- c("Abortion","Respiratory")
  
  # output$plot.alarms.svala <- renderPlot({
  #   input$species
  #   plot(x=weekly.object[[as.numeric(input$species)]],
  #        syndromes=input$syndromes.list,
  #        window=baseline.window.week,
  #        baseline=TRUE,
  #        UCL=1,
  #        algorithms=1:2,
  #        limit=3
  #   )
  # })
    
  
  
  output$plot.alarms.svala <- renderPlotly({
    
    rows <- (length(weekly.object[[as.numeric(input$species)]]@dates[,1])-150):
      length(weekly.object[[as.numeric(input$species)]]@dates[,1])
    
    plot_ly(x = ISOweek2date(weekly.object[[as.numeric(input$species)]]@dates[rows,1])) %>% 
      add_trace(y = weekly.object[[as.numeric(input$species)]]@UCL[rows,input$syndromes.list,1]*1.5,
                name = '99%CI', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='tomato',
                fill = 'tozeroy') %>%
      add_trace(y = weekly.object[[as.numeric(input$species)]]@UCL[rows,input$syndromes.list,1]*1.25,
                name = '97.5%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='orange',
                fill = 'tozeroy') %>%
      add_trace(y = weekly.object[[as.numeric(input$species)]]@UCL[rows,input$syndromes.list,1],
                name = '95%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='yellow',
                fill = 'tozeroy') %>%
      add_trace(y = weekly.object[[as.numeric(input$species)]]@baseline[rows,input$syndromes.list],
                name = '', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='lightgreen',
                fill = 'tozeroy') %>%
      add_trace(y = weekly.object[[as.numeric(input$species)]]@observed[rows,input$syndromes.list],
                name = 'Recorded events', type = 'scatter', mode = 'lines+markers',
                line = list(shape = "linear",color="red"),
                marker=list(color="red")) %>%
      add_trace(y = weekly.object[[as.numeric(input$species)]]@baseline[rows,input$syndromes.list],
                name = 'Baseline (expected)', type = 'scatter', mode = 'lines',
                linetype = I(1),
                line = list(shape = "linear", color="black")) %>%
      #add_bars(y = weekly.object[[as.numeric(input$species)]]@alarms[rows,input$syndromes.list,1],
      #         name = 'Alarms HW', type = 'scatter',
      #         marker = list(color = 'rgba(55, 128, 191, 0.7)',
      #                       line = list(color = 'rgba(55, 128, 191, 0.7)',
      #                                   width = 4))) %>%
      add_trace(y = weekly.object[[as.numeric(input$species)]]@UCL[rows,input$syndromes.list,2],
                name = '95% CI secondary alarms', type = 'scatter', mode = 'lines',
                linetype = I(1),
                line = list(shape = "linear", color="purple",dash = 'dot')) %>%
      layout(title = "Syndromic events",
             paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
             xaxis = list(title = "weeks",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE),
             yaxis = list(title = "test submissions per week",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE)) %>%
      rangeslider()
    
    
    
    
  })
    
    
    
  
})
