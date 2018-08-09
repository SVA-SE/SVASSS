

library(shiny)
library(plotly)
library(ISOweek)
library(DT)




shinyServer(function(input, output, session) {
  
  #data imported from outside ----
  source("Definitions.r",local=TRUE,encoding="native.enc")
  
  load(paste0(wd.history,"/status.RData"))
  load(paste0(wd.sourcefiles,"/svala.data.RData"))
  display.data <- svala.data[svala.data.dates>=(max(svala.data.dates)-60),]
  
  
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
      add_trace(y = weekly.object[[as.numeric(input$species)]]@UCL[rows,input$syndromes.list,9]*1.5,
                name = 'HW alarm level 5', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='red',
                fill = 'tozeroy') %>%
      add_trace(y = weekly.object[[as.numeric(input$species)]]@UCL[rows,input$syndromes.list,8]*1.25,
                name = 'HW 4', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='tomato',
                fill = 'tozeroy') %>%
      add_trace(y = weekly.object[[as.numeric(input$species)]]@UCL[rows,input$syndromes.list,7],
                name = 'HW 3', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='orange',
                fill = 'tozeroy') %>%
      add_trace(y = weekly.object[[as.numeric(input$species)]]@UCL[rows,input$syndromes.list,6],
                name = 'HW 2', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='yellow',
                fill = 'tozeroy') %>%
      add_trace(y = weekly.object[[as.numeric(input$species)]]@UCL[rows,input$syndromes.list,5],
                name = 'HW 1', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='lightgreen',
                fill = 'tozeroy') %>%
      # add_trace(y = weekly.object[[as.numeric(input$species)]]@baseline[rows,input$syndromes.list],
      #           name = '', type = 'scatter', mode = 'lines',
      #           line = list(color = 'transparent'),
      #           fillcolor='lightgreen',
      #           fill = 'tozeroy') %>%
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
    
  
  # species summary ----
    
  output$species.summary  <- renderPlot({
    barplot(rep(1,length(status.true)),
            xlim=c(0,1),
            names.arg=rev(species.acronyms),
            horiz=TRUE,
            col=ifelse(rev(status.true)>0,"red",ifelse(rev(status.scnd)>0,"yellow","green")),
            main="Alarms",
            las=1,
            xaxt="n")
  }) 
  
  
  
  
  
  # syndromes per species summary ----
  
  output$alarms.per.species <- renderPlot({
    par(mfrow=c(1,2),mar=c(5,10,4,1)) #bottom, left, top, right
    
    plotb1=rev(weekly.object[[as.numeric(input$species)]]@alarms[
      dim(weekly.object[[as.numeric(input$species)]]@alarms)[1],,1])
    plotb2=rev(sp.weekly.hw.thresholds[[as.numeric(input$species)]])
    plotb2[plotb2>5]<-NA
    b=barplot(plotb1,
              xlim=c(0,5),
              names.arg=rev(sp.syndromes.names[[as.numeric(input$species)]]),
              horiz=TRUE,
              col=ifelse(plotb1>plotb2,"red",ifelse(plotb1==plotb2,"yellow","green")),
              main="TRUE alarms",
              las=1)
    points(y=b,x=plotb2,col="red",pch="|")
    
    plotc1=rev(weekly.object[[as.numeric(input$species)]]@alarms[
      dim(weekly.object[[as.numeric(input$species)]]@alarms)[1],,2])
    plotc2=rev(sp.weekly.ewma.thresholds[[as.numeric(input$species)]])
    plotc2[plotb2>5]<-NA
    c=barplot(plotc1,
              xlim=c(0,5),
              names.arg=rev(sp.syndromes.names[[as.numeric(input$species)]]),
              horiz=TRUE,
              col=ifelse(plotc1>plotc2,"red",ifelse(plotc1==plotc2,"yellow","green")),
              main="Secondary alarms",
              las=1)
    points(y=c,x=plotc2,col="red",pch="|")
  })
  
  # data table tab ----
  
  output$species.table <- renderUI({
    input$species
    selectInput("species.table",
                "Species:",
                c("All",species.names),
                selected = species.names[as.numeric(input$species)])
  })
  
  output$syndromes.table <- renderUI({
    input$species
    selectInput("syndromes.table",
                "Syndrome:",
                c("All",syndromes.names),
                selected = input$syndromes.list)
  })
  
  
  
  # output$table <- DT::renderDataTable(DT::datatable({
  #   data <- display.data
  #   if (input$species != "All") {
  #     data <- data[data$SPECIES == input$species.table,]
  #   }
  #   if (input$cyl != "All") {
  #     data <- data[data$cyl == input$cyl,]
  #   }
  #   if (input$trans != "All") {
  #     data <- data[data$trans == input$trans,]
  #   }
  #   data
  # }))
  # 
  # svala.data[,c("UPPDRAG","ANKOMSTDATUM","ÖVERORDNATUPPDRAG","ÖVERORDNADEUPPDRAG","PROVTAGNINGSORSAK","INSÄNTMATERIAL",
  #               "DJURSLAG","DIAGNOSER","RESULTATUNDERSÖKNING","RESULTATANALYS",
  #               "AGENS","PÅVISAD","ANALYSBESKRIVNING","ANALYSMATERIAL","UNDERSÖKNINGBESKRIVNING","MATERIAL",
  #               "PPN_original","CITY","SYNDROMIC","SPECIES")]
  #display.data
  #save week (save only 4-8 last weeks?)
  #"Avian" to "Poultry"  "Dog" to "Dogs"            "Environment" to "Environm."
  #"Cat" to "Cats"       "Equidae" to "Horses" 
  #syndrome labels to syndromes names
  #antimicrobial resistance and doodes not on syndromic
  
 
  
  
  
  # code to make links between tabs ----
  
  # When we change from one `tabPanel` to another, update the URL hash
  observeEvent(input$tabs, {
    
    # No work to be done if input$tabs and the hash are already the same
    if (getUrlHash() == input$tabs) return()
    
    # The 'push' argument is necessary so that the hash change event occurs and
    # so that the other observer is triggered.
    updateQueryString(
      paste0(getQueryString(), input$tabs),
      "push"
    )
    # Don't run the first time so as to not generate a circular dependency 
    # between the two observers
  }, ignoreInit = TRUE)
  
  # When the hash changes (due to clicking on the link in the sidebar or switching
  # between the `tabPanel`s), switch tabs and update an input. Note that clicking 
  # another `tabPanel` already switches tabs.
  observeEvent(getUrlHash(), {
    hash <- getUrlHash()
    
    # No work to be done if input$tabs and the hash are already the same
    if (hash == input$tabs) return()
    
    valid <- c("#panel_summary", "#panel_alarm_charts", "#panel_maps","#panel_data")
    
    if (hash %in% valid) {
      updateTabsetPanel(session, "tabs", hash)
    }
  })
  
  
  
})
