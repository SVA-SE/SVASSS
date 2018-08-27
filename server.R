

require(shiny)
require(plotly)
require(ISOweek)
require(DT)
require(shinycssloaders)



# function to be able to decide the number of plots reactively
#in this case barplots for svaga
get_plot_output_list <- function(max_plots, barplot.data, weeks, weeks.to.plot) {
  # Insert plot output objects the list
  plot_output_list <- lapply(1:max_plots, function(i) {
    plotname <- paste("plot", i, sep="")
    plot_output_object <- plotlyOutput(plotname, height = 280, width = 250)
    plot_output_object <- renderPlotly({
      
      data.length = dim(barplot.data)[1]
      plot.window = (data.length-weeks.to.plot+1):data.length
      agens = colnames(barplot.data)[i]
      total.data <- barplot.data[plot.window,i,1]   #(total.data=posit.data+rpois(49,5))
      pos.data <- barplot.data[plot.window,i,2]     #(posit.data=rpois(49,5))
      neg.data <- total.data - pos.data  
      
      
      plot_ly(x = weeks[plot.window]) %>% 
        add_bars(y = pos.data,
                 name = 'Number of POSITIVE samples', type = 'scatter',
                 marker = list(color = 'red'))%>%
        add_bars(y = neg.data,
                 name = 'Number of tested samples', type = 'scatter',
                 marker = list(color = 'grey'))%>%
        layout(title = agens, barmode='stack',
               paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
               xaxis = list(title = "",
                            gridcolor = 'rgb(255,255,255)',
                            showgrid = TRUE,
                            showline = FALSE,
                            showticklabels = TRUE,
                            tickcolor = 'rgb(127,127,127)',
                            ticks = 'outside',
                            zeroline = FALSE),
               yaxis = list(title = "tests per week",
                            gridcolor = 'rgb(255,255,255)',
                            showgrid = TRUE,
                            showline = FALSE,
                            showticklabels = TRUE,
                            tickcolor = 'rgb(127,127,127)',
                            ticks = 'outside',
                            zeroline = FALSE),
               legend=list(orientation="h"))    
      
      
    })
  })
  
  do.call(tagList, plot_output_list) # needed to display properly.
  
  return(plot_output_list)
}


shinyServer(function(input, output, session) {
  
  #data imported from outside ----
  source("Definitions.r",local=TRUE,encoding="native.enc")
  
  load(paste0(shiny.history,"/status.RData"))
  load(paste0(shiny.history,"/classified.species.data.Rdata"))
  
  
  
  for(species in species.acronyms){
    eval(parse(text=paste0("load('",shiny.history,species,".RData')")))
  }
  
  sp.syndromes.names <- lapply(1:length(sp.syndromes), function(x){
    synd.list <- as.list(1:length(sp.syndromes[[x]]))#as.list(sp.syndromes[[x]])
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
  
  svaga.object <- list(CAT=CAT.svaga,
                       BOV=BOV.svaga,
                       DOG=DOG.svaga,
                       ENV=ENV.svaga,
                       FOD=FOD.svaga,
                       FSK=FSK.svaga,
                       EQU=EQU.svaga,
                       AVI=AVI.svaga,
                       SRU=SRU.svaga,
                       SWI=SWI.svaga,
                       VLT=VLT.svaga)
  
  
  
    #select syndromes checkboxes ----
  
    output$syndromes <- renderUI({
      input$species
      radioButtons("syndromes", "Select syndromes", 
                                               choices = sp.syndromes.names[[as.numeric(input$species)]],
                                               selected = 1     )
      
  })

  
  # renderPlot for syndromic data ----
  
  #input$species <- 2
  #input$syndromes <- "Respiratory"
  #--> can't be used for plotly -->input$syndromes <- c("Abortion","Respiratory")
  
  # output$plot.alarms.svala <- renderPlot({
  #   input$species
  #   plot(x=weekly.object[[as.numeric(input$species)]],
  #        syndromes=input$syndromes,
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
      add_trace(y = weekly.object[[as.numeric(input$species)]]@UCL[rows,sp.colnames[[as.numeric(input$species)]][[as.numeric(input$syndromes)]],9]*1.5,
                name = 'HW alarm level 5', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='red',
                fill = 'tozeroy') %>%
      add_trace(y = weekly.object[[as.numeric(input$species)]]@UCL[rows,sp.colnames[[as.numeric(input$species)]][[as.numeric(input$syndromes)]],8]*1.25,
                name = 'HW 4', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='tomato',
                fill = 'tozeroy') %>%
      add_trace(y = weekly.object[[as.numeric(input$species)]]@UCL[rows,sp.colnames[[as.numeric(input$species)]][[as.numeric(input$syndromes)]],7],
                name = 'HW 3', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='orange',
                fill = 'tozeroy') %>%
      add_trace(y = weekly.object[[as.numeric(input$species)]]@UCL[rows,sp.colnames[[as.numeric(input$species)]][[as.numeric(input$syndromes)]],6],
                name = 'HW 2', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='yellow',
                fill = 'tozeroy') %>%
      add_trace(y = weekly.object[[as.numeric(input$species)]]@UCL[rows,sp.colnames[[as.numeric(input$species)]][[as.numeric(input$syndromes)]],5],
                name = 'HW 1', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='lightgreen',
                fill = 'tozeroy') %>%
      # add_trace(y = weekly.object[[as.numeric(input$species)]]@baseline[rows,input$syndromes],
      #           name = '', type = 'scatter', mode = 'lines',
      #           line = list(color = 'transparent'),
      #           fillcolor='lightgreen',
      #           fill = 'tozeroy') %>%
      add_trace(y = weekly.object[[as.numeric(input$species)]]@observed[rows,sp.colnames[[as.numeric(input$species)]][[as.numeric(input$syndromes)]]],
                name = 'Recorded events', type = 'scatter', mode = 'lines+markers',
                line = list(shape = "linear",color="red"),
                marker=list(color="red")) %>%
      add_trace(y = weekly.object[[as.numeric(input$species)]]@baseline[rows,sp.colnames[[as.numeric(input$species)]][[as.numeric(input$syndromes)]]],
                name = 'Baseline (expected)', type = 'scatter', mode = 'lines',
                linetype = I(1),
                line = list(shape = "linear", color="black")) %>%
      #add_bars(y = weekly.object[[as.numeric(input$species)]]@alarms[rows,input$syndromes,1],
      #         name = 'Alarms HW', type = 'scatter',
      #         marker = list(color = 'rgba(55, 128, 191, 0.7)',
      #                       line = list(color = 'rgba(55, 128, 191, 0.7)',
      #                                   width = 4))) %>%
      add_trace(y = weekly.object[[as.numeric(input$species)]]@UCL[rows,sp.colnames[[as.numeric(input$species)]][[as.numeric(input$syndromes)]],2],
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
              names.arg=rev(sp.syndromes[[as.numeric(input$species)]]),
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
              names.arg=rev(sp.syndromes[[as.numeric(input$species)]]),
              horiz=TRUE,
              col=ifelse(plotc1>plotc2,"red",ifelse(plotc1==plotc2,"yellow","green")),
              main="Secondary alarms",
              las=1)
    points(y=c,x=plotc2,col="red",pch="|")
  })
  
  # data table tab ----
  
  output$week.table <- renderUI({
    selectInput("week.table",
              "Week:",
              c("All",
                rev(unique(as.character(display.data$week)))))
  })
  
  
  output$pavisad.table <- renderUI({
    selectInput("pavisad.table",
              "Påvisad:",
              c("All",
                unique(as.character(display.data$PÅVISAD))))
  })
  
  
   output$table <- DT::renderDataTable(DT::datatable(rownames= FALSE,{
     
     data <- display.data
     data <- data[data$SPECIES == species.original[as.numeric(input$species)],]
     data <- data[data$SYNDROMIC == sp.colnames[[as.numeric(input$species)]][as.numeric(input$syndromes)],]
     if (input$week.table != "All") {
       data <- data[data$week == input$week.table,]
     }
     if (input$pavisad.table != "All") {
       data <- data[data$PÅVISAD == input$pavisad.table,]
     }

          data[,input$columns.table, drop = FALSE]
   }, options=list(
     initComplete = JS(
       "function(settings, json) {",
       "$(this.api().table().header()).css({'font-size': '80%'});",
       "}"))
   )
   %>%
    DT::formatStyle(columns = input$columns.table, fontSize = '80%')
   )
  
 
  
  # svaga ----
   
   #figure out which dataset is counting
   svaga.dataset <- reactive({
     svaga.object[[as.numeric(input$species)]][[
       sp.colnames[[as.numeric(input$species)]][as.numeric(input$syndromes)]]]
   })
   
   
   
   observe({
     output$svaga.plots <- renderUI({ get_plot_output_list(dim(svaga.dataset())[2], 
                                                           svaga.dataset(),
                                                           ISOweek2date(weekly.object[[as.numeric(input$species)]]@dates[,1]),
                                                           input$svaga.weeks.slider
                                                           ) })
   })
   
   
   
   
   # # Insert the right number of plot output objects into the web page
   #  output$svaga.plots <- renderUI({
   #   plot_output_list <- lapply(1:(dim(svaga.dataset())[2]), function(i) {
   #     plotname <- paste("plot", i, sep="")
   #     plotOutput(plotname, height = 280, width = 250)
   #   })
   # 
   #   # Convert the list to a tagList - this is necessary for the list of items
   #   # to display properly.
   #   do.call(tagList, plot_output_list)
   # })
   # 
   # 
   # for (i in 1:(dim(svaga.dataset())[2])) {
   #   # Need local so that each item gets its own number. Without it, the value
   #   # of i in the renderPlot() will be the same across all instances, because
   #   # of when the expression is evaluated.
   #   local({
   #     my_i <- i
   #     plotname <- paste("plot", my_i, sep="")
   # 
   #     output[[plotname]] <- renderPlot({
   #       barplot(t(svaga.dataset()[,i,]))
   # 
   # 
   #     })
   #   })
   # }


   
  
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
    
    valid <- c("#panel_summary", "#panel_alarm_charts", "#panel_maps","#panel_data","#panel_svaga")
    
    if (hash %in% valid) {
      updateTabsetPanel(session, "tabs", hash)
    }
  })
  
  
  
})
