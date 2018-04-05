

library(shiny)

shinyServer(function(input, output, session) {
  
  #data imported from outside ----
   source("Definitions.r",local=TRUE,encoding="native.enc")
  sp.syndromes.names <- lapply(1:length(sp.syndromes), function(x){
    synd.list <- as.list(sp.syndromes[[x]])
    names(synd.list) <- sp.syndromes[[x]]
    return(synd.list)
  })
 
  
  
    #select syndromes checkboxes ----
  
    output$syndromes <- renderUI({
      input$species
          checkboxGroupInput("syndromes.list", "Select syndromes", 
                                               choices = sp.syndromes.names[[as.numeric(input$species)]],
                                               selected = sp.syndromes[[as.numeric(input$species)]]     )
      
  })

  
    
    
    
    
  
})
