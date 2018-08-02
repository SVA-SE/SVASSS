if (!require("shiny")) install.packages("shiny")
require(shiny)
if (!require("markdown")) install.packages("markdown")
require(markdown)
if (!require("shinythemes")) install.packages("shinythemes")
require(shinythemes)
if (!require("plotly")) install.packages("plotly")
require(plotly)


library(shiny)

source("Definitions.r",local=TRUE,encoding="native.enc")


shinyUI(navbarPage(
  theme = shinythemes::shinytheme("united"),  
  "SVASSS",
  
  # tabpanel=Syndromic Surveillance ----
  
  tabPanel("Syndromic Surveillance",
           sidebarPanel(width=2,
             
                        #menu 1 ----
              radioButtons("species", label = h3("Choose species"),
                           choices = list(
                             "Cats" = 1, 
                             "Cattle" = 2, 
                             "Dogs" = 3,
                             "Environmental" = 4,
                             "Feed" = 5,
                             "Fish" = 6,
                             "Horses" = 7,
                             "Poultry" = 8,
                             "Small Ruminants"= 9,
                             "Swine" = 10,
                             "Wildlife" = 11), 
                           selected = 2)
           ),
           
           #menu 2 ----
           sidebarPanel(width=2,
                        
                        uiOutput("syndromes")
                        
                        
           ),
           
           #TABS ----
           
           mainPanel(
             tabsetPanel(
               
               #TAB 1----
               tabPanel("Alarms",
                        
                        plotlyOutput("plot.alarms.svala")
                        # h4("Table"),
                        # h1("Header 1"),
                        # h2("Header 2"),
                        # h3("Header 3"),
                        # h4("Header 4"),
                        # h5("Header 5")
               ),
               
               #TAB 2----
               tabPanel("Charts", "This panel is intentionally left blank"),
               
               #TAB 3----
               tabPanel("Maps", "This panel is intentionally left blank"),
               
               #TAB 4----
               tabPanel("Data", "This panel is intentionally left blank")
             )
           )
  ),
  
  #Navbar 2----
  tabPanel("Active Surveillance", "This panel is intentionally left blank"),
  
  
  #Navbar 3----
  tabPanel("Navbar 3", "This panel is intentionally left blank")
)
)
