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
           #menu 1 ----
           #sidebarPanel(width=1,
          #              plotOutput("species.summary")
          # ),
                        
           #menu 2 ----
           sidebarPanel(width=2,
                        plotOutput("species.summary"),
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
           
           #menu 3 ----
           sidebarPanel(width=2,
                        
                        uiOutput("syndromes")
                        
                        
           ),
           
           #TABS ----
           
           mainPanel(
             tabsetPanel(id = "tabs",
               
               #TAB 1----
               tabPanel("Summary", value ="#panel_summary",
                        plotOutput("alarms.per.species"),
                        
                        h4("Select a specific syndrome onthe left, and navigate to detailed information"),
                        tags$a("Go to Specific alarm charts", href = "#panel_alarm_charts"),
                        br(),
                        tags$a("Go to MAPS", href = "#panel_maps"),
                        br(),
                        tags$a("See the original SVALA data", href = "#panel_data"),
                        br(),
                        h4("True alarms:"),
                        h5("Are alarms from the algorithm that can deal with seasonal effetc (Holt-Winters).
                            The plot shows a bar from 0 to 5, indicating the gravity of outbreak signal detected
                            in the data from last week. The red vertical lines are the alarm threshold 
                            for each syndrome - if the outbreak signal is strong enough to be greater than the
                            threshold, the bar is colored red, and an e-mail is sent to ESS stating 
                            'true alarms detected'."),
                        h5("These alarms are more trust-worthy, but only applicable when there is enough data. 
                            If a syndrome doesn't have a red vertical threshold, then the algorithm is NOT BEING
                            APPLIED. In this case, it is important to pay more attention to the secondary
                            alarms"),
                        h4("Secondary alarms:"),
                        h5("Outbreak signal using the algorithm EWMA, which does not account for seasons.
                           Please note that for syndromes for which there is no true (primary) alarm data, you should
                           pay close attention to secondary alarms.")
                        
                        
                        ),
               
               #TAB 2----
               
               tabPanel("Alarms charts", value ="#panel_alarm_charts",
                        
                        plotlyOutput("plot.alarms.svala"),
                        
                        h4("Change syndrome on the left"),
                        tags$a("Go to MAPS", href = "#panel_maps"),
                        br(),
                        tags$a("See the original SVALA data", href = "#panel_data"),
                        br(),
                        tags$a("Go back to summary", href = "#panel_summary")
                        
                        
                        
               ),
               
               
               
               #TAB 3----
               tabPanel("Maps", value ="#panel_maps",
                        
                        
                        h4("Change syndrome on the left"),
                        tags$a("Go to Specific alarm charts", href = "#panel_alarm_charts"),
                        br(),
                        tags$a("See the original SVALA data", href = "#panel_data"),
                        br(),
                        tags$a("Go back to summary", href = "#panel_summary")
                        
                        ),
               
               #TAB 4----
               tabPanel("Data", value ="#panel_data",
                        
                        
                        
                        h4("Change syndrome on the left"),
                        tags$a("Go to Specific alarm charts", href = "#panel_alarm_charts"),
                        br(),
                        tags$a("Go to MAPS", href = "#panel_maps"),
                        br(),
                        tags$a("Go back to summary", href = "#panel_summary")
                        
                        )
             )
           )
  ),
  
  #Navbar 2----
  tabPanel("Active Surveillance", "This panel is intentionally left blank"),
  
  
  #Navbar 3----
  tabPanel("Navbar 3", "This panel is intentionally left blank")
)
)
