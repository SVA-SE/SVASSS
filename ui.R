require(shiny)
require(markdown)
require(shinythemes)
require(plotly)
require(DT)
library(bindrcpp)
#require(shinycssloaders)
#library(magrittr)
#library(shinyjs)


source("Definitions.r",local=TRUE,encoding="native.enc")
load(paste0(shiny.history,"/menu.summaries.RData"))

shinyUI(navbarPage(
  theme = shinythemes::shinytheme("united"),  
  "SVASSS",
  
  # useShinyjs(),
  # 
  # div(
  #   id = "loading_page",
  #   h1("Loading...")
  # ),
  # 
  # hidden(
  #   div(
  #     id = "main_content",
      
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
               
               #TAB Summary----
               tabPanel("Summary", value ="#panel_summary",
                        plotOutput("alarms.per.species"),#%>% withSpinner(),
                        
                        h4("Select a specific syndrome onthe left, and navigate to detailed information"),
                        tags$a("Go to Specific alarm charts", href = "#panel_alarm_charts"),
                        br(),
                        tags$a("Go to MAPS", href = "#panel_maps"),
                        br(),
                        tags$a("See the original SVALA data", href = "#panel_data"),
                        br(),
                        tags$a("See SVAGA data", href = "#panel_svaga"),
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
               
               #TAB Alarm charts----
               
               tabPanel("Alarms charts", value ="#panel_alarm_charts",
                        
                        plotlyOutput("plot.alarms.svala"),
                        
                        h4("Change syndrome on the left"),
                        tags$a("Go to MAPS", href = "#panel_maps"),
                        br(),
                        tags$a("See the original SVALA data", href = "#panel_data"),
                        br(),
                        tags$a("See SVAGA data", href = "#panel_svaga"),
                        br(),
                        tags$a("Go back to summary", href = "#panel_summary")
                        
                        
                        
               ),
               
               
               
               #TAB MAPS----
               tabPanel("Maps", value ="#panel_maps",
                        
                        
                        h4("Change syndrome on the left"),
                        tags$a("Go to Specific alarm charts", href = "#panel_alarm_charts"),
                        br(),
                        tags$a("See the original SVALA data", href = "#panel_data"),
                        br(),
                        tags$a("See SVAGA data", href = "#panel_svaga"),
                        br(),
                        tags$a("Go back to summary", href = "#panel_summary")
                        
                        ),
               
               #TAB DATA ----
               tabPanel("Data", value ="#panel_data",
                        #row 1 = select data
                        fluidRow(
                          column(4, uiOutput("week.table")),
                          column(4, uiOutput("pavisad.table"))
                        ),
                        
                        #row 2 = select columns
                        fluidRow(
                          checkboxGroupInput(inputId="columns.table", label="Select columns to display", 
                                             choices=columns.display.data, 
                                             selected=columns.display.data,
                                             inline = TRUE)
                        ),
                        #actionButton("table.go", "CLICK to generate table (only needed the first time, once generated, updates automatically)"),
                        
                        # Create a new row for the table.
                        fluidRow(
                          DT::dataTableOutput("table")
                        ),
                        
                        
                        h4("Change syndrome on the left"),
                        tags$a("Go to Specific alarm charts", href = "#panel_alarm_charts"),
                        br(),
                        tags$a("Go to MAPS", href = "#panel_maps"),
                        br(),
                        tags$a("See SVAGA data", href = "#panel_svaga"),
                        br(),
                        tags$a("Go back to summary", href = "#panel_summary")
                        
                        ),
               tabPanel("SVAGA", value ="#panel_svaga",
                        
                        sliderInput("svaga.weeks.slider", label = h4("Total number of weeks to plot"), min = 10, 
                                    max = 300, value = 53),
                        h4("The plot always displays data up to the current week. You can
                           select the number of historical weeks to include. You can also drag the mouse over the plot 
                           to select a smaller window to show."),
                        
                        actionButton("svaga.go", "CLICK to generate svaga plots (only needed the first time)"),
                        
                        uiOutput("svaga.plots"),
                        #plotOutput("plots"),
                        
                        
                        h4("Change syndrome on the left"),
                        tags$a("Go to Specific alarm charts", href = "#panel_alarm_charts"),
                        br(),
                        tags$a("Go to MAPS", href = "#panel_maps"),
                        br(),
                        tags$a("See the original SVALA data", href = "#panel_data"),
                        br(),
                        tags$a("Go back to summary", href = "#panel_summary")
                        
               )
             )
           )
  ),
  
  #Navbar 2----
  tabPanel("Active Surveillance", "This panel is intentionally left blank")
  
  
  #Navbar 3----
  #tabPanel("Navbar 3", "This panel is intentionally left blank")
  
    
#  ))
)
)
