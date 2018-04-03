if (!require("shiny")) install.packages("shiny")
require(shiny)
if (!require("markdown")) install.packages("markdown")
require(markdown)
if (!require("shinythemes")) install.packages("shinythemes")
require(shinythemes)


library(shiny)


shinyUI(navbarPage(
  theme = shinythemes::shinytheme("united"),  
  "SVASSS",
  tabPanel("Syndromic Surveillance",
           sidebarPanel(
              tags$h3("Choose Species"),
              tags$h3("Daily/Weekly"),
              tags$h3("Choose data sources")
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("Alarms",
                        h4("Table"),
                        h1("Header 1"),
                        h2("Header 2"),
                        h3("Header 3"),
                        h4("Header 4"),
                        h5("Header 5")
               ),
               tabPanel("Charts", "This panel is intentionally left blank"),
               tabPanel("Maps", "This panel is intentionally left blank"),
               tabPanel("Data", "This panel is intentionally left blank")
             )
           )
  ),
  tabPanel("Navbar 2", "This panel is intentionally left blank"),
  tabPanel("Navbar 3", "This panel is intentionally left blank")
)
)
