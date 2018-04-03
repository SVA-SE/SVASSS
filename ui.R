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
              tags$h5("Species")
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("Tab 1",
                        h4("Table"),
                        h1("Header 1"),
                        h2("Header 2"),
                        h3("Header 3"),
                        h4("Header 4"),
                        h5("Header 5")
               ),
               tabPanel("Tab 2", "This panel is intentionally left blank"),
               tabPanel("Tab 3", "This panel is intentionally left blank")
             )
           )
  ),
  tabPanel("Navbar 2", "This panel is intentionally left blank"),
  tabPanel("Navbar 3", "This panel is intentionally left blank")
)
)
