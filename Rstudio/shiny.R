library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

library(shinydashboard)
data <- read.csv("moviehaat.csv")

ui <- dashboardPage(
  
  dashboardHeader(title = "MovieHaat dashboard"),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th"))
  )),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot", height = 350)),
               
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 900, 50)
                )
              )
      ),
      
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  ) 
)


server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    library(ggplot2)
    ggplot(movies_1, aes(x=Industry)) + geom_bar(fill="blue", color="red") + 
      
      labs(title="Movie Downloads", 
           x="Indsutry", 
           y="Downloads")
    
  })
}
shinyApp(ui, server)