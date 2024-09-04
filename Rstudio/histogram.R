library(shiny)
library(ggplot2)

# Load the CSV file
data <- read.csv("moviehaat.csv")

# Define the UI
ui <- fluidPage(
  titlePanel("Dashboard"),
  mainPanel(
    plotOutput("plot")
  )
)

# Define the server
server <- function(input, output, session) {
  
  # Generate the plot
  output$plot <- renderPlot({
    library(ggplot2)
    ggplot(movies_1, aes(x=Industry)) + geom_bar(fill="blue", color="red") + 
      
      labs(title="Simple Bar chart", 
           x="Indsutry", 
           y="Downloads")
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
