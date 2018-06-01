library(shiny)
library(plotly)


loan <- readRDS("C:/Users/sebde/OneDrive/Documents/cepe-2018-kickclub/Data/loan5.RDS")

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Titre"),
  sidebarLayout(
    sidebarPanel(
      textOutput("text")
    ),
    mainPanel(
        tabsetPanel(
          tabPanel("tab1"),
          tabPanel("tab2")
        )
      )
       
      
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    

}

shinyApp(ui = ui, server = server)

