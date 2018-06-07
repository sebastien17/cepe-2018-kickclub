library(shiny)
library(plotly)

#load data
loan <- readRDS("..\\Data\\loan5.RDS")

#load models
models_list = list()

#RF Specific part
RF_models = list("none"=readRDS("..\\Model\\SL_RF_01_none.mod"),
                 "up"=readRDS("..\\Model\\SL_RF_01_up.mod"),
                 "down"=readRDS("..\\Model\\SL_RF_01_down.mod"))

RF_perf <- data.frame("mtry" = RF_models[["none"]]$results$mtry,
                      "none" = RF_models[["none"]]$results$F,
                      "up" = RF_models[["up"]]$results$F,
                      "down" = RF_models[["down"]]$results$F
                      )


#Merge models
models_list[["RF"]] <- RF_models

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
        tabPanel(
          "RF",
          sidebarLayout(
            sidebarPanel(),
            mainPanel(plotlyOutput("SL_perf_chart"))
            )
          ),
        tabPanel("tab2")
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$SL_perf_chart <- renderPlotly({
    p <- ggplot(data = RF_perf, aes(x = mtry)) +
      geom_line(aes(y = none, color="red")) +
      geom_line(aes(y = up, color="green")) +
      geom_line(aes(y = down, color="blue"))
    ggplotly(p)
    #plot_ly(RF_perf, x = ~mtry, y = ~none, type = "scatter", mode="line") %>%
    #  add_trace(RF_perf, x = ~mtry, y = ~down, type = "scatter", mode="line")  %>%
    #  add_trace(RF_perf, x = ~mtry, y = ~up, type = "scatter", mode="line")
      
  })
    

}

shinyApp(ui = ui, server = server)

