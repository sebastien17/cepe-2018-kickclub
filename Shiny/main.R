library(shiny)
library(plotly)


loan <- readRDS("./loan.RDS")
cl <- sapply(loan, class)
fact <- sort(colnames(loan)[which(cl == "factor")])
num <- sort(colnames(loan)[which(cl %in% c("integer", "numeric"))])

# load model knn
knn_tune_none <- readRDS("./model/SDE_knn_none_k 1 - 200_n 2527.RDS")
knn_tune_up <- readRDS("./model/SDE_knn_up_k 1 - 200_n 2527.RDS")
knn_tune_down <- readRDS("./model/SDE_knn_down_k 1 - 200_n 2527.RDS")

knn_none_cm <- readRDS("./model/knn_none_cm.RDS")
knn_up_cm <- readRDS("./model/knn_up_cm.RDS")
knn_down_cm <- readRDS("./model/knn_down_cm.RDS")


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Titre"),
  # sidebarLayout(
  #   sidebarPanel(
  #   ),
  #   mainPanel(
  tabsetPanel(
    tabPanel("Variables",
             h2("Variables Quantitatives"),
             fluidRow(
              column(8,
                     plotlyOutput("graph_quanti")
                     ),
              column(4,
                     textOutput("graph_quanti_click"),
                     plotlyOutput("graph_quanti_details")
              )
             ),
             h2("Variables Qualitatives"),
             radioButtons("rb_quali", label = "", choices = fact, inline = TRUE),
             plotlyOutput("graph_quali")
    ),
    tabPanel("KNN",
             plotlyOutput("f_measure_knn"),
             fluidRow(
               column(4,
                      verbatimTextOutput("confusion_matrix_down")
               ),
               column(4,
                      verbatimTextOutput("confusion_matrix_none")
               ),
               column(4,
                      verbatimTextOutput("confusion_matrix_up")
               ))
    )
  )
)


#     )
# )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ###### tab Variables #####
  output$graph_quanti <- renderPlotly({
    plt <- plot_ly(type = "box")
    for(i in num){
      plt <- add_boxplot(plt, y = as.vector(scale(loan[, i])), name = i, visible = "legendonly")
    }
    plt
  })
  
  output$graph_quali <- renderPlotly({
    nb_not_paid <- nrow(loan[loan$loan_status == "CO",])
    nb_paid <- nrow(loan[loan$loan_status == "FP",])
    t <- table(loan[,c("loan_status", input$rb_quali)])
    
    t["CO",] <- t["CO",]/nb_not_paid
    t["FP",] <- t["FP",]/nb_paid
    
    x <- colnames(t)
    y1 <- as.vector(t["CO",])
    y2 <- as.vector(t["FP",])
    
    df <- data.frame(x, y1, y2)
    
    p_di <- plot_ly(df, x = ~x, y = ~y1, type = 'bar', name = 'Charged Off', marker = list(color = "rgb(255, 179, 179)",
                                                                                           line = list(color = "rgb(255, 128, 128)",
                                                                                                       width = 1.5))) %>%
      add_trace(y = ~y2, name = 'Fully Paid', marker = list(color = "rgb(159, 223, 190)",
                                                            line = list(color = "rgb(64, 191, 125)",
                                                                        width = 1.5))) %>%
      layout(xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = ""),
             margin = list(b = 100),
             barmode = 'group')
    
    t <- table(loan[,input$rb_quali])
    y <- as.vector(t)
    
    p_ag <- plot_ly(df, x = ~x, y = ~y, type = 'bar', name = 'All', marker = list(color = "rgb(194, 194, 239)",
                                                                                  line = list(color = "rgb(114, 114, 218)",
                                                                                              width = 1.5))) %>%
      layout(xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = ""),
             margin = list(b = 100),
             barmode = 'group')
    
    subplot(p_ag, p_di)
    
  })
  
  output$graph_quanti_click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)){
      "Cliquez sur un boxplot!"
    } else {
      col <- d[1,"x"]
      output$graph_quanti_details <- renderPlotly({
        plt <- plot_ly(type = "box")
        plt <- add_boxplot(plt, y = as.vector(loan[loan$loan_status == "FP", col]), name = "Fully Paid",
                           marker = list(color = 'rgb(159, 223, 190)'),
                                         line = list(color = "rgb(64, 191, 125)"))
        plt <- add_boxplot(plt, y = as.vector(loan[loan$loan_status == "CO", col]), name = "Charged Off",
                           marker = list(color = "rgb(255, 179, 179)"),
                                         line = list(color = "rgb(255, 128, 128)"))
        layout(plt,
               title = paste("Répartition pour la variable ", col))
      })
    }
  })
  
  ###### tab KNN #####
  output$f_measure_knn <- renderPlotly({
    plt <- plot_ly(mode = "lines", type = 'scatter') %>%
      add_trace(data = knn_tune_none$results, x = ~k, y = ~F, line = list(color = c("green")), name = "none") %>%
      add_trace(data = knn_tune_up$results, x = ~k, y = ~F, line = list(color = c("red")), name = "up") %>%
      add_trace(data = knn_tune_down$results, x = ~k, y = ~F, line = list(color = c("blue")), name = "down") %>%
      layout(xaxis = list(title = "K", tickangle = -45),
             yaxis = list(title = "F-measure"))
  })
  
  output$confusion_matrix_down <- renderPrint(knn_down_cm)
  output$confusion_matrix_none <- renderPrint(knn_none_cm)
  output$confusion_matrix_up <- renderPrint(knn_up_cm)
}

shinyApp(ui = ui, server = server)

