library(shiny)
library(plotly)


loan <- readRDS("../Data/loan5.RDS")
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


############################################ SL ##############################################
##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##

models_list = list()

#RandomForest Specific part
RF_models = list("none"=readRDS("./model/SL_RF_01_none.mod"),
                 "up"=readRDS("./model/SL_RF_01_up.mod"),
                 "down"=readRDS("./model/SL_RF_01_down.mod"))

#Adaboost Specific part
AB_models = list("none"=readRDS("./model/SL_Adaboost_01_none.mod"),
                 "up"=readRDS("./model/SL_Adaboost_01_up.mod"),
                 "down"=readRDS("./model/SL_Adaboost_01_down.mod"))


#Merge models
models_list[["RF"]] <- RF_models
models_list[["AB"]] <- AB_models

##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
############################################ SL ##############################################



# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Titre"),
  # sidebarLayout(
  #   sidebarPanel(
  #   ),
  #   mainPanel(
  tabsetPanel(
############################################ SL ##############################################
##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##

        tabPanel("Random Forest",
             plotlyOutput("SL_f_measure_RF"),
             fluidRow(
               column(4,
                      div(style="text-align:center","Down"),
                      verbatimTextOutput("SL_RF_confusion_matrix_down")
               ),
               column(4,
                      div(style="text-align:center","None"),
                      verbatimTextOutput("SL_RF_confusion_matrix_none")
               ),
               column(4,
                      div(style="text-align:center","Up"),
                      verbatimTextOutput("SL_RF_confusion_matrix_up")
               ))
    ),
    tabPanel("AdaBoost",
             plotlyOutput("SL_f_measure_AB"),
             fluidRow(
               column(4,
                      div(style="text-align:center","Down"),
                      verbatimTextOutput("SL_AB_confusion_matrix_down")
               ),
               column(4,
                      div(style="text-align:center","None"),
                      verbatimTextOutput("SL_AB_confusion_matrix_none")
               ),
               column(4,
                      div(style="text-align:center","Up"),
                      verbatimTextOutput("SL_AB_confusion_matrix_up")
               ))
    )

##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
############################################# SL ##############################################

  )
)


#     )
# )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ############################################ SL ##############################################
  ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##
  output$SL_f_measure_RF <- renderPlotly({
              plt <- plot_ly(mode = "lines", type = 'scatter') %>%
                add_trace(data = models_list[["RF"]][["none"]]$results, x = ~mtry, y = ~F, line = list(color = c("green")), name = "none") %>%
                add_trace(data = models_list[["RF"]][["up"]]$results, x = ~mtry, y = ~F, line = list(color = c("red")), name = "up") %>%
                add_trace(data = models_list[["RF"]][["down"]]$results, x = ~mtry, y = ~F, line = list(color = c("blue")), name = "down") %>%
                layout(xaxis = list(title = "Mtry", tickangle = -45),
                       yaxis = list(title = "F-measure"))
            })
  
  output$SL_RF_confusion_matrix_down <- renderPrint(models_list[["RF"]][["down"]])
  output$SL_RF_confusion_matrix_none <- renderPrint(models_list[["RF"]][["none"]])
  output$SL_RF_confusion_matrix_up <- renderPrint(models_list[["RF"]][["up"]])
  
  
  output$SL_f_measure_AB <- renderPlotly({
    plt <- plot_ly(mode = "lines", type = 'scatter') %>%
      add_trace(data = models_list[["AB"]][["none"]]$results, x = ~nIter, y = ~F, line = list(color = c("green")), name = "none") %>%
      add_trace(data = models_list[["AB"]][["up"]]$results, x = ~nIter, y = ~F, line = list(color = c("red")), name = "up") %>%
      add_trace(data = models_list[["AB"]][["down"]]$results, x = ~nIter, y = ~F, line = list(color = c("blue")), name = "down") %>%
      layout(xaxis = list(title = "nIter", tickangle = -45),
             yaxis = list(title = "F-measure"))
  })
  
  output$SL_AB_confusion_matrix_down <- renderPrint(models_list[["AB"]][["down"]])
  output$SL_AB_confusion_matrix_none <- renderPrint(models_list[["AB"]][["none"]])
  output$SL_AB_confusion_matrix_up <- renderPrint(models_list[["AB"]][["up"]])
  
  
  ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
  ############################################# SL ##############################################

}

shinyApp(ui = ui, server = server)

