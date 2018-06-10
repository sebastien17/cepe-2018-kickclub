library(shiny)
library(plotly)

dirpath <- "C:/Users/Eric/Documents/Eric/Pro/Transition/Formation/CEPE ENSAE ENSAI Certificat Data Scientist/INTENSIVE/PROJETS/KickClub Project/Lending Club/LC0715"
dirpath2 <- "C:/Users/Eric/Documents/Eric/Pro/Transition/Formation/R Projects/cepe-2018-kickclub/Shiny"

loan <- readRDS(paste0(dirpath,"/loan5.RDS"))
cl <- sapply(loan, class)
fact <- sort(colnames(loan)[which(cl == "factor")])
num <- sort(colnames(loan)[which(cl %in% c("integer", "numeric"))])

############################################ SDE ##############################################
##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##

# load model knn
knn_tune_none <- readRDS(paste0(dirpath2,"/model/SDE_knn_none_k 1 - 200_n 2527.RDS"))
knn_tune_up <- readRDS(paste0(dirpath2,"/model/SDE_knn_up_k 1 - 200_n 2527.RDS"))
knn_tune_down <- readRDS(paste0(dirpath2,"/model/SDE_knn_down_k 1 - 200_n 2527.RDS"))

knn_none_cm <- readRDS(paste0(dirpath2,"/model/knn_none_cm.RDS"))
knn_up_cm <- readRDS(paste0(dirpath2,"/model/knn_up_cm.RDS"))
knn_down_cm <- readRDS(paste0(dirpath2,"/model/knn_down_cm.RDS"))

##############################################################################

"Warning in origRenderFunc() :
  Ignoring explicitly provided widget ID 'xxxxx'; Shiny doesn't use them"
# options(warn = -1) # pour ne plus avoir de warning

############################################ SL ##############################################
##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##

models_list = list()

#RandomForest Specific part
RF_models = list("none" = readRDS(paste0(dirpath2,"/model/SL_RF_01_none.mod")),
                 "up" = readRDS(paste0(dirpath2,"/model/SL_RF_01_up.mod")),
                 "down" = readRDS(paste0(dirpath2,"/model/SL_RF_01_down.mod")))

#Adaboost Specific part
AB_models = list("none" = readRDS(paste0(dirpath2,"/model/SL_Adaboost_01_none.mod")),
                 "up" = readRDS(paste0(dirpath2,"/model/SL_Adaboost_01_up.mod")),
                 "down" = readRDS(paste0(dirpath2,"/model/SL_Adaboost_01_down.mod")))


#Merge models
models_list[["RF"]] <- RF_models
models_list[["AB"]] <- AB_models

##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
############################################# SL ##############################################


############################################ EC ##############################################
##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##

Results_list <- list()

#GLMNET Bootsrap: Specific part
GLMNET_BOOT_models <- list("none" = readRDS(paste0(dirpath2,"/model/EC_GLMNET_BOOT_01_none.mod")),
                 "up" = readRDS(paste0(dirpath2,"/model/EC_GLMNET_BOOT_01_up.mod")),
                 "down" = readRDS(paste0(dirpath2,"/model/EC_GLMNET_BOOT_01_down.mod")))

GLMNET_BOOT <- readRDS(paste0(dirpath2,"/model/EC_GLMNET_BOOT_01_results.mod"))

#GLMNET Cross Validation: Specific part
GLMNET_CV_models <- list("none" = readRDS(paste0(dirpath2,"/model/EC_GLMNET_CV_01_none.mod")),
                          "up" = readRDS(paste0(dirpath2,"/model/EC_GLMNET_CV_01_up.mod")),
                          "down" = readRDS(paste0(dirpath2,"/model/EC_GLMNET_CV_01_down.mod")))

GLMNET_CV <- readRDS(paste0(dirpath2,"/model/EC_GLMNET_CV_01_results.mod"))

#Merge models
models_list[["GLMNET_BOOT"]] <- GLMNET_BOOT_models
models_list[["GLMNET_CV"]] <- GLMNET_CV_models

#Merge models' results
Results_list[["GLMNET_BOOT"]] <- GLMNET_BOOT
Results_list[["GLMNET_CV"]] <- GLMNET_CV

##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
############################################# EC ##############################################


######################  UI DEFINITION #########################################################

### Define UI for application that draws a histogram

ui <- fluidPage(
  # Application title
  titlePanel("Titre"),
  # sidebarLayout(
  #   sidebarPanel(
  #   ),
  #   mainPanel(
 
   tabsetPanel(
    
     ############################################ SDE ##############################################
     ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##
    
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
    ), # end tabPanel("KNN"
    
    ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
    ############################################# SDE ##############################################
    
    
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
    ),
    
    ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
    ############################################# SL ##############################################
    
    
    ############################################ EC ##############################################
    ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##
    
    tabPanel("GLMNET Bootstrap",
             plotlyOutput("EC_f_measure_GLMNET_BOOT"),
             fluidRow(
               #column(4,
               #       div(style = "text-align:center","Down"),
               #       verbatimTextOutput("EC_GLMNET_BOOT_confusion_matrix_down")
               #),
               #column(4,
               #       div(style = "text-align:center","None"),
               #       verbatimTextOutput("EC_GLMNET_BOOT_confusion_matrix_none")
               #),
               #column(4,
               #       div(style = "text-align:center","Up"),
               #       verbatimTextOutput("EC_GLMNET_BOOT_confusion_matrix_up")
               #),
               column(4,
                      div(style = "text-align:center","Key Performance Metrics"),
                      verbatimTextOutput("EC_GLMNET_BOOT_results")
               ),
               column(4,
                      div(style = "text-align:center","Model infos","down"),
                      verbatimTextOutput("EC_GLMNET_BOOT_infos_down")
               ),
               column(4,
                      div(style = "text-align:center","Model infos","none"),
                      verbatimTextOutput("EC_GLMNET_BOOT_infos_none")
               ),
               column(4,
                      div(style = "text-align:center","Model infos", "up"),
                      verbatimTextOutput("EC_GLMNET_BOOT_infos_up")
               ))
    ),
    
    tabPanel("GLMNET Cross Validation",
             plotlyOutput("EC_f_measure_GLMNET_CV"),
             fluidRow(
               #column(4,
               #       div(style = "text-align:center","Down"),
               #       verbatimTextOutput("EC_GLMNET_CV_confusion_matrix_down")
               #),
               #column(4,
               #       div(style = "text-align:center","None"),
               #       verbatimTextOutput("EC_GLMNET_CV_confusion_matrix_none")
               #),
               #column(4,
               #       div(style = "text-align:center","Up"),
               #       verbatimTextOutput("EC_GLMNET_CV_confusion_matrix_up")
               #),
               column(4,
                      div(style = "text-align:center","Key Performance Metrics"),
                      verbatimTextOutput("EC_GLMNET_CV_results")
               ))
    )
    
    ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
    ############################################# EC ##############################################
    
    
    
  ) # end tabsetPanel(

  ) # end fluidPage(


#     )
# )


####################### SERVER ####################################################################

### Define server logic required to draw a histogram
server <- function(input, output) {
  
  ############################################ SDE ##############################################
  ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##
  
  ###### tab Variables #####
  output$graph_quanti <- renderPlotly({
    plt <- plot_ly(type = "box")
    for (i in num) {
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
    if (is.null(d)) {
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
  
  ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
  ############################################# SDE ##############################################
  
  
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
  
  
  ############################################ EC ##############################################
  ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##
  output$EC_f_measure_GLMNET_BOOT <- renderPlotly({
    plt <- plot_ly(mode = "lines", type = 'scatter3d') %>%
      add_trace(data = models_list[["GLMNET_BOOT"]][["none"]]$results, x = ~alpha, z = ~F, y = ~lambda, line = list(color = c("green")), name = "none") %>%
      add_trace(data = models_list[["GLMNET_BOOT"]][["up"]]$results, x = ~alpha, z = ~F, y = ~lambda, line = list(color = c("red")), name = "up") %>%
      add_trace(data = models_list[["GLMNET_BOOT"]][["down"]]$results, x = ~alpha, z = ~F, y = ~lambda, line = list(color = c("blue")), name = "down") %>%
      layout(xaxis = list(title = "Alpha", tickangle = -45),
             yaxis = list(title = "F-measure"))
  })
  
  output$EC_GLMNET_BOOT_results <- renderPrint(Results_list[["GLMNET_BOOT"]])
  output$EC_GLMNET_BOOT_infos_down <- renderPrint(models_list[["GLMNET_BOOT"]][["down"]])
  output$EC_GLMNET_BOOT_infos_none <- renderPrint(models_list[["GLMNET_BOOT"]][["none"]])
  output$EC_GLMNET_BOOT_infos_up <- renderPrint(models_list[["GLMNET_BOOT"]][["up"]])
  #output$SL_RF_confusion_matrix_down <- renderPrint(models_list[["RF"]][["down"]])
  #output$SL_RF_confusion_matrix_none <- renderPrint(models_list[["RF"]][["none"]])
  #output$SL_RF_confusion_matrix_up <- renderPrint(models_list[["RF"]][["up"]])
  
  
  output$EC_f_measure_GLMNET_CV <- renderPlotly({
    plt <- plot_ly(mode = "lines", type = 'scatter3d') %>%
      add_trace(data = models_list[["GLMNET_CV"]][["none"]]$results, x = ~alpha, y = ~lambda, z = ~F, line = list(color = c("green")), name = "none") %>%
      add_trace(data = models_list[["GLMNET_CV"]][["up"]]$results, x = ~alpha, y = ~lambda, z = ~F, line = list(color = c("red")), name = "up") %>%
      add_trace(data = models_list[["GLMNET_CV"]][["down"]]$results, x = ~alpha, y = ~lambda, z = ~F, line = list(color = c("blue")), name = "down") %>%
      layout(xaxis = list(title = "Alpha", tickangle = -45),
             yaxis = list(title = "F-measure"))
  })
  
  
  output$EC_GLMNET_CV_results <- renderPrint(Results_list[["GLMNET_CV"]])
  #output$SL_AB_confusion_matrix_down <- renderPrint(models_list[["AB"]][["down"]])
  #output$SL_AB_confusion_matrix_none <- renderPrint(models_list[["AB"]][["none"]])
  #output$SL_AB_confusion_matrix_up <- renderPrint(models_list[["AB"]][["up"]])
  
  
  ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
  ############################################# EC ##############################################
  
  
} # end of: server <- function(input, output)

shinyApp(ui = ui, server = server)

