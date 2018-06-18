library(shiny)
library(plotly)

dirpath <- "C:/Users/Eric/Documents/Eric/Pro/Transition/Formation/CEPE ENSAE ENSAI Certificat Data Scientist/INTENSIVE/PROJETS/KickClub Project/Lending Club/LC0715"
dirpath2 <- "."

models_list = list()
Results_list <- list()

loan <- readRDS("./loan.RDS")
cl <- sapply(loan, class)
fact <- sort(colnames(loan)[which(cl == "factor")])
num <- sort(colnames(loan)[which(cl %in% c("integer", "numeric"))])


############################################ SDE ##############################################
##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##

KNN_0_models <-  list("none" = readRDS("./model/SDE_knn_none_k 1 - 200_n 2527.RDS"),
                 "up" = readRDS("./model/SDE_knn_up_k 1 - 200_n 2527.RDS"),
                 "down" = readRDS("./model/SDE_knn_down_k 1 - 200_n 2527.RDS"))

models_list[["KNN_SD_0"]] <- KNN_0_models

# load model knn
# SDE_knn_tune_none <- readRDS("./model/SDE_knn_none_k 1 - 200_n 2527.RDS")
# SDE_knn_tune_up <- readRDS("./model/SDE_knn_up_k 1 - 200_n 2527.RDS")
# SDE_knn_tune_down <- readRDS("./model/SDE_knn_down_k 1 - 200_n 2527.RDS")

# SDE_knn_tune_results <- readRDS("./model/SDE_knn_k 1 - 200_n 2527_results.RDS")

SDE_knn_tune_roc <- readRDS("./model/SDE_knn_k 1 - 200_n 2527_roc.RDS")

SDE_knn_none_cm <- readRDS("./model/SDE_knn_none_k 1 - 200_n 2527_cm.RDS")
SDE_knn_up_cm <- readRDS("./model/SDE_knn_up_k 1 - 200_n 2527_cm.RDS")
SDE_knn_down_cm <- readRDS("./model/SDE_knn_down_k 1 - 200_n 2527_cm.RDS")

Results_list[["KNN_SD_0"]] <- readRDS("./model/SDE_knn_k 1 - 200_n 2527_results.RDS")

##############################################################################

"Warning in origRenderFunc() :
  Ignoring explicitly provided widget ID 'xxxxx'; Shiny doesn't use them"
# options(warn = -1) # pour ne plus avoir de warning

############################################ SL ##############################################
##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##

#RandomForest Specific part
RF_models = list("none" = readRDS(paste0(dirpath2,"/model/SL_RF_01_none.mod")),
                 "up" = readRDS(paste0(dirpath2,"/model/SL_RF_01_up.mod")),
                 "down" = readRDS(paste0(dirpath2,"/model/SL_RF_01_down.mod")))

RF <- readRDS(paste0(dirpath2,"/model/SL_RF_01_results.mod"))

#Adaboost Specific part
AB_models = list("none" = readRDS(paste0(dirpath2,"/model/SL_Adaboost_01_none.mod")),
                 "up" = readRDS(paste0(dirpath2,"/model/SL_Adaboost_01_up.mod")),
                 "down" = readRDS(paste0(dirpath2,"/model/SL_Adaboost_01_down.mod")))

AB <- readRDS(paste0(dirpath2,"/model/SL_Adaboost_01_results.mod"))

#Merge models
models_list[["RF"]] <- RF_models
models_list[["AB"]] <- AB_models

Results_list[["RF"]] <- RF
Results_list[["AB"]] <- AB

##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
############################################# SL ##############################################


############################################ EC ##############################################
##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##

# Results_list <- list()  # deplace en haut pour permettre inclusion AB et RF

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

#KNN sans eclatement des modalites et sans scaling: Specific part
KNN_EC_01_models <- list("none" = readRDS(paste0(dirpath2,"/model/EC_KNN_01_none.mod")),
                         "up" = readRDS(paste0(dirpath2,"/model/EC_KNN_01_up.mod")),
                         "down" = readRDS(paste0(dirpath2,"/model/EC_KNN_01_down.mod")))

KNN_EC_01 <- readRDS(paste0(dirpath2,"/model/EC_KNN_01_results.mod"))

#KNN sans eclamtement des modalites et sans sclaing: Specific part
KNN_EC_02_models <- list("none" = readRDS(paste0(dirpath2,"/model/EC_KNN_02_none.mod")),
                         "up" = readRDS(paste0(dirpath2,"/model/EC_KNN_02_up.mod")),
                         "down" = readRDS(paste0(dirpath2,"/model/EC_KNN_02_down.mod")))

KNN_EC_02 <- readRDS(paste0(dirpath2,"/model/EC_KNN_02_results.mod"))

#KNN sans eclamtement des modalites et sans sclaing: Specific part
GBM_01_models <- list("none" = readRDS(paste0(dirpath2,"/model/EC_GBM_01_none.mod")),
                         "up" = readRDS(paste0(dirpath2,"/model/EC_GBM_01_up.mod")),
                         "down" = readRDS(paste0(dirpath2,"/model/EC_GBM_01_down.mod")))

GBM_01 <- readRDS(paste0(dirpath2,"/model/EC_GBM_01_results.mod"))

#Merge models
models_list[["GLMNET_BOOT"]] <- GLMNET_BOOT_models
models_list[["GLMNET_CV"]] <- GLMNET_CV_models
models_list[["KNN_EC_01"]] <- KNN_EC_01_models
models_list[["KNN_EC_02"]] <- KNN_EC_02_models
models_list[["GBM_01"]] <- GBM_01_models

#Merge models' results
Results_list[["GLMNET_BOOT"]] <- GLMNET_BOOT
Results_list[["GLMNET_CV"]] <- GLMNET_CV
Results_list[["KNN_EC_01"]] <- KNN_EC_01
Results_list[["KNN_EC_02"]] <- KNN_EC_02
Results_list[["GBM_01"]] <- GBM_01

##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
############################################# EC ##############################################





all_measure <- data.frame()

for(mod.name in names(Results_list)){
  for(sk in c("down", "none", "up")){
    df <- data.frame()
    df <- as.data.frame(Results_list[[mod.name]][,sk])
    colnames(df) <- "measure"
    df["measure_name"] <- rownames(df)
    df["sampling_kind"] <- sk
    df["modele"] <- mod.name
    
    all_measure <- rbind(all_measure, df)
  }
}

measures <- c("Accuracy",
              "Kappa",
              "F-measure",
              "Sensitivity",
              "Specificity",
              "Precision / posPredValue",
              "negPredValue",
              "AUC")

measure_method <- as.data.frame(rep("max", length(measures)), row.names = measures)
colnames(measure_method) <- "method"

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
    tabPanel("GENERAL",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("measure_name", label = "Criteria", choices = c("Accuracy",
                                                                              "Kappa",
                                                                              "F-measure",
                                                                              "Sensitivity",
                                                                              "Specificity",
                                                                              "Precision / posPredValue",
                                                                              "negPredValue",
                                                                              "AUC"))
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 dataTableOutput("best_model_table")
               )
             )
             
             
             
    ),
    tabPanel("KNN 0",
             fluidRow(
               column(8,
                      h2("F-measure"),
                      plotlyOutput("SDE_knn_f_measure")
               ),
               column(4,
                      h2("ROC"),
                      plotlyOutput("SDE_knn_roc")
               )
             ),
             fluidRow(
               column(4,
                      h2("Analytics"),
                      dataTableOutput("SDE_knn_results")
               ),
               column(8,
                      h2("Confusion Matrix"),
                      plotOutput("SDE_knn_cm_all")
               )
             ),
             fluidRow(
               h2("Model details"),
               column(4,
                      verbatimTextOutput("SDE_knn_cm_down")
               ),
               column(4,
                      verbatimTextOutput("SDE_knn_cm_none")
               ),
               column(4,
                      verbatimTextOutput("SDE_knn_cm_up")
               )
             )
             
             
    ), # end tabPanel("KNN 0")
    
    ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
    ############################################# SDE ##############################################
    
    
    ############################################ SL ##############################################
    ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##
    
    tabPanel("Random Forest",
             plotlyOutput("SL_f_measure_RF"),
             fluidRow(
               column(4,
                      div(style = "text-align:center","Down"),
                      verbatimTextOutput("SL_RF_confusion_matrix_down")
               ),
               column(4,
                      div(style = "text-align:center","None"),
                      verbatimTextOutput("SL_RF_confusion_matrix_none")
               ),
               column(4,
                      div(style = "text-align:center","Up"),
                      verbatimTextOutput("SL_RF_confusion_matrix_up")
               ),column(4,
                        div(style = "text-align:center","Key Performance Metrics"),
                        verbatimTextOutput("SL_RF_results")
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
               ),column(4,
                        div(style = "text-align:center","Key Performance Metrics"),
                        verbatimTextOutput("SL_AB_results")
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
               ),
               column(4,
                      div(style = "text-align:center","Model infos","down"),
                      verbatimTextOutput("EC_GLMNET_CV_infos_down")
               ),
               column(4,
                      div(style = "text-align:center","Model infos","none"),
                      verbatimTextOutput("EC_GLMNET_CV_infos_none")
               ),
               column(4,
                      div(style = "text-align:center","Model infos", "up"),
                      verbatimTextOutput("EC_GLMNET_CV_infos_up")
               ))
    ),
    
    tabPanel("KNN EC 01",
             plotlyOutput("EC_f_measure_KNN_EC_01"),
             fluidRow(
               #column(4,
               #       div(style = "text-align:center","Down"),
               #       verbatimTextOutput("EC_KNN_01_confusion_matrix_down")
               #),
               #column(4,
               #       div(style = "text-align:center","None"),
               #       verbatimTextOutput("EC_KNN_01_confusion_matrix_none")
               #),
               #column(4,
               #       div(style = "text-align:center","Up"),
               #       verbatimTextOutput("EC_KNN_01_confusion_matrix_up")
               #),
               column(4,
                      div(style = "text-align:center","Key Performance Metrics"),
                      verbatimTextOutput("EC_KNN_01_results")
               ),
               column(4,
                      div(style = "text-align:center","Model infos","down"),
                      verbatimTextOutput("EC_KNN_01_infos_down")
               ),
               column(4,
                      div(style = "text-align:center","Model infos","none"),
                      verbatimTextOutput("EC_KNN_01_infos_none")
               ),
               column(4,
                      div(style = "text-align:center","Model infos", "up"),
                      verbatimTextOutput("EC_KNN_01_infos_up")
               ))
    ),
    
    tabPanel("KNN EC 02",
             plotlyOutput("EC_f_measure_KNN_EC_02"),
             fluidRow(
               #column(4,
               #       div(style = "text-align:center","Down"),
               #       verbatimTextOutput("EC_KNN_02_confusion_matrix_down")
               #),
               #column(4,
               #       div(style = "text-align:center","None"),
               #       verbatimTextOutput("EC_KNN_02_confusion_matrix_none")
               #),
               #column(4,
               #       div(style = "text-align:center","Up"),
               #       verbatimTextOutput("EC_KNN_02_confusion_matrix_up")
               #),
               column(4,
                      div(style = "text-align:center","Key Performance Metrics"),
                      verbatimTextOutput("EC_KNN_02_results")
               ),
               column(4,
                      div(style = "text-align:center","Model infos","down"),
                      verbatimTextOutput("EC_KNN_02_infos_down")
               ),
               column(4,
                      div(style = "text-align:center","Model infos","none"),
                      verbatimTextOutput("EC_KNN_02_infos_none")
               ),
               column(4,
                      div(style = "text-align:center","Model infos", "up"),
                      verbatimTextOutput("EC_KNN_02_infos_up")
               ))
    ),
    
    tabPanel("GBM 01",
             plotlyOutput("EC_f_measure_EC_GBM_01"),
             fluidRow(
               #column(4,
               #       div(style = "text-align:center","Down"),
               #       verbatimTextOutput("EC_GBM_01_confusion_matrix_down")
               #),
               #column(4,
               #       div(style = "text-align:center","None"),
               #       verbatimTextOutput("EC_GBM_01_confusion_matrix_none")
               #),
               #column(4,
               #       div(style = "text-align:center","Up"),
               #       verbatimTextOutput("EC_GBM_01_confusion_matrix_up")
               #),
               column(4,
                      div(style = "text-align:center","Key Performance Metrics"),
                      verbatimTextOutput("EC_GBM_01_results")
               ),
               column(4,
                      div(style = "text-align:center","Model infos","down"),
                      verbatimTextOutput("EC_GBM_01_infos_down")
               ),
               column(4,
                      div(style = "text-align:center","Model infos","none"),
                      verbatimTextOutput("EC_GBM_01_infos_none")
               ),
               column(4,
                      div(style = "text-align:center","Model infos", "up"),
                      verbatimTextOutput("EC_GBM_01_infos_up")
               ))
    ),
    
    ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
    ############################################# EC ##############################################
 
  
  ############################################ GG ##############################################
  ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##   
  
  tabPanel("AUTRE"
           
  )
)
)

##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
############################################# GG ##############################################


####################### SERVER ####################################################################

### Define server logic required to draw a histogram
server <- function(input, output) {
  
  ############################################ SDE ##############################################
  ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##
  
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
  
  ###### tab GENERAL #####
  output$best_model_table <- DT::renderDataTable({

    method <- TRUE
    if(measure_method[input$measure_name, "method"] == "min"){
      method <- FALSE
    }

    df_f <- na.omit(all_measure[all_measure[,"measure_name"] == input$measure_name,])
    row_order <- order(df_f$measure, decreasing = method)
    df_f_o <- df_f[row_order,]

    datatable(df_f_o, rownames = FALSE, selection = list(selected = c(1), target = 'row'))
    
  })
  
  
  ###### tab KNN #####
  output$SDE_knn_f_measure <- renderPlotly({
    plot_ly(mode = "lines", type = 'scatter') %>%
      add_trace(data = models_list$KNN_SD_0$none$results, x = ~k, y = ~F, line = list(color = c("green")), name = "none") %>%
      add_trace(data = models_list$KNN_SD_0$up$results, x = ~k, y = ~F, line = list(color = c("red")), name = "up") %>%
      add_trace(data = models_list$KNN_SD_0$down$results, x = ~k, y = ~F, line = list(color = c("blue")), name = "down") %>%
      layout(xaxis = list(title = "K", tickangle = -45),
             yaxis = list(title = "F-measure"))
  })
  
  output$SDE_knn_roc <- renderPlotly({
    plot_ly(mode = "lines", type = 'scatter') %>%
      add_trace(data = as.data.frame(t(SDE_knn_tune_roc$none)), x = ~specificity, y = ~sensitivity, line = list(color = c("green")), name = "none") %>%
      add_trace(data = as.data.frame(t(SDE_knn_tune_roc$up)), x = ~specificity, y = ~sensitivity, line = list(color = c("red")), name = "up") %>%
      add_trace(data = as.data.frame(t(SDE_knn_tune_roc$down)), x = ~specificity, y = ~sensitivity, line = list(color = c("blue")), name = "down") %>%
      layout(xaxis = list(title = "Specificity", tickangle = -45, autorange = "reversed"),
             yaxis = list(title = "Sensitivity"))
  })
  
  output$SDE_knn_cm_all <- renderPlot({
    cm_array <- array(c(SDE_knn_down_cm$table, SDE_knn_none_cm$table, SDE_knn_up_cm$table), dim=c(2,2,3), dimnames = list(c("Prediction CO", "Prediction FP"), c("Reference CO", "Reference FP"), c("down", "none", "up")))
    fourfoldplot(cm_array, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, mfcol = c(1,3), std = "all.max")
  })
  
  output$SDE_knn_cm_down <- renderPrint(SDE_knn_down_cm)
  output$SDE_knn_cm_none <- renderPrint(SDE_knn_none_cm)
  output$SDE_knn_cm_up <- renderPrint(SDE_knn_up_cm)
  
  output$SDE_knn_results <- DT::renderDataTable({
    datatable(Results_list$KNN_SD_0, rownames = TRUE)
  })
  
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
  
  output$SL_RF_results <- renderPrint(Results_list[["RF"]])
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
  
  output$SL_AB_results <- renderPrint(Results_list[["AB"]])
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
             yaxis = list(title = "Lambda"),
             zaxis = list(title = "F-measure"))
  })
  
  output$EC_GLMNET_BOOT_results <- renderPrint(Results_list[["GLMNET_BOOT"]])
  output$EC_GLMNET_BOOT_infos_down <- renderPrint(models_list[["GLMNET_BOOT"]][["down"]])
  output$EC_GLMNET_BOOT_infos_none <- renderPrint(models_list[["GLMNET_BOOT"]][["none"]])
  output$EC_GLMNET_BOOT_infos_up <- renderPrint(models_list[["GLMNET_BOOT"]][["up"]])
  #output$EC_GLMNET_BOOT_confusion_matrix_down <- renderPrint(models_list[["GLMNET_BOOT"]][["down"]])
  #output$EC_GLMNET_BOOT_confusion_matrix_none <- renderPrint(models_list[["GLMNET_BOOT"]][["none"]])
  #output$EC_GLMNET_BOOT_confusion_matrix_up <- renderPrint(models_list[["GLMNET_BOOT"]][["up"]])
  
  
  output$EC_f_measure_GLMNET_CV <- renderPlotly({
    plt <- plot_ly(mode = "lines", type = 'scatter3d') %>%
      add_trace(data = models_list[["GLMNET_CV"]][["none"]]$results, x = ~alpha, y = ~lambda, z = ~F, line = list(color = c("green")), name = "none") %>%
      add_trace(data = models_list[["GLMNET_CV"]][["up"]]$results, x = ~alpha, y = ~lambda, z = ~F, line = list(color = c("red")), name = "up") %>%
      add_trace(data = models_list[["GLMNET_CV"]][["down"]]$results, x = ~alpha, y = ~lambda, z = ~F, line = list(color = c("blue")), name = "down") %>%
      layout(xaxis = list(title = "Alpha", tickangle = -45),
             yaxis = list(title = "Lambda"),
             zaxis = list(title = "F-measure"))
  })
  
  
  output$EC_GLMNET_CV_results <- renderPrint(Results_list[["GLMNET_CV"]])
  output$EC_GLMNET_CV_infos_down <- renderPrint(models_list[["GLMNET_CV"]][["down"]])
  output$EC_GLMNET_CV_infos_none <- renderPrint(models_list[["GLMNET_CV"]][["none"]])
  output$EC_GLMNET_CV_infos_up <- renderPrint(models_list[["GLMNET_CV"]][["up"]])
  #output$EC_GLMNET_CV_confusion_matrix_down <- renderPrint(models_list[["GLMNET_CV"]][["down"]])
  #output$EC_GLMNET_CV_confusion_matrix_none <- renderPrint(models_list[["GLMNET_CV"]][["none"]])
  #output$EC_GLMNET_CV_confusion_matrix_up <- renderPrint(models_list[["GLMNET_CV"]][["up"]])
  
  
  output$EC_f_measure_KNN_EC_01 <- renderPlotly({
    plt <- plot_ly(mode = "lines", type = 'scatter') %>%
      add_trace(data = models_list[["KNN_EC_01"]][["none"]]$results, x = ~k, y = ~ROC, line = list(color = c("green")), name = "none") %>%
      add_trace(data = models_list[["KNN_EC_01"]][["up"]]$results, x = ~k, y = ~ROC, line = list(color = c("red")), name = "up") %>%
      add_trace(data = models_list[["KNN_EC_01"]][["down"]]$results, x = ~k, y = ~ROC, line = list(color = c("blue")), name = "down") %>%
      layout(xaxis = list(title = "K", tickangle = -45),
             yaxis = list(title = "ROC"))
  })
  
  
  output$EC_KNN_01_results <- renderPrint(Results_list[["KNN_EC_01"]])
  output$EC_KNN_01_infos_down <- renderPrint(models_list[["KNN_EC_01"]][["down"]])
  output$EC_KNN_01_infos_none <- renderPrint(models_list[["KNN_EC_01"]][["none"]])
  output$EC_KNN_01_infos_up <- renderPrint(models_list[["KNN_EC_01"]][["up"]])
  #output$EC_KNN_01_confusion_matrix_down <- renderPrint(models_list[["KNN_EC_01"]][["down"]])
  #output$EC_KNN_01_confusion_matrix_none <- renderPrint(models_list[["KNN_EC_01"]][["none"]])
  #output$EC_KNN_01_confusion_matrix_up <- renderPrint(models_list[["KNN_EC_01"]][["up"]])
  
  
  output$EC_f_measure_KNN_EC_02 <- renderPlotly({
    plt <- plot_ly(mode = "lines", type = 'scatter') %>%
      add_trace(data = models_list[["KNN_EC_02"]][["none"]]$results, x = ~k, y = ~ROC, line = list(color = c("green")), name = "none") %>%
      add_trace(data = models_list[["KNN_EC_02"]][["up"]]$results, x = ~k, y = ~ROC, line = list(color = c("red")), name = "up") %>%
      add_trace(data = models_list[["KNN_EC_02"]][["down"]]$results, x = ~k, y = ~ROC, line = list(color = c("blue")), name = "down") %>%
      layout(xaxis = list(title = "K", tickangle = -45),
             yaxis = list(title = "ROC"))
  })
  
  
  output$EC_KNN_02_results <- renderPrint(Results_list[["KNN_EC_02"]])
  output$EC_KNN_02_infos_down <- renderPrint(models_list[["KNN_EC_02"]][["down"]])
  output$EC_KNN_02_infos_none <- renderPrint(models_list[["KNN_EC_02"]][["none"]])
  output$EC_KNN_02_infos_up <- renderPrint(models_list[["KNN_EC_02"]][["up"]])
  #output$EC_KNN_02_confusion_matrix_down <- renderPrint(models_list[["KNN_EC_02"]][["down"]])
  #output$EC_KNN_02_confusion_matrix_none <- renderPrint(models_list[["KNN_EC_02"]][["none"]])
  #output$EC_KNN_02_confusion_matrix_up <- renderPrint(models_list[["KNN_EC_02"]][["up"]])
  
  
  output$EC_f_measure_EC_GBM_01 <- renderPlotly({
    plt <- plot_ly(mode = "lines", type = 'scatter3d') %>%
      add_trace(data = models_list[["GBM_01"]][["none"]]$results, x = ~n.trees, y = ~interaction.depth, z = ~F, line = list(color = c("green")), name = "none") %>%
      add_trace(data = models_list[["GBM_01"]][["up"]]$results, x = ~n.trees, y = ~interaction.depth, z = ~F, line = list(color = c("red")), name = "up") %>%
      add_trace(data = models_list[["GBM_01"]][["down"]]$results, x = ~n.trees, y = ~interaction.depth, z = ~F, line = list(color = c("blue")), name = "down") %>%
      layout(xaxis = list(title = "n.trees", tickangle = -45),
             yaxis = list(title = "interaction.depth"),
             zaxis = list(title = "F"))
  })
  
  
  output$EC_GBM_01_results <- renderPrint(Results_list[["GBM_01"]])
  output$EC_GBM_01_infos_down <- renderPrint(models_list[["GBM_01"]][["down"]])
  output$EC_GBM_01_infos_none <- renderPrint(models_list[["GBM_01"]][["none"]])
  output$EC_GBM_01_infos_up <- renderPrint(models_list[["GBM_01"]][["up"]])
  #output$EC_GBM_01_confusion_matrix_down <- renderPrint(models_list[["GBM_01"]][["down"]])
  #output$EC_GBM_01_confusion_matrix_none <- renderPrint(models_list[["GBM_01"]][["none"]])
  #output$EC_GBM_01_confusion_matrix_up <- renderPrint(models_list[["GBM_01"]][["up"]])
  
  ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
  ############################################# EC ##############################################
  
  
} # end of: server <- function(input, output)

shinyApp(ui = ui, server = server)

