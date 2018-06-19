library(shiny)
library(plotly)
library(DT)
library(gbm)
library(caret)
library(pROC)

dirpath2 <- "C:/Users/sebde/OneDrive/Documents/cepe-2018-kickclub/Shiny"

models_list = list()
results_list <- list()
cm_list <- list()
var_imp_list <- list()
roc_list <- list()

loan <- readRDS(paste0(dirpath2,"./loan.RDS"))
cl <- sapply(loan, class)
fact <- sort(colnames(loan)[which(cl == "factor")])
num <- sort(colnames(loan)[which(cl %in% c("integer", "numeric"))])

set.seed(17)
train.index <- createDataPartition(loan$loan_status, p = .66, list = FALSE, times = 1)

loan.test <- as.data.frame(loan[-train.index,])

############################################ SDE ##############################################
##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##

feed_roc_list <- function(x){
  my.cutoff <- seq(0.1,0.9,0.05)
  coords_details <- round(coords(x, c(-Inf, sort(my.cutoff), Inf), input = "threshold", ret = c("specificity", "sensitivity")),2)
  return(coords_details)
}


KNN_0_models <-  list("none" = readRDS(paste0(dirpath2,"./model/SDE_knn_none_k 1 - 200_n 2527.RDS")),
                      "up" = readRDS(paste0(dirpath2,"./model/SDE_knn_up_k 1 - 200_n 2527.RDS")),
                      "down" = readRDS(paste0(dirpath2,"./model/SDE_knn_down_k 1 - 200_n 2527.RDS")))

models_list[["KNN_00"]] <- KNN_0_models

results_list[["KNN_00"]] <- readRDS("./model/SDE_knn_k 1 - 200_n 2527_results.RDS")

# KNN_0_cm <- list("none" = readRDS(paste0(dirpath2,"./model/SDE_knn_none_k 1 - 200_n 2527_cm.RDS")),
#                  "up" = readRDS(paste0(dirpath2,"./model/SDE_knn_up_k 1 - 200_n 2527_cm.RDS")),
#                  "down" = readRDS(paste0(dirpath2,"./model/SDE_knn_down_k 1 - 200_n 2527_cm.RDS")))
# 
# cm_list[["KNN_00"]] <- KNN_0_cm

roc_list[["KNN_00"]] <- readRDS(paste0(dirpath2,"./model/SDE_knn_k 1 - 200_n 2527_roc.RDS"))


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

results_list[["RF"]] <- RF
results_list[["AB"]] <- AB

roc_list[["AB"]] <- list("down" = feed_roc_list(readRDS(paste0(dirpath2,"/model/Graph/SL_Adaboost_01_down_roc_g.RDS"))),
                         "up" = feed_roc_list(readRDS(paste0(dirpath2,"/model/Graph/SL_Adaboost_01_up_roc_g.RDS"))),
                         "none" = feed_roc_list(readRDS(paste0(dirpath2,"/model/Graph/SL_Adaboost_01_none_roc_g.RDS"))))

roc_list[["RF"]] <- list("down" = feed_roc_list(readRDS(paste0(dirpath2,"/model/Graph/SL_RF_01_down_roc_g.RDS"))),
                         "up" = feed_roc_list(readRDS(paste0(dirpath2,"/model/Graph/SL_RF_01_up_roc_g.RDS"))),
                         "none" = feed_roc_list(readRDS(paste0(dirpath2,"/model/Graph/SL_RF_01_none_roc_g.RDS"))))


##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
############################################# SL ##############################################


############################################ EC ##############################################
##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##

# results_list <- list()  # deplace en haut pour permettre inclusion AB et RF

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
KNN_01_models <- list("none" = readRDS(paste0(dirpath2,"/model/EC_KNN_01_none.mod")),
                      "up" = readRDS(paste0(dirpath2,"/model/EC_KNN_01_up.mod")),
                      "down" = readRDS(paste0(dirpath2,"/model/EC_KNN_01_down.mod")))

KNN_01 <- readRDS(paste0(dirpath2,"/model/EC_KNN_01_results.mod"))

#KNN sans eclamtement des modalites et sans sclaing: Specific part
KNN_02_models <- list("none" = readRDS(paste0(dirpath2,"/model/EC_KNN_02_none.mod")),
                      "up" = readRDS(paste0(dirpath2,"/model/EC_KNN_02_up.mod")),
                      "down" = readRDS(paste0(dirpath2,"/model/EC_KNN_02_down.mod")))

KNN_02 <- readRDS(paste0(dirpath2,"/model/EC_KNN_02_results.mod"))

#KNN sans eclamtement des modalites et sans sclaing: Specific part
GBM_01_models <- list("none" = readRDS(paste0(dirpath2,"/model/EC_GBM_01_none.mod")),
                      "up" = readRDS(paste0(dirpath2,"/model/EC_GBM_01_up.mod")),
                      "down" = readRDS(paste0(dirpath2,"/model/EC_GBM_01_down.mod")))

GBM_01 <- readRDS(paste0(dirpath2,"/model/EC_GBM_01_results.mod"))

#Merge models
models_list[["GLMNET_BOOT"]] <- GLMNET_BOOT_models
models_list[["GLMNET_CV"]] <- GLMNET_CV_models
models_list[["KNN_01"]] <- KNN_01_models
models_list[["KNN_02"]] <- KNN_02_models
models_list[["GBM_01"]] <- GBM_01_models

#Merge models' results
results_list[["GLMNET_BOOT"]] <- GLMNET_BOOT
results_list[["GLMNET_CV"]] <- GLMNET_CV
results_list[["KNN_01"]] <- KNN_01
results_list[["KNN_02"]] <- KNN_02
results_list[["GBM_01"]] <- GBM_01

roc_list[["GLMNET_BOOT"]] <- list("down" = feed_roc_list(readRDS(paste0(dirpath2,"/model/Graph/EC_GLMNET_BOOT_01_down_roc_g.RDS"))),
                                  "up" = feed_roc_list(readRDS(paste0(dirpath2,"/model/Graph/EC_GLMNET_BOOT_01_up_roc_g.RDS"))),
                                  "none" = feed_roc_list(readRDS(paste0(dirpath2,"/model/Graph/EC_GLMNET_BOOT_01_none_roc_g.RDS"))))

roc_list[["GLMNET_CV"]] <- list("down" = feed_roc_list(readRDS(paste0(dirpath2,"/model/Graph/EC_GLMNET_CV_01_down_roc_g.RDS"))),
                                "up" = feed_roc_list(readRDS(paste0(dirpath2,"/model/Graph/EC_GLMNET_CV_01_up_roc_g.RDS"))),
                                "none" = feed_roc_list(readRDS(paste0(dirpath2,"/model/Graph/EC_GLMNET_CV_01_none_roc_g.RDS"))))

roc_list[["KNN_01"]] <- list("down" = feed_roc_list(readRDS(paste0(dirpath2,"/model/Graph/EC_KNN_01_down_roc_g.RDS"))),
                             "up" = feed_roc_list(readRDS(paste0(dirpath2,"/model/Graph/EC_KNN_01_up_roc_g.RDS"))),
                             "none" = feed_roc_list(readRDS(paste0(dirpath2,"/model/Graph/EC_KNN_01_none_roc_g.RDS"))))

roc_list[["KNN_02"]] <- list("down" = feed_roc_list(readRDS(paste0(dirpath2,"/model/Graph/EC_KNN_02_down_roc_g.RDS"))),
                             "up" = feed_roc_list(readRDS(paste0(dirpath2,"/model/Graph/EC_KNN_02_up_roc_g.RDS"))),
                             "none" = feed_roc_list(readRDS(paste0(dirpath2,"/model/Graph/EC_KNN_02_none_roc_g.RDS"))))

roc_list[["GBM_01"]] <- list("down" = feed_roc_list(readRDS(paste0(dirpath2,"/model/Graph/EC_GBM_01_down_roc_g.RDS"))),
                             "up" = feed_roc_list(readRDS(paste0(dirpath2,"/model/Graph/EC_GBM_01_up_roc_g.RDS"))),
                             "none" = feed_roc_list(readRDS(paste0(dirpath2,"/model/Graph/EC_GBM_01_none_roc_g.RDS"))))


##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
############################################# EC ##############################################

# calcul de var_imp_list
for(mod.name in names(models_list)){
  
  
  for(sm in c("none", "up", "down")){
    temp <- varImp(models_list[[mod.name]][[sm]])
    temp <- temp$importance[1]
    colnames(temp) <- c("importance")
    temp$variable <- rownames(temp)
    
    temp <- temp[order(temp["importance"], decreasing = TRUE),]
    var_imp_list[[mod.name]][[sm]] <- temp
  }
}

for(mod.name in names(models_list)){
  for(sampling.method in c("none", "up", "down")){
    if(mod.name == "KNN_00"){
      loan.test.mm <- data.frame(cbind(loan.test[, "loan_status"], as.data.frame(model.matrix(loan_status~. -1, data = loan.test))))
      prediction <- predict(object = models_list[[mod.name]][[sampling.method]], loan.test.mm, type = "raw") 
    } else {
      prediction <- predict(object = models_list[[mod.name]][[sampling.method]], loan.test, type = "raw") 
      # Error when using: 'object = train_results[[samp_]]$finalModel': "no applicable method for 'predict' applied to an object of class "LogitBoost" "
    }
    conf_matrix <- confusionMatrix(prediction, loan.test$loan_status)
    
    cm_list[[mod.name]][[sampling.method]] <- conf_matrix
  }
}

all_measure <- data.frame()

for(mod.name in names(results_list)){
  for(sk in c("down", "none", "up")){
    df <- data.frame()
    df <- as.data.frame(results_list[[mod.name]][,sk])
    colnames(df) <- "measure"
    df["measure_name"] <- rownames(df)
    df["sampling_kind"] <- sk
    df["modele"] <- mod.name
    
    all_measure <- rbind(all_measure, df)
  }
}

all_measure$prediction <- ""

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
    
    ############################################ VARIABLES ##############################################
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
    
    ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
    ############################################# VARIABLES ##############################################
    
    ############################################ GENERAL ##############################################
    ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##
    
    tabPanel("General",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("measure_name", label = "Criteria", choices = c("Accuracy",
                                                                              "Kappa",
                                                                              "F-measure",
                                                                              "Sensitivity",
                                                                              "Specificity",
                                                                              "Precision / posPredValue",
                                                                              "negPredValue",
                                                                              "AUC")),
                 
                 fluidRow(
                   column(4,
                          actionButton("generate_sample", "Generate Sample")
                   ),
                   column(4,
                          selectInput("selected_mod", label = NULL, choices = c("CO", "FP"))
                   ),
                   column(4,
                          textOutput("sample")
                   )
                 ),
                 fluidRow(
                   column(6,
                          tableOutput("vote_result")
                   ),
                   column(6,
                          h2(textOutput("vote"))
                   )
                 ),
                 tableOutput("ind_details")
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 dataTableOutput("best_model_table")
               )
             )
             
             
             
    ),
    
    ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
    ############################################# GENERAL ##############################################
    
    ############################################ KNN_00 ##############################################
    ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##
    tabPanel("KNN_00",
             fluidRow(
               column(8,
                      h2("F-measure"),
                      plotlyOutput("KNN_00_f_measure")
               ),
               column(4,
                      h2("ROC"),
                      plotlyOutput("KNN_00_roc")
               )
             ),
             fluidRow(
               column(4,
                      h2("Analytics"),
                      dataTableOutput("KNN_00_results"),
                      h2("Variable Importance"),
                      plotlyOutput("KNN_00_var_imp")
               ),
               column(8,
                      h2("Confusion Matrix"),
                      plotOutput("KNN_00_cm"),
                      fluidRow(
                        h2("Model details"),
                        column(4,
                               verbatimTextOutput("KNN_00_down_output")
                        ),
                        column(4,
                               verbatimTextOutput("KNN_00_none_output")
                        ),
                        column(4,
                               verbatimTextOutput("KNN_00_up_output")
                        )
                      )
               )
             )
             
             
             
    ), # end tabPanel("KNN 0")
    
    ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
    ############################################# KNN_00 ##############################################
    
    ############################################ KNN_01 ##############################################
    ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##
    tabPanel("KNN_01",
             fluidRow(
               column(8,
                      h2("F-measure"),
                      plotlyOutput("KNN_01_f_measure")
               ),
               column(4,
                      h2("ROC"),
                      plotlyOutput("KNN_01_roc")
               )
             ),
             fluidRow(
               column(4,
                      h2("Analytics"),
                      dataTableOutput("KNN_01_results"),
                      h2("Variable Importance"),
                      plotlyOutput("KNN_01_var_imp")
               ),
               column(8,
                      h2("Confusion Matrix"),
                      plotOutput("KNN_01_cm"),
                      fluidRow(
                        h2("Model details"),
                        column(4,
                               verbatimTextOutput("KNN_01_down_output")
                        ),
                        column(4,
                               verbatimTextOutput("KNN_01_none_output")
                        ),
                        column(4,
                               verbatimTextOutput("KNN_01_up_output")
                        )
                      )
               )
             )
             
             
    ),
    ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
    ############################################# KNN_01 ##############################################
    
    ############################################ KNN_02 ##############################################
    ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##
    tabPanel("KNN_02",
             fluidRow(
               column(8,
                      h2("F-measure"),
                      plotlyOutput("KNN_02_f_measure")
               ),
               column(4,
                      h2("ROC"),
                      plotlyOutput("KNN_02_roc")
               )
             ),
             fluidRow(
               column(4,
                      h2("Analytics"),
                      dataTableOutput("KNN_02_results"),
                      h2("Variable Importance"),
                      plotlyOutput("KNN_02_var_imp")
               ),
               column(8,
                      h2("Confusion Matrix"),
                      plotOutput("KNN_02_cm"),
                      fluidRow(
                        h2("Model details"),
                        column(4,
                               verbatimTextOutput("KNN_02_down_output")
                        ),
                        column(4,
                               verbatimTextOutput("KNN_02_none_output")
                        ),
                        column(4,
                               verbatimTextOutput("KNN_02_up_output")
                        )
                      )
               )
             )
             
             
    ),
    ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
    ############################################# KNN_02 ##############################################
    
    ############################################ AB ##############################################
    ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##
    tabPanel("AB",
             fluidRow(
               column(8,
                      h2("F-measure"),
                      plotlyOutput("AB_f_measure")
               ),
               column(4,
                      h2("ROC"),
                      plotlyOutput("AB_roc")
               )
             ),
             fluidRow(
               column(4,
                      h2("Analytics"),
                      dataTableOutput("AB_results"),
                      h2("Variable Importance"),
                      plotlyOutput("AB_var_imp")
               ),
               column(8,
                      h2("Confusion Matrix"),
                      plotOutput("AB_cm"),
                      fluidRow(
                        h2("Model details"),
                        column(4,
                               verbatimTextOutput("AB_down_output")
                        ),
                        column(4,
                               verbatimTextOutput("AB_none_output")
                        ),
                        column(4,
                               verbatimTextOutput("AB_up_output")
                        )
                      )
               )
             )
             
             
    ),
    ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
    ############################################# AB ##############################################
    
    ############################################ RF ##############################################
    ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##
    tabPanel("RF",
             fluidRow(
               column(8,
                      h2("F-measure"),
                      plotlyOutput("RF_f_measure")
               ),
               column(4,
                      h2("ROC"),
                      plotlyOutput("RF_roc")
               )
             ),
             fluidRow(
               column(4,
                      h2("Analytics"),
                      dataTableOutput("RF_results"),
                      h2("Variable Importance"),
                      plotlyOutput("RF_var_imp")
               ),
               column(8,
                      h2("Confusion Matrix"),
                      plotOutput("RF_cm"),
                      fluidRow(
                        h2("Model details"),
                        column(4,
                               verbatimTextOutput("RF_down_output")
                        ),
                        column(4,
                               verbatimTextOutput("RF_none_output")
                        ),
                        column(4,
                               verbatimTextOutput("RF_up_output")
                        )
                      )
               )
             )
             
             
    ),
    ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
    ############################################# RF ##############################################
    
    ############################################ GLMNET_BOOT ##############################################
    ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##
    tabPanel("GLMNET_BOOT",
             fluidRow(
               column(8,
                      h2("F-measure"),
                      plotlyOutput("GLMNET_BOOT_f_measure")
               ),
               column(4,
                      h2("ROC"),
                      plotlyOutput("GLMNET_BOOT_roc")
               )
             ),
             fluidRow(
               column(4,
                      h2("Analytics"),
                      dataTableOutput("GLMNET_BOOT_results"),
                      h2("Variable Importance"),
                      plotlyOutput("GLMNET_BOOT_var_imp")
               ),
               column(8,
                      h2("Confusion Matrix"),
                      plotOutput("GLMNET_BOOT_cm"),
                      fluidRow(
                        h2("Model details"),
                        column(4,
                               verbatimTextOutput("GLMNET_BOOT_down_output")
                        ),
                        column(4,
                               verbatimTextOutput("GLMNET_BOOT_none_output")
                        ),
                        column(4,
                               verbatimTextOutput("GLMNET_BOOT_up_output")
                        )
                      )
               )
             )
             
             
    ),
    ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
    ############################################# GLMNET_BOOT ##############################################
    
    ############################################ GLMNET_CV ##############################################
    ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##
    tabPanel("GLMNET_CV",
             fluidRow(
               column(8,
                      h2("F-measure"),
                      plotlyOutput("GLMNET_CV_f_measure")
               ),
               column(4,
                      h2("ROC"),
                      plotlyOutput("GLMNET_CV_roc")
               )
             ),
             fluidRow(
               column(4,
                      h2("Analytics"),
                      dataTableOutput("GLMNET_CV_results"),
                      h2("Variable Importance"),
                      plotlyOutput("GLMNET_CV_var_imp")
               ),
               column(8,
                      h2("Confusion Matrix"),
                      plotOutput("GLMNET_CV_cm"),
                      fluidRow(
                        h2("Model details"),
                        column(4,
                               verbatimTextOutput("GLMNET_CV_down_output")
                        ),
                        column(4,
                               verbatimTextOutput("GLMNET_CV_none_output")
                        ),
                        column(4,
                               verbatimTextOutput("GLMNET_CV_up_output")
                        )
                      )
               )
             )
             
             
    ),
    ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
    ############################################# GLMNET_CV ##############################################
    
    ############################################ GBM_01 ##############################################
    ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##
    tabPanel("GBM_01",
             fluidRow(
               column(8,
                      h2("F-measure"),
                      plotlyOutput("GBM_01_f_measure")
               ),
               column(4,
                      h2("ROC"),
                      plotlyOutput("GBM_01_roc")
               )
             ),
             fluidRow(
               column(4,
                      h2("Analytics"),
                      dataTableOutput("GBM_01_results"),
                      h2("Variable Importance"),
                      plotlyOutput("GBM_01_var_imp")
               ),
               column(8,
                      h2("Confusion Matrix"),
                      plotOutput("GBM_01_cm"),
                      fluidRow(
                        h2("Model details"),
                        column(4,
                               verbatimTextOutput("GBM_01_down_output")
                        ),
                        column(4,
                               verbatimTextOutput("GBM_01_none_output")
                        ),
                        column(4,
                               verbatimTextOutput("GBM_01_up_output")
                        )
                      )
               )
             )
             
             
    )
    ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
    ############################################# GBM_01 ##############################################
    
    
  )
)



####################### SERVER ####################################################################

### Define server logic required to draw a histogram
server <- function(input, output) {
  
  ############################################ VARIABLE ##############################################
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
  
  ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
  ############################################# VARIABLE ##############################################
  
  
  ############################################ GENERAL ##############################################
  ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##
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
  
  observeEvent(input$generate_sample, {
    
    loan_mod <- loan[which(loan[, "loan_status"] == input$selected_mod),]
    r <- sample(rownames(loan_mod), size = 1)
    
    output$sample <- renderText({
      as.character(r)
    })
    
    output$sample_prediction <- renderText({
      as.character(loan[r, "loan_status"])
    })
    
    output$best_model_table <- DT::renderDataTable({
      # calcul des pred
      pred <- list()
      for(mod.name in names(models_list)){
        if(mod.name == "KNN_00"){
          loan_mm <- data.frame(cbind(loan[r, "loan_status"], as.data.frame(model.matrix(loan_status~. -1, data = loan[r,]))))
          pred[[mod.name]] <- predict(object = models_list[[mod.name]], newdata = loan_mm[r,], type = "raw")
        } else{
          pred[[mod.name]] <- predict(object = models_list[[mod.name]], loan[r,], type = "raw")
        }
        
      }
      for(mod.name in names(pred)){
        for(sampling.kind in names(pred[[mod.name]])){
          all_measure[which(all_measure[,"modele"] == mod.name & all_measure[,"sampling_kind"] == sampling.kind), "prediction"] <- as.character(pred[[mod.name]][[sampling.kind]])
        }
      }
      
      method <- TRUE
      if(measure_method[input$measure_name, "method"] == "min"){
        method <- FALSE
      }
      
      df_f <- na.omit(all_measure[all_measure[,"measure_name"] == input$measure_name,])
      row_order <- order(df_f$measure, decreasing = method)
      df_f_o <- df_f[row_order,]
      
      output$vote_result <- renderTable({
        vote_table <- as.data.frame(table(as.factor(all_measure$prediction))/length(measures))
        names(vote_table) <- c("Modality", "Frequency")
        
        output$vote <- renderText({
          row_ordered <- order(vote_table[,"Frequency"], decreasing = TRUE)
          as.character(vote_table[row_ordered[1], "Modality"])
        })
        
        
        vote_table
      })
      
      output$ind_details <- renderTable(rownames = TRUE,
                                        {
                                          t(loan[r,])
                                        })
      
      output$vote <- renderText({
        vote_table <- as.data.frame(table(as.factor(all_measure$prediction))/length(measures))
        names(vote_table) <- c("Modality", "Frequency")
        row_ordered <- order(vote_table[,"Frequency"], decreasing = TRUE)
        
        vote_table[row_order[1], "Modality"]
      })
      
      
      
      
      
      datatable(df_f_o, rownames = FALSE, selection = list(selected = c(1), target = 'row'))
      
    })
    
  })
  
  ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
  ############################################# GENERAL ##############################################
  
  
  ############################################ KNN_00 ##############################################
  ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##
  
  ###### tab KNN #####
  output$KNN_00_f_measure <- renderPlotly({
    plot_ly(mode = "lines", type = 'scatter') %>%
      add_trace(data = models_list$KNN_00$none$results, x = ~k, y = ~F, line = list(color = c("green")), name = "none") %>%
      add_trace(data = models_list$KNN_00$up$results, x = ~k, y = ~F, line = list(color = c("red")), name = "up") %>%
      add_trace(data = models_list$KNN_00$down$results, x = ~k, y = ~F, line = list(color = c("blue")), name = "down") %>%
      layout(xaxis = list(title = "K", tickangle = -45),
             yaxis = list(title = "F-measure"))
  })
  
  output$KNN_00_var_imp <- renderPlotly({
    plot_ly(type = 'bar') %>%
      add_trace(data = var_imp_list[["KNN_00"]][["none"]], y = ~variable, x = ~importance, marker = list(color = c("green")), name = "none") %>%
      add_trace(data = var_imp_list[["KNN_00"]][["up"]], y = ~variable, x = ~importance, marker = list(color = c("red")), name = "up") %>%
      add_trace(data = var_imp_list[["KNN_00"]][["down"]], y = ~variable, x = ~importance, marker = list(color = c("blue")), name = "down") %>%
      layout(yaxis = list(autorange = "reversed"), margin = list(l = 200, r = 50, b = 50, t = 50, pad = 4))
  })
  
  output$KNN_00_roc <- renderPlotly({
    plot_ly(mode = "lines", type = 'scatter') %>%
      add_trace(data = as.data.frame(t(roc_list$KNN_00$none)), x = ~specificity, y = ~sensitivity, line = list(color = c("green")), name = "none") %>%
      add_trace(data = as.data.frame(t(roc_list$KNN_00$up)), x = ~specificity, y = ~sensitivity, line = list(color = c("red")), name = "up") %>%
      add_trace(data = as.data.frame(t(roc_list$KNN_00$down)), x = ~specificity, y = ~sensitivity, line = list(color = c("blue")), name = "down") %>%
      layout(xaxis = list(title = "Specificity", tickangle = -45, autorange = "reversed"),
             yaxis = list(title = "Sensitivity"))
  })
  
  output$KNN_00_cm <- renderPlot({
    cm_array <- array(c(cm_list$KNN_00$down$table, cm_list$KNN_00$none$table, cm_list$KNN_00$up$table), dim=c(2,2,3), dimnames = list(c("Prediction CO", "Prediction FP"), c("Reference CO", "Reference FP"), c("down", "none", "up")))
    fourfoldplot(cm_array, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, mfcol = c(1,3), std = "all.max")
  })
  
  output$KNN_00_down_output <- renderPrint(cm_list$KNN_00$down)
  output$KNN_00_none_output <- renderPrint(cm_list$KNN_00$none)
  output$KNN_00_up_output <- renderPrint(cm_list$KNN_00$up)
  
  output$KNN_00_results <- DT::renderDataTable({
    datatable(results_list$KNN_00, rownames = TRUE)
  })
  
  ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
  ############################################# KNN_00 ##############################################
  
  ############################################ KNN_01 ##############################################
  ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##
  output$KNN_01_f_measure <- renderPlotly({
    plot_ly(mode = "lines", type = 'scatter') %>%
      add_trace(data = models_list$KNN_01$none$results, x = ~k, y = ~F, line = list(color = c("green")), name = "none") %>%
      add_trace(data = models_list$KNN_01$up$results, x = ~k, y = ~F, line = list(color = c("red")), name = "up") %>%
      add_trace(data = models_list$KNN_01$down$results, x = ~k, y = ~F, line = list(color = c("blue")), name = "down") %>%
      layout(xaxis = list(title = "K", tickangle = -45),
             yaxis = list(title = "F-measure"))
  })
  
  output$KNN_01_var_imp <- renderPlotly({
    plot_ly(type = 'bar') %>%
      add_trace(data = var_imp_list[["KNN_01"]][["none"]], y = ~variable, x = ~importance, marker = list(color = c("green")), name = "none") %>%
      add_trace(data = var_imp_list[["KNN_01"]][["up"]], y = ~variable, x = ~importance, marker = list(color = c("red")), name = "up") %>%
      add_trace(data = var_imp_list[["KNN_01"]][["down"]], y = ~variable, x = ~importance, marker = list(color = c("blue")), name = "down") %>%
      layout(yaxis = list(autorange = "reversed"), margin = list(l = 200, r = 50, b = 50, t = 50, pad = 4))
  })
  
  output$KNN_01_roc <- renderPlotly({
    plot_ly(mode = "lines", type = 'scatter') %>%
      add_trace(data = as.data.frame(t(roc_list$KNN_01$none)), x = ~specificity, y = ~sensitivity, line = list(color = c("green")), name = "none") %>%
      add_trace(data = as.data.frame(t(roc_list$KNN_01$up)), x = ~specificity, y = ~sensitivity, line = list(color = c("red")), name = "up") %>%
      add_trace(data = as.data.frame(t(roc_list$KNN_01$down)), x = ~specificity, y = ~sensitivity, line = list(color = c("blue")), name = "down") %>%
      layout(xaxis = list(title = "Specificity", tickangle = -45, autorange = "reversed"),
             yaxis = list(title = "Sensitivity"))
  })
  
  output$KNN_01_cm <- renderPlot({
    cm_array <- array(c(cm_list$KNN_01$down$table, cm_list$KNN_01$none$table, cm_list$KNN_01$up$table), dim=c(2,2,3), dimnames = list(c("Prediction CO", "Prediction FP"), c("Reference CO", "Reference FP"), c("down", "none", "up")))
    fourfoldplot(cm_array, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, mfcol = c(1,3), std = "all.max")
  })
  
  output$KNN_01_down_output <- renderPrint(cm_list$KNN_01$down)
  output$KNN_01_none_output <- renderPrint(cm_list$KNN_01$none)
  output$KNN_01_up_output <- renderPrint(cm_list$KNN_01$up)
  
  output$KNN_01_results <- DT::renderDataTable({
    datatable(results_list$KNN_01, rownames = TRUE)
  })  
  ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
  ############################################# KNN_01 ##############################################
  
  ############################################ KNN_02 ##############################################
  ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##
  output$KNN_02_f_measure <- renderPlotly({
    plot_ly(mode = "lines", type = 'scatter') %>%
      add_trace(data = models_list$KNN_02$none$results, x = ~k, y = ~F, line = list(color = c("green")), name = "none") %>%
      add_trace(data = models_list$KNN_02$up$results, x = ~k, y = ~F, line = list(color = c("red")), name = "up") %>%
      add_trace(data = models_list$KNN_02$down$results, x = ~k, y = ~F, line = list(color = c("blue")), name = "down") %>%
      layout(xaxis = list(title = "K", tickangle = -45),
             yaxis = list(title = "F-measure"))
  })
  
  output$KNN_02_var_imp <- renderPlotly({
    plot_ly(type = 'bar') %>%
      add_trace(data = var_imp_list[["KNN_02"]][["none"]], y = ~variable, x = ~importance, marker = list(color = c("green")), name = "none") %>%
      add_trace(data = var_imp_list[["KNN_02"]][["up"]], y = ~variable, x = ~importance, marker = list(color = c("red")), name = "up") %>%
      add_trace(data = var_imp_list[["KNN_02"]][["down"]], y = ~variable, x = ~importance, marker = list(color = c("blue")), name = "down") %>%
      layout(yaxis = list(autorange = "reversed"), margin = list(l = 200, r = 50, b = 50, t = 50, pad = 4))
  })
  
  output$KNN_02_roc <- renderPlotly({
    plot_ly(mode = "lines", type = 'scatter') %>%
      add_trace(data = as.data.frame(t(roc_list$KNN_02$none)), x = ~specificity, y = ~sensitivity, line = list(color = c("green")), name = "none") %>%
      add_trace(data = as.data.frame(t(roc_list$KNN_02$up)), x = ~specificity, y = ~sensitivity, line = list(color = c("red")), name = "up") %>%
      add_trace(data = as.data.frame(t(roc_list$KNN_02$down)), x = ~specificity, y = ~sensitivity, line = list(color = c("blue")), name = "down") %>%
      layout(xaxis = list(title = "Specificity", tickangle = -45, autorange = "reversed"),
             yaxis = list(title = "Sensitivity"))
  })
  
  output$KNN_02_cm <- renderPlot({
    cm_array <- array(c(cm_list$KNN_02$down$table, cm_list$KNN_02$none$table, cm_list$KNN_02$up$table), dim=c(2,2,3), dimnames = list(c("Prediction CO", "Prediction FP"), c("Reference CO", "Reference FP"), c("down", "none", "up")))
    fourfoldplot(cm_array, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, mfcol = c(1,3), std = "all.max")
  })
  
  output$KNN_02_down_output <- renderPrint(cm_list$KNN_02$down)
  output$KNN_02_none_output <- renderPrint(cm_list$KNN_02$none)
  output$KNN_02_up_output <- renderPrint(cm_list$KNN_02$up)
  
  output$KNN_02_results <- DT::renderDataTable({
    datatable(results_list$KNN_02, rownames = TRUE)
  })  
  ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
  ############################################# KNN_02 ##############################################
  
  ############################################ AB ##############################################
  ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##
  output$AB_f_measure <- renderPlotly({
    plot_ly(mode = "lines", type = 'scatter') %>%
      add_trace(data = models_list$AB$none$results, x = ~nIter, y = ~F, line = list(color = c("green")), name = "none") %>%
      add_trace(data = models_list$AB$up$results, x = ~nIter, y = ~F, line = list(color = c("red")), name = "up") %>%
      add_trace(data = models_list$AB$down$results, x = ~nIter, y = ~F, line = list(color = c("blue")), name = "down") %>%
      layout(xaxis = list(title = "nIter", tickangle = -45),
             yaxis = list(title = "F-measure"))
  })
  
  output$AB_var_imp <- renderPlotly({
    plot_ly(type = 'bar') %>%
      add_trace(data = var_imp_list[["AB"]][["none"]], y = ~variable, x = ~importance, marker = list(color = c("green")), name = "none") %>%
      add_trace(data = var_imp_list[["AB"]][["up"]], y = ~variable, x = ~importance, marker = list(color = c("red")), name = "up") %>%
      add_trace(data = var_imp_list[["AB"]][["down"]], y = ~variable, x = ~importance, marker = list(color = c("blue")), name = "down") %>%
      layout(yaxis = list(autorange = "reversed"), margin = list(l = 200, r = 50, b = 50, t = 50, pad = 4))
  })
  
  output$AB_roc <- renderPlotly({
    plot_ly(mode = "lines", type = 'scatter') %>%
      add_trace(data = as.data.frame(t(roc_list$AB$none)), x = ~specificity, y = ~sensitivity, line = list(color = c("green")), name = "none") %>%
      add_trace(data = as.data.frame(t(roc_list$AB$up)), x = ~specificity, y = ~sensitivity, line = list(color = c("red")), name = "up") %>%
      add_trace(data = as.data.frame(t(roc_list$AB$down)), x = ~specificity, y = ~sensitivity, line = list(color = c("blue")), name = "down") %>%
      layout(xaxis = list(title = "Specificity", tickangle = -45, autorange = "reversed"),
             yaxis = list(title = "Sensitivity"))
  })
  
  output$AB_cm <- renderPlot({
    cm_array <- array(c(cm_list$AB$down$table, cm_list$AB$none$table, cm_list$AB$up$table), dim=c(2,2,3), dimnames = list(c("Prediction CO", "Prediction FP"), c("Reference CO", "Reference FP"), c("down", "none", "up")))
    fourfoldplot(cm_array, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, mfcol = c(1,3), std = "all.max")
  })
  
  output$AB_down_output <- renderPrint(cm_list$AB$down)
  output$AB_none_output <- renderPrint(cm_list$AB$none)
  output$AB_up_output <- renderPrint(cm_list$AB$up)
  
  output$AB_results <- DT::renderDataTable({
    datatable(results_list$AB, rownames = TRUE)
  })   
  ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
  ############################################# AB ##############################################
  
  ############################################ RF ##############################################
  ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##
  output$RF_f_measure <- renderPlotly({
    plot_ly(mode = "lines", type = 'scatter') %>%
      add_trace(data = models_list$RF$none$results, x = ~mtry, y = ~F, line = list(color = c("green")), name = "none") %>%
      add_trace(data = models_list$RF$up$results, x = ~mtry, y = ~F, line = list(color = c("red")), name = "up") %>%
      add_trace(data = models_list$RF$down$results, x = ~mtry, y = ~F, line = list(color = c("blue")), name = "down") %>%
      layout(xaxis = list(title = "mtry", tickangle = -45),
             yaxis = list(title = "F-measure"))
  })
  
  output$RF_var_imp <- renderPlotly({
    plot_ly(type = 'bar') %>%
      add_trace(data = var_imp_list[["RF"]][["none"]], y = ~variable, x = ~importance, marker = list(color = c("green")), name = "none") %>%
      add_trace(data = var_imp_list[["RF"]][["up"]], y = ~variable, x = ~importance, marker = list(color = c("red")), name = "up") %>%
      add_trace(data = var_imp_list[["RF"]][["down"]], y = ~variable, x = ~importance, marker = list(color = c("blue")), name = "down") %>%
      layout(yaxis = list(autorange = "reversed"), margin = list(l = 200, r = 50, b = 50, t = 50, pad = 4))
  })
  
  output$RF_roc <- renderPlotly({
    plot_ly(mode = "lines", type = 'scatter') %>%
      add_trace(data = as.data.frame(t(roc_list$RF$none)), x = ~specificity, y = ~sensitivity, line = list(color = c("green")), name = "none") %>%
      add_trace(data = as.data.frame(t(roc_list$RF$up)), x = ~specificity, y = ~sensitivity, line = list(color = c("red")), name = "up") %>%
      add_trace(data = as.data.frame(t(roc_list$RF$down)), x = ~specificity, y = ~sensitivity, line = list(color = c("blue")), name = "down") %>%
      layout(xaxis = list(title = "Specificity", tickangle = -45, autorange = "reversed"),
             yaxis = list(title = "Sensitivity"))
  })
  
  output$RF_cm <- renderPlot({
    cm_array <- array(c(cm_list$RF$down$table, cm_list$RF$none$table, cm_list$RF$up$table), dim=c(2,2,3), dimnames = list(c("Prediction CO", "Prediction FP"), c("Reference CO", "Reference FP"), c("down", "none", "up")))
    fourfoldplot(cm_array, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, mfcol = c(1,3), std = "all.max")
  })
  
  output$RF_down_output <- renderPrint(cm_list$RF$down)
  output$RF_none_output <- renderPrint(cm_list$RF$none)
  output$RF_up_output <- renderPrint(cm_list$RF$up)
  
  output$RF_results <- DT::renderDataTable({
    datatable(results_list$RF, rownames = TRUE)
  })   
  ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
  ############################################# RF ##############################################
  
  ############################################ GLMNET_BOOT ##############################################
  ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##
  output$GLMNET_BOOT_f_measure <- renderPlotly({
    plot_ly(mode = "lines", type = 'scatter3d') %>%
      add_trace(data = models_list$GLMNET_BOOT$none$results, x = ~alpha, y = ~lambda, z = ~F, line = list(color = c("green")), name = "none") %>%
      add_trace(data = models_list$GLMNET_BOOT$up$results, x = ~alpha, y = ~lambda, z = ~F, line = list(color = c("red")), name = "up") %>%
      add_trace(data = models_list$GLMNET_BOOT$down$results, x = ~alpha, y = ~lambda, z = ~F, line = list(color = c("blue")), name = "down")
  })
  
  output$GLMNET_BOOT_var_imp <- renderPlotly({
    plot_ly(type = 'bar') %>%
      add_trace(data = var_imp_list[["GLMNET_BOOT"]][["none"]], y = ~variable, x = ~importance, marker = list(color = c("green")), name = "none") %>%
      add_trace(data = var_imp_list[["GLMNET_BOOT"]][["up"]], y = ~variable, x = ~importance, marker = list(color = c("red")), name = "up") %>%
      add_trace(data = var_imp_list[["GLMNET_BOOT"]][["down"]], y = ~variable, x = ~importance, marker = list(color = c("blue")), name = "down") %>%
      layout(yaxis = list(autorange = "reversed"), margin = list(l = 200, r = 50, b = 50, t = 50, pad = 4))
  })
  
  output$GLMNET_BOOT_roc <- renderPlotly({
    plot_ly(mode = "lines", type = 'scatter') %>%
      add_trace(data = as.data.frame(t(roc_list$GLMNET_BOOT$none)), x = ~specificity, y = ~sensitivity, line = list(color = c("green")), name = "none") %>%
      add_trace(data = as.data.frame(t(roc_list$GLMNET_BOOT$up)), x = ~specificity, y = ~sensitivity, line = list(color = c("red")), name = "up") %>%
      add_trace(data = as.data.frame(t(roc_list$GLMNET_BOOT$down)), x = ~specificity, y = ~sensitivity, line = list(color = c("blue")), name = "down") %>%
      layout(xaxis = list(title = "Specificity", tickangle = -45, autorange = "reversed"),
             yaxis = list(title = "Sensitivity"))
  })
  
  output$GLMNET_BOOT_cm <- renderPlot({
    cm_array <- array(c(cm_list$GLMNET_BOOT$down$table, cm_list$GLMNET_BOOT$none$table, cm_list$GLMNET_BOOT$up$table), dim=c(2,2,3), dimnames = list(c("Prediction CO", "Prediction FP"), c("Reference CO", "Reference FP"), c("down", "none", "up")))
    fourfoldplot(cm_array, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, mfcol = c(1,3), std = "all.max")
  })
  
  output$GLMNET_BOOT_down_output <- renderPrint(cm_list$GLMNET_BOOT$down)
  output$GLMNET_BOOT_none_output <- renderPrint(cm_list$GLMNET_BOOT$none)
  output$GLMNET_BOOT_up_output <- renderPrint(cm_list$GLMNET_BOOT$up)
  
  output$GLMNET_BOOT_results <- DT::renderDataTable({
    datatable(results_list$GLMNET_BOOT, rownames = TRUE)
  })   
  ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
  ############################################# GLMNET_BOOT ##############################################
  
  ############################################ GLMNET_CV ##############################################
  ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##
  output$GLMNET_CV_f_measure <- renderPlotly({
    plot_ly(mode = "lines", type = 'scatter3d') %>%
      add_trace(data = models_list$GLMNET_CV$none$results, x = ~alpha, y = ~lambda, z = ~F, line = list(color = c("green")), name = "none") %>%
      add_trace(data = models_list$GLMNET_CV$up$results, x = ~alpha, y = ~lambda, z = ~F, line = list(color = c("red")), name = "up") %>%
      add_trace(data = models_list$GLMNET_CV$down$results, x = ~alpha, y = ~lambda, z = ~F, line = list(color = c("blue")), name = "down")
  })
  
  output$GLMNET_CV_var_imp <- renderPlotly({
    plot_ly(type = 'bar') %>%
      add_trace(data = var_imp_list[["GLMNET_CV"]][["none"]], y = ~variable, x = ~importance, marker = list(color = c("green")), name = "none") %>%
      add_trace(data = var_imp_list[["GLMNET_CV"]][["up"]], y = ~variable, x = ~importance, marker = list(color = c("red")), name = "up") %>%
      add_trace(data = var_imp_list[["GLMNET_CV"]][["down"]], y = ~variable, x = ~importance, marker = list(color = c("blue")), name = "down") %>%
      layout(yaxis = list(autorange = "reversed"), margin = list(l = 200, r = 50, b = 50, t = 50, pad = 4))
  })
  
  output$GLMNET_CV_roc <- renderPlotly({
    plot_ly(mode = "lines", type = 'scatter') %>%
      add_trace(data = as.data.frame(t(roc_list$GLMNET_CV$none)), x = ~specificity, y = ~sensitivity, line = list(color = c("green")), name = "none") %>%
      add_trace(data = as.data.frame(t(roc_list$GLMNET_CV$up)), x = ~specificity, y = ~sensitivity, line = list(color = c("red")), name = "up") %>%
      add_trace(data = as.data.frame(t(roc_list$GLMNET_CV$down)), x = ~specificity, y = ~sensitivity, line = list(color = c("blue")), name = "down") %>%
      layout(xaxis = list(title = "Specificity", tickangle = -45, autorange = "reversed"),
             yaxis = list(title = "Sensitivity"))
  })
  
  output$GLMNET_CV_cm <- renderPlot({
    cm_array <- array(c(cm_list$GLMNET_CV$down$table, cm_list$GLMNET_CV$none$table, cm_list$GLMNET_CV$up$table), dim=c(2,2,3), dimnames = list(c("Prediction CO", "Prediction FP"), c("Reference CO", "Reference FP"), c("down", "none", "up")))
    fourfoldplot(cm_array, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, mfcol = c(1,3), std = "all.max")
  })
  
  output$GLMNET_CV_down_output <- renderPrint(cm_list$GLMNET_CV$down)
  output$GLMNET_CV_none_output <- renderPrint(cm_list$GLMNET_CV$none)
  output$GLMNET_CV_up_output <- renderPrint(cm_list$GLMNET_CV$up)
  
  output$GLMNET_CV_results <- DT::renderDataTable({
    datatable(results_list$GLMNET_CV, rownames = TRUE)
  })   
  ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
  ############################################# GLMNET_CV ##############################################
  
  ############################################ GBM_01 ##############################################
  ##VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv##
  output$GBM_01_f_measure <- renderPlotly({
    plot_ly(mode = "lines", type = 'scatter3d') %>%
      add_trace(data = models_list$GBM_01$none$results, x = ~interaction.depth, y = ~n.trees, z = ~F, line = list(color = c("green")), name = "none") %>%
      add_trace(data = models_list$GBM_01$up$results, x = ~interaction.depth, y = ~n.trees, z = ~F, line = list(color = c("red")), name = "up") %>%
      add_trace(data = models_list$GBM_01$down$results, x = ~interaction.depth, y = ~n.trees, z = ~F, line = list(color = c("blue")), name = "down")
  })
  
  output$GBM_01_var_imp <- renderPlotly({
    plot_ly(type = 'bar') %>%
      add_trace(data = var_imp_list[["GBM_01"]][["none"]], y = ~variable, x = ~importance, marker = list(color = c("green")), name = "none") %>%
      add_trace(data = var_imp_list[["GBM_01"]][["up"]], y = ~variable, x = ~importance, marker = list(color = c("red")), name = "up") %>%
      add_trace(data = var_imp_list[["GBM_01"]][["down"]], y = ~variable, x = ~importance, marker = list(color = c("blue")), name = "down") %>%
      layout(yaxis = list(autorange = "reversed"), margin = list(l = 200, r = 50, b = 50, t = 50, pad = 4))
  })
  
  output$GBM_01_roc <- renderPlotly({
    plot_ly(mode = "lines", type = 'scatter') %>%
      add_trace(data = as.data.frame(t(roc_list$GBM_01$none)), x = ~specificity, y = ~sensitivity, line = list(color = c("green")), name = "none") %>%
      add_trace(data = as.data.frame(t(roc_list$GBM_01$up)), x = ~specificity, y = ~sensitivity, line = list(color = c("red")), name = "up") %>%
      add_trace(data = as.data.frame(t(roc_list$GBM_01$down)), x = ~specificity, y = ~sensitivity, line = list(color = c("blue")), name = "down") %>%
      layout(xaxis = list(title = "Specificity", tickangle = -45, autorange = "reversed"),
             yaxis = list(title = "Sensitivity"))
  })
  
  output$GBM_01_cm <- renderPlot({
    cm_array <- array(c(cm_list$GBM_01$down$table, cm_list$GBM_01$none$table, cm_list$GBM_01$up$table), dim=c(2,2,3), dimnames = list(c("Prediction CO", "Prediction FP"), c("Reference CO", "Reference FP"), c("down", "none", "up")))
    fourfoldplot(cm_array, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, mfcol = c(1,3), std = "all.max")
  })
  
  output$GBM_01_down_output <- renderPrint(cm_list$GBM_01$down)
  output$GBM_01_none_output <- renderPrint(cm_list$GBM_01$none)
  output$GBM_01_up_output <- renderPrint(cm_list$GBM_01$up)
  
  output$GBM_01_results <- DT::renderDataTable({
    datatable(results_list$GBM_01, rownames = TRUE)
  })   
  ##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
  ############################################# GBM_01 ##############################################
  
} # end of: server <- function(input, output)

shinyApp(ui = ui, server = server)

