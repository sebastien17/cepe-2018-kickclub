library(caret)
library(MLmetrics)
library(doParallel)
library(plotly)
library(webshot)
library(ROSE)

tune_up

# load dataset
dta_ <- readRDS("C:/Users/sebde/OneDrive/Documents/cepe-2018-kickclub/Data/loan5.RDS")
# comment <- "grade - addr_state - issue_y - purpose - acc_now-delinq"
comment <- ""
# excl <- which(colnames(dta_) %in% c("issue_y", "addr_state", "grade", "purpose", "acc_now_delinq"))
# dta_ <- dta_[,-excl]

# reduction individus dataset
sampling_factor=0.01

set.seed(17)
dta_sample_index <- createDataPartition(dta_$loan_status, p = sampling_factor, list = FALSE, times = 1)
dta_sample <- dta_[dta_sample_index,]
summary(dta_sample)
dta_sample <- cbind(dta_sample["loan_status"], as.data.frame(model.matrix(loan_status~. -1, data = dta_sample)))

k_th <- round(sqrt(nrow(dta_sample)))

# les indices pour train set
train.index <- createDataPartition(dta_sample$loan_status, p = .66, list = FALSE, times = 1)

# n1 <- sum(dta_sample$loan_status == "CO")
# n2 <- sum(dta_sample$loan_status == "FP")
# train.index <- c(sample(which(dta_sample$loan_status == "CO"), round(2*n1/3)),
#                 sample(which(dta_sample$loan_status == "FP"), round(1*n2/3)))
# 
# table(dta_sample[train.index,"loan_status"])/nrow(dta_sample[train.index,])

# sinon
dta_.train <- data.frame(dta_sample[train.index,])
dta_.test <- data.frame(dta_sample[-train.index,])

dta_.train.rose <- ROSE(loan_status ~ ., data = dta_.train, seed = 1)$data

# training model
k_cv_sampling = 10
# k_list <- c(seq(1, round(4/5*k_th-1), 10),seq(round(4/5*k_th), round(6/5*k_th), 1), seq(6/5*k_th+1, 4*k_th, 10))

k_list <- seq(1, 4*k_th, 1)
length(k_list)

details <- paste(comment, "_k ", min(k_list), " - ", max(k_list), sep = "")
details <- paste(details, "_n ", nrow(dta_sample), sep = "")

# seq de k
gridsearch <- expand.grid(k=k_list)

# NONE
detectCores() # nombre de coeurs sur la machine
cluster <- makeCluster(detectCores() - 1) # par convention on laisse un coeur pour l'OS
registerDoParallel(cluster)

objControl_none <- trainControl(method="cv",
                                number= k_cv_sampling,
                                returnResamp='none',
                                summaryFunction = prSummary,
                                # classProbs = TRUE,
                                allowParallel = TRUE)

ptm <- proc.time()
tune_none <- train(loan_status ~ .,
              data = dta_.train,
              method = "knn",
              tuneGrid=gridsearch,
              trControl=objControl_none,
              preProcess = c("center", "scale"),
              metric='F')
proc.time()-ptm

stopCluster(cluster)

# ROSE
# detectCores() # nombre de coeurs sur la machine
# cluster <- makeCluster(detectCores() - 1) # par convention on laisse un coeur pour l'OS
# registerDoParallel(cluster)
# 
# objControl_none <- trainControl(method="cv",
#                                 number= k_cv_sampling,
#                                 returnResamp='none',
#                                 summaryFunction = prSummary,
#                                 # classProbs = TRUE,
#                                 allowParallel = TRUE)
# 
# ptm <- proc.time()
# tune_rose <- train(loan_status ~ .,
#                    data = dta_.train.rose,
#                    method = "knn",
#                    tuneGrid=gridsearch,
#                    trControl=objControl_none,
#                    preProcess = c("center", "scale"),
#                    metric='F')
# proc.time()-ptm
# 
# stopCluster(cluster)


# UP
detectCores() # nombre de coeurs sur la machine
cluster <- makeCluster(detectCores() - 1) # par convention on laisse un coeur pour l'OS
registerDoParallel(cluster)

objControl_up <- trainControl(method="cv",
                              number= k_cv_sampling,
                              returnResamp='none',
                              summaryFunction = prSummary,
                              # classProbs = TRUE,  
                              allowParallel = TRUE,
                              sampling = 'up')

ptm <- proc.time()
tune_up <- train(loan_status ~ .,
              data = dta_.train,
              method = "knn",
              tuneGrid=gridsearch,
              trControl=objControl_up,
              preProcess = c("center", "scale"),
              metric='F')
proc.time()-ptm

stopCluster(cluster)

# DOWN
detectCores() # nombre de coeurs sur la machine
cluster <- makeCluster(detectCores() - 1) # par convention on laisse un coeur pour l'OS
registerDoParallel(cluster)

objControl_down <- trainControl(method="cv",
                                number= k_cv_sampling,
                                returnResamp='none',
                                summaryFunction = prSummary,
                                # classProbs = TRUE,
                                allowParallel = TRUE,
                                sampling = 'down')

# parametrage modèle (dépendant des modèles)

ptm <- proc.time()
tune_down <- train(loan_status ~ .,
              data = dta_.train,
              method = "knn",
              tuneGrid=gridsearch,
              trControl=objControl_down,
              preProcess = c("center", "scale"),
              metric='F')
proc.time()-ptm

stopCluster(cluster)

plt <- plot_ly(mode = "lines", type = 'scatter') %>%
  add_trace(data = tune_none$results, x = ~k, y = ~F, line = list(color = c("green")), name = "none") %>%
  add_trace(data = tune_up$results, x = ~k, y = ~F, line = list(color = c("red")), name = "up") %>%
  add_trace(data = tune_down$results, x = ~k, y = ~F, line = list(color = c("blue")), name = "down")

plt

saveRDS(tune_none, paste("C:/Users/sebde/OneDrive/Documents/cepe-2018-kickclub/Model/SDE_knn_none", details, ".RDS", sep = ""))
# saveRDS(tune_rose, paste("C:/Users/sebde/OneDrive/Documents/cepe-2018-kickclub/Model/SDE_rose", details, ".RDS", sep = ""))
saveRDS(tune_up, paste("C:/Users/sebde/OneDrive/Documents/cepe-2018-kickclub/Model/SDE_knn_up", details, ".RDS", sep = ""))
saveRDS(tune_down, paste("C:/Users/sebde/OneDrive/Documents/cepe-2018-kickclub/Model/SDE_knn_down", details, ".RDS", sep = ""))

# export(p = plt, file = paste("C:/Users/sebde/OneDrive/Documents/cepe-2018-kickclub/SDE/knn_", details, ".png", sep = ""))

confusionMatrix(tune_none)
# confusionMatrix(tune_rose)
confusionMatrix(tune_up)
confusionMatrix(tune_down)

pred_none <- relevel(predict(tune_none, newdata = dta_.test), "CO")
# pred_rose <- relevel(predict(tune_rose, newdata = dta_.test), "CO")
pred_up <- relevel(predict(tune_up, newdata = dta_.test), "CO")
pred_down <- relevel(predict(tune_down, newdata = dta_.test), "CO")

knn_none_cm <- confusionMatrix(predict(tune_none, newdata = dta_.test), dta_.test$loan_status)
# confusionMatrix(predict(tune_rose, newdata = dta_.test), dta_.test$loan_status)
knn_up_cm <- confusionMatrix(predict(tune_up, newdata = dta_.test), dta_.test$loan_status)
knn_down_cm <- confusionMatrix(predict(tune_down, newdata = dta_.test), dta_.test$loan_status)

attributes(knn_none_cm)




saveRDS(knn_none_cm, "C:/Users/sebde/OneDrive/Documents/cepe-2018-kickclub/Shiny/model/knn_none_cm.RDS")
saveRDS(knn_up_cm, "C:/Users/sebde/OneDrive/Documents/cepe-2018-kickclub/Shiny/model/knn_up_cm.RDS")
saveRDS(knn_down_cm, "C:/Users/sebde/OneDrive/Documents/cepe-2018-kickclub/Shiny/model/knn_down_cm.RDS")