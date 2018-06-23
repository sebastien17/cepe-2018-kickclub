#Level Name Compatibility (à adapter)
levels(dta_.train.y) <- c("CO","FP")
levels(dta_.test.y) <- c("CO","FP")

#Level ref = FP
dta_.train.y <- relevel(dta_.train.y, "CO")
dta_.test.y <- relevel(dta_.test.y, "CO")

# ci-dessus à faire avant la generation du RDS data

# load dataset
dta_ <- readRDS("..\\Data\\loan5.RDS")

library(caret)
library(MLmetrics)
library(parallel)
library(doParallel)
library(reshape2)
library(ggplot2)
library(e1071)
library(rlist)


# reduction individus dataset
sampling_factor=0.01

set.seed(17)
dta_sample_index <- createDataPartition(dta_$loan_status, p = sampling_factor, list = FALSE, times = 1)
dta_sample <- dta_[dta_sample_index,]

# les indices pour train set
train.index <- createDataPartition(dta_sample$loan_status, p = .66, list = FALSE, times = 1)

# si besoin de séparer Y et X
dta_.train.x <- data.frame(dta_sample[train.index,!(colnames(dta_) %in% c("loan_status"))])
dta_.train.y <- as.factor(dta_sample[train.index,c("loan_status")])
dta_.test.x <- data.frame(dta_sample[-train.index,!(colnames(dta_) %in% c("loan_status"))])
dta_.test.y <- as.factor(dta_sample[-train.index,c("loan_status")])

# sinon
dta_.train <- data.frame(dta_sample[train.index,])
dta_.test <- data.frame(dta_sample[-train.index,])


# paral
detectCores() # nombre de coeurs sur la machine
cluster <- makeCluster(detectCores() - 1) # par convention on laisse un coeur pour l'OS
registerDoParallel(cluster)

# training model
k_cv_sampling = 10

# à définir si vous voulez
seed_list = lapply(1:k_cv_sampling+1,function(in_){seq(1:24)})
seed_list <- list.append(seed_list,17)
#seeds <- list(seed_list,seed_list,seed_list,17)

objControl <- trainControl(method="cv",
                           number= k_cv_sampling,
                           returnResamp='none',
                           summaryFunction = prSummary,
                           classProbs = TRUE,
                           allowParallel = TRUE,
                           seeds = seed_list,
                           sampling = 'up')
# parametrage modèle (dépendant des modèles)
gridsearch <- expand.grid(mtry=seq(1, 24, 1))

# méthode à changer suivant le cas
getModelInfo("parRF") # details model
tune <- train(dta_.train.x,dta_.train.y,method = "parRF",tuneGrid=gridsearch, trControl=objControl,metric='F')
tune

stopCluster(cluster)

saveRDS(tune, "C:...")



