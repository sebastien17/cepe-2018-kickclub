loan_all <- readRDS("C:/Users/sebde/Documents/Cours/CEPE Data Scientist/Atelier/data/lending_club_2/loan5.RDS")

factor_to_num <- function(mod){
  df <- as.data.frame(mod)
  for(i in levels(mod)){
    s <- as.data.frame(rep(0, times = length(mod)))
    colnames(s) <- i
    l <- which(mod == i)
    s[l,] <- 1
    df[,i] <- s
  }
  return(df[,-1])
}

explode_factor <- function(dat, excl_col = c("loan_status")){
  df <- dat[,]
  cl <- sapply(df, class)
  col_fact <- which(cl == "factor")
  
  for(i in col_fact){
    if(!(i %in% excl_col)){
      i_matrix <- factor_to_num(df[,i])
      df <- cbind(df,i_matrix)
    }
  }
  
  df <- cbind(df[excl_col], df[, -col_fact])
  return(df)
}

success_rate <- function(t){
  c <- colnames(t)
  r <- rownames(t)
  
  if(! "Fully Paid" %in% c){
    col_names <- colnames(t)
    t <- cbind(t, 0)
    colnames(t) <- c(col_names, "Fully Paid")
  }
  if(! "Charged Off" %in% c){
    col_names <- colnames(t)
    t <- cbind(t, 0)
    colnames(t) <- c(col_names, "Charged Off")
  }
  if(! "Fully Paid" %in% r){
    row_names <- rownames(t)
    t <- rbind(t, 0)
    rownames(t) <- c(row_names, "Fully Paid")
  }
  if(! "Charged Off" %in% r){
    row_names <- rownames(t)
    t <- rbind(t, 0)
    rownames(t) <- c(row_names, "Charged Off")
  }
  
  res <- (t["Charged Off", "Charged Off"]+t["Fully Paid", "Fully Paid"]) / sum(t)
  return(res)
}

# on fixe la graine
set.seed(0)
n_data_set <- 50000
row_data_set <- sample(1:nrow(loan_all),n_data_set)

# on réduit le data set
loan5 <- loan_all[row_data_set,]

# on divise le data set en 2 (qualibrage des modèles / choix du modèle)

loan5_num <- explode_factor(loan5)

set.seed(0)
intrain <- createDataPartition(y = loan5_num$loan_status, p= 0.7, list = FALSE)
training <- loan5_num[intrain,]
testing <- loan5_num[-intrain,]

library(caret)
library(doParallel)
cl <- makeCluster(3)
registerDoParallel(cl)

# Avec caret + parallelisation
ptm <- proc.time()
paramgrid = data.frame(k=seq(10,150,by=10))
trctrl <- trainControl(method = "cv", number = 10, allowParallel = TRUE)
set.seed(0)
knn_fit <- train(loan_status ~., data = training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneGrid = paramgrid)
proc.time() - ptm

stopCluster(cl)

saveRDS(knn_fit, "C:/Users/sebde/Documents/Cours/CEPE Data Scientist/Atelier/data/lending_club_2/knn_fit_1_to_10_cv_10.RDS")

knn_pred <- predict(knn_fit, loan_all_num[,-1])


plot(knn_fit)
confusionMatrix(knn_fit)


# caret + sampling up
cl <- makeCluster(3)
registerDoParallel(cl)
ptm <- proc.time()
trctrl <- trainControl(method = "cv", number = 10, allowParallel = TRUE, sampling = "up")
set.seed(0)
knn_fit <- train(loan_status ~., data = training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneGrid = paramgrid)
proc.time() - ptm

saveRDS(knn_fit, "C:/Users/sebde/Documents/Cours/CEPE Data Scientist/Atelier/data/lending_club_2/knn_fit_10_to_150_cv_10_resampling.RDS")
stopCluster(cl)


# sélection de variable
# 1. le gros nettoyage
# 2. separation corel + factor

# ==> pdf
# expliquer la diminution du set

# est-ce qu'on peut prédire le défaut?


# filtrage loan_status
# echantillonnage
# equilibré et pas

# modèle
# glm: eric
# glmnet: eric
# lda: eric
# qda: eric
# knn: sebd
# rpart: guillaume
# bagging: sebL
# randomForest: sebL
# boosting: sebL
# svm: guillaume