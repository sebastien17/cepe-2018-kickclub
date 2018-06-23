rm(list = ls())
.rs.restartR()
objects()


# load dataset
dta <- readRDS("..\\Data\\loan5.RDS")
dta <- readRDS("loan5.RDS")

library(caret)
library(kernlab) #important pour la fonction predict
library(MLmetrics)
library(parallel)
library(doParallel)
library(reshape2)
library(ggplot2)
library(rlist)


# reduction individus dataset
sampling_factor=0.01

set.seed(1234)
dta_sample_index <- createDataPartition(dta$loan_status, p = sampling_factor, list = FALSE, times = 1)
dta_sample <- dta[dta_sample_index,]

mean(dta$loan_status=='CO')
mean(dta$loan_status=='FP')

mean(dta_sample$loan_status=='CO')
mean(dta_sample$loan_status=='FP')

dim(dta)
dim(dta_sample)

# les indices pour train set
train.index <- createDataPartition(dta_sample$loan_status, p = .66, list = FALSE, times = 1)

#jeu de donnees d'apprentissage et de test 2/3 1/3
dta.train <- dta_sample[train.index,]
dta.test <- dta_sample[-train.index,]

# si besoin de séparer Y et X
dta.train.x <- dta.train[,2:length(dta_sample)]
dta.train.y <- dta.train[,1]

dta.test.x <- dta.test[,2:length(dta_sample)]
dta.test.y <- dta.test[,1]

dim(dta.train.x)
dim(dta.train.y)
is.vector(dta.train.y)
is.matrix(dta.train.y)
is.data.frame(dta.train.y)
class(dta.train.y)
length(dta.train.y)

dim(dta.test.x)
length(dta.test.y)

identical(dta.test$loan_status,dta.test.y)

# details model
getModelInfo("svmLinear") 
getModelInfo("svmRadial") 

# paral
detectCores() # nombre de coeurs sur la machine
cluster <- makeCluster(detectCores() - 1) # par convention on laisse un coeur pour l'OS
registerDoParallel(cluster)

# on va optimiser via le package caret avec un C entre 0.001 et 100 en modele lineaire
gridsearch <- expand.grid(C=(10^(-3:2)))

# on fait une optimisation via une cross-validation 5-fold
objControl <- trainControl(method="cv",
                           number= 5,
                           search='grid')


ptm <- proc.time()
svm.lin.opt <- train(loan_status~.,data=dta.train,
              method = "svmLinear",
              tuneGrid=gridsearch, 
              trControl=objControl,
              allowParallel=TRUE)

stopCluster(cluster)

proc.time()-ptm
svm.lin.opt
plot(svm.lin.opt)

attributes(svm.lin.opt)
#accuracy de 68% pas terrible
svm.lin.opt$results
svm.lin.opt.pred <- predict(svm.lin.opt$finalModel,dta.test.x)
#la matrice a ete binarisee sur les facteurs -> il faudrait le faire a la main pour pouvoir predire
#nombre finale de dimensions = 142
length(svm.lin.opt$coefnames)
svm.lin.opt$coefnames

#on peut essayer de verifier que le predict fonctionne quand on ne prend que des variables quanti + le facteur a expliquer
index.quanti <- c(1,which(sapply(dta_sample,class)!='factor'))
dta_sample.quanti <- dta_sample[,index.quanti]

dta.train.quanti <- dta_sample.quanti[train.index,]
dta.test.quanti <- dta_sample.quanti[-train.index,]


# paral
detectCores() # nombre de coeurs sur la machine
cluster <- makeCluster(detectCores() - 1) # par convention on laisse un coeur pour l'OS
registerDoParallel(cluster)

# on va optimiser via le package caret avec un C entre 0.001 et 100 en modele lineaire
gridsearch <- expand.grid(C=(10^(-3:2)))

# on fait une optimisation via une cross-validation 5-fold
objControl <- trainControl(method="cv",
                           number= 5,
                           search='grid')


ptm <- proc.time()
svm.lin.opt.quanti <- train(loan_status~.,data=dta.train.quanti,
                     method = "svmLinear",
                     tuneGrid=gridsearch, 
                     trControl=objControl,
                     allowParallel=TRUE)

stopCluster(cluster)

proc.time()-ptm

svm.lin.opt.quanti
plot(svm.lin.opt.quanti)

attributes(svm.lin.opt.quanti)
mean(dta.train.quanti$loan_status=='FP')
#accuracy est celle du modele qui predit FP tout le temps
svm.lin.opt.quanti$results
svm.lin.opt.quanti.pred <- predict(svm.lin.opt.quanti$finalModel,dta.test.quanti[,2:length(dta.test.quanti)])
#maintenant on peut predire pcq le nombre de dimensions de la matrice de reval n'a pas ete change
length(svm.lin.opt.quanti$coefnames)
svm.lin.opt.quanti$coefnames


table(svm.lin.opt.quanti.pred,dta_.test.quanti$loan_status)
risque.est <- mean(dta_.test.quanti$loan_status!=svm.lin.opt.quanti.pred)
print(risque.est)


#on peut essayer sur les donnees quanti mais avec un noyau gaussien
# paral
detectCores() # nombre de coeurs sur la machine
cluster <- makeCluster(detectCores() - 1) # par convention on laisse un coeur pour l'OS
registerDoParallel(cluster)

# on va optimiser via le package caret un SVM gaussien avec un C entre 0.001 et 10 en sigma -0.01 et 100
gridsearch <- expand.grid(sigma=10^(-2:2),C=10^(-3:1))

ptm <- proc.time()
svm.gauss.opti.quanti <- train(loan_status~.,data=dta.train.quanti,
                               method = "svmRadialSigma",
                               tuneGrid=gridsearch, 
                               trControl=objControl,
                               allowParallel=TRUE)

stopCluster(cluster)

proc.time()-ptm

svm.gauss.opti.quanti
plot(svm.gauss.opti.quanti)

attributes(svm.gauss.opti.quanti)
svm.gauss.opti.quanti$results
mean(dta.train.quanti$loan_status=='FP')
#accuracy est celle du modele qui predit FP tout le temps
svm.gauss.opti.quanti$finalModel
svm.gauss.opti.quanti.pred <- predict(svm.gauss.opti.quanti$finalModel,dta.test.quanti[,2:length(dta.test.quanti)])
#maintenant on peut predire pcq le nombre de dimensions de la matrice de reval n'a pas ete change
length(svm.gauss.opti.quanti$coefnames)
svm.gauss.opti.quanti$coefnames


table(svm.gauss.opti.quanti.pred,dta_.test.quanti$loan_status)
risque.est <- mean(dta_.test.quanti$loan_status!=svm.gauss.opti.quanti.pred)
print(risque.est)

summary(dta.train.quanti)

#on essaie un modele en deux dimensions pour essayer de comprendre ce qui se passe
#on choisit les variables annual_inc et int_rate
dta.train.quanti.2d <- dta_sample.quanti[train.index,c('loan_status','int_rate','annual_inc')]
dta.test.quanti.2d <- dta_sample.quanti[-train.index,c('loan_status','int_rate','annual_inc')]

#ces deux variables ne sont quasiment pas correlees
plot(dta.train.quanti.2d$int_rate~dta.train.quanti.2d$annual_inc)
cor(dta.train.quanti.2d$int_rate,dta.train.quanti.2d$annual_inc)

# paral
detectCores() # nombre de coeurs sur la machine
cluster <- makeCluster(detectCores() - 1) # par convention on laisse un coeur pour l'OS
registerDoParallel(cluster)

# on va optimiser via le package caret avec un C entre 0.001 et 100 en modele lineaire
gridsearch <- expand.grid(C=(10^(-3:2)))

ptm <- proc.time()
svm.lin.opt.quanti.2d <- train(loan_status~.,data=dta.train.quanti.2d,
                            method = "svmLinear",
                            tuneGrid=gridsearch, 
                            trControl=objControl,
                            allowParallel=TRUE,
                            scale=F)

stopCluster(cluster)

proc.time()-ptm

svm.lin.opt.quanti.2d
plot(svm.lin.opt.quanti.2d)

attributes(svm.lin.opt.quanti.2d)
mean(dta.train.quanti.2d$loan_status=='FP')
#accuracy est moins bonne que celle du modele qui predit FP tout le temps
svm.lin.opt.quanti.2d$results
svm.lin.opt.quanti.2d.pred <- predict(svm.lin.opt.quanti.2d$finalModel,dta.test.quanti.2d[,2:length(dta.test.quanti.2d)])
#maintenant on peut predire pcq le nombre de dimensions de la matrice de reval n'a pas ete change
length(svm.lin.opt.quanti.2d$coefnames)
svm.lin.opt.quanti.2d$coefnames


table(svm.lin.opt.quanti.2d.pred,dta.test.quanti.2d$loan_status)
risque.est <- mean(dta.test.quanti.2d$loan_status!=svm.lin.opt.quanti.2d.pred)
print(risque.est)


#a t on de meilleurs resultats en centrant et reduisant les donnees ?
dta.train.quanti.2d.rescaled <- cbind(dta.train.quanti.2d$loan_status, data.frame(scale(dta.train.quanti.2d[,c(2:3)])))
names(dta.train.quanti.2d.rescaled) <- names(dta.train.quanti.2d)

#on verifie que ca a bien marche
sapply(dta.train.quanti.2d,class)
sapply(dta.train.quanti.2d.rescaled,class)

mean(dta.train.quanti.2d.rescaled$int_rate)
mean(dta.train.quanti.2d.rescaled$annual_inc)
var(dta.train.quanti.2d.rescaled$int_rate)
var(dta.train.quanti.2d.rescaled$annual_inc)

dta.test.quanti.2d.rescaled <- cbind(dta.test.quanti.2d$loan_status, data.frame(scale(dta.test.quanti.2d[,c(2:3)])))
names(dta.test.quanti.2d.rescaled) <- names(dta.test.quanti.2d)

# paral
detectCores() # nombre de coeurs sur la machine
cluster <- makeCluster(detectCores() - 1) # par convention on laisse un coeur pour l'OS
registerDoParallel(cluster)

# on va optimiser via le package caret avec un C entre 0.001 et 100 en modele lineaire
gridsearch <- expand.grid(C=(10^(-3:2)))

ptm <- proc.time()
svm.lin.opt.quanti.2d.rescaled <- train(loan_status~.,data=dta.train.quanti.2d.rescaled,
                               method = "svmLinear",
                               tuneGrid=gridsearch, 
                               trControl=objControl,
                               allowParallel=TRUE)

stopCluster(cluster)

proc.time()-ptm

svm.lin.opt.quanti.2d.rescaled
plot(svm.lin.opt.quanti.2d.rescaled)

attributes(svm.lin.opt.quanti.2d.rescaled)
mean(dta_.train.quanti.2d.rescaled$loan_status=='FP')
#accuracy est celle du modele qui predit FP tout le temps
svm.lin.opt.quanti.2d.rescaled$results
svm.lin.opt.quanti.2d.rescaled.pred <- predict(svm.lin.opt.quanti.2d.rescaled$finalModel,dta_.test.quanti.2d.rescaled[,2:length(dta_.test.quanti.2d.rescaled)])
#maintenant on peut predire pcq le nombre de dimensions de la matrice de reval n'a pas ete change
length(svm.lin.opt.quanti.2d.rescaled$coefnames)
svm.lin.opt.quanti.2d.rescaled$coefnames
svm.lin.opt.quanti.2d.rescaled$finalModel
dim(svm.lin.opt.quanti.2d.rescaled$trainingData)

table(svm.lin.opt.quanti.2d.rescaled.pred,dta.test.quanti.2d.rescaled$loan_status)
risque.est <- mean(dta.test.quanti.2d.rescaled$loan_status!=svm.lin.opt.quanti.2d.rescaled.pred)
print(risque.est)


#grosso modo quan on rescale les donnees annual_inc et int_rate la meilleur prediction est le pret est tout le temps rembourse
#en fait il a l'air complique de pouvoir separer les points en dimension 2 en lineaire
plot(dta.train.quanti.2d$int_rate,dta.train.quanti.2d$annual_inc,col=as.numeric(dta.train.quanti.2d$loan_status))

summary(dta.train.quanti)

#on essaye la meme chose sur les variables qualitatives
index.quali <- which(sapply(dta_sample,class)=='factor')
dta_sample.quali <- dta_sample[,index.quali]

dta.train.quali <- dta_sample.quali[train.index,]
dta.test.quali <- dta_sample.quali[-train.index,]


# paral
detectCores() # nombre de coeurs sur la machine
cluster <- makeCluster(detectCores() - 1) # par convention on laisse un coeur pour l'OS
registerDoParallel(cluster)

# on va optimiser via le package caret avec un C entre 0.001 et 100 en modele lineaire
gridsearch <- expand.grid(C=(10^(-3:2)))

ptm <- proc.time()
svm.lin.opt.quali <- train(loan_status~.,data=dta.train.quali,
                                        method = "svmLinear",
                                        tuneGrid=gridsearch, 
                                        trControl=objControl,
                                        allowParallel=TRUE)

stopCluster(cluster)

proc.time()-ptm

svm.lin.opt.quali
plot(svm.lin.opt.quali)

attributes(svm.lin.opt.quali)
mean(dta.train.quali$loan_status=='FP')
#accuracy est celle du modele qui predit FP tout le temps
svm.lin.opt.quali$results
svm.lin.opt.quali.pred <- predict(svm.lin.opt.quali$finalModel,dta.test.quali[,2:length(dta.test.quali)])
#maintenant on peut predire pcq le nombre de dimensions de la matrice de reval n'a pas ete change
length(svm.lin.opt.quali$coefnames)
svm.lin.opt.quali$coefnames
svm.lin.opt.quali$finalModel
dim(svm.lin.opt.quali$trainingData)

table(svm.lin.opt.quali.pred,dta.test.quali$loan_status)
risque.est <- mean(dta.test.quanti.2d.rescaled$loan_status!=svm.lin.opt.quanti.2d.rescaled.pred)
print(risque.est)

#on essaye en 2d avec les variables term et grade
dta.train.quali.2d <- dta_sample.quali[train.index,c('loan_status','term','grade')]
dta.test.quali.2d <- dta_sample.quali[-train.index,c('loan_status','term','grade')]

#ces deux variables ont clairement un impact sur le taux de remboursement
library(dplyr)

dta.train.quali.2d %>%
  group_by(grade,term) %>%
  mutate(pret_rembourse=as.numeric(loan_status=='FP')) %>%
  group_by(grade,term) %>%
  summarise(taux_de_non_remboursement = 1-mean(pret_rembourse)) %>%
  View()

plot(as.numeric(dta.train.quali.2d$term),as.numeric(dta.train.quali.2d$grade),col=as.numeric(dta.train.quali.2d$loan_status))
plot(as.numeric(dta.train.quali.2d$term))

# paral
detectCores() # nombre de coeurs sur la machine
cluster <- makeCluster(detectCores() - 1) # par convention on laisse un coeur pour l'OS
registerDoParallel(cluster)

# on va optimiser via le package caret avec un C entre 0.001 et 100 en modele lineaire
gridsearch <- expand.grid(C=(10^(-3:2)))

ptm <- proc.time()
svm.lin.opt.quali.2d <- train(loan_status~.,data=dta.train.quali.2d,
                               method = "svmLinear",
                               tuneGrid=gridsearch, 
                               trControl=objControl,
                               allowParallel=TRUE)

stopCluster(cluster)
proc.time()-ptm
#prevision est celle de tout le temps rembourse
svm.lin.opt.quali.2d

#on essaie avec un polynome
gridsearch <- expand.grid(scale=10^(-4:2),C=10^(-4:1),degree=(2:10))

cluster <- makeCluster(detectCores() - 1) # par convention on laisse un coeur pour l'OS
registerDoParallel(cluster)
ptm <- proc.time()
svm.poly.opt.quali.2d <- train(loan_status~.,data=dta.train.quali.2d,
                              method = "svmPoly",
                              tuneGrid=gridsearch, 
                              trControl=objControl,
                              allowParallel=TRUE)

stopCluster(cluster)
proc.time()-ptm

plot(svm.poly3.opt.quali.2d)
svm.poly.opt.quali.2d
saveRDS(svm.poly.opt.quali.2d, "C:...")

attributes(svm.lin.opt.quanti.2d)
mean(dta.train.quanti.2d$loan_status=='FP')
#accuracy est moins bonne que celle du modele qui predit FP tout le temps
svm.lin.opt.quanti.2d$results
svm.lin.opt.quanti.2d.pred <- predict(svm.lin.opt.quanti.2d$finalModel,dta.test.quanti.2d[,2:length(dta.test.quanti.2d)])
#maintenant on peut predire pcq le nombre de dimensions de la matrice de reval n'a pas ete change
length(svm.lin.opt.quanti.2d$coefnames)
svm.lin.opt.quanti.2d$coefnames


table(svm.lin.opt.quanti.2d.pred,dta.test.quanti.2d$loan_status)
risque.est <- mean(dta.test.quanti.2d$loan_status!=svm.lin.opt.quanti.2d.pred)
print(risque.est)

saveRDS(tune, "C:/Users/Guillaume/Documents/projet R/cepe-2018-kickclub/Model/GG_SVM_poly_2d_quali.RDS")

