library(glmnet)
library(randomForest)
library(xgboost)

mydata <- read.csv("bank.csv", sep = ";", quote = "\"'")
str(mydata)
summary(mydata)
head(mydata)
apply(is.na(mydata), 2, sum)

shuffle <- sample(1:nrow(mydata), replace = F, nrow(mydata))
cut <- seq.int(1, nrow(mydata), 900)
predictive.ability1 <- NULL
for(i in 1:5){
  test.index <- cut[i] : (cut[i] + 899)
  train.index <- c(1:nrow(mydata))[which(!1:nrow(mydata) %in% 
                                            test.index)]
  mytest <- mydata[test.index,]
  mytrain <- mydata[train.index,]
  myfit.full <- glm(y ~., data = mytrain, family = "binomial")
  myfit.null <- glm(y ~ 1, data = mytrain, family = "binomial")
  myfit.BIC <- step(myfit.null, scope = list(upper = myfit.full,
                                             lower = myfit.null),
                    direction = "forward", k = log(nrow(mytrain)))
  
  BIC.pred <- predict(myfit.BIC, mytest, type = 'response')
  BIC.pred <- ifelse(BIC.pred > 0.5, 1, 0)
  freq <- table(mytest$y, BIC.pred)
  precision <- freq[2,2]/(freq[1,2] + freq[2,2])
  recall <- freq[2,2]/(freq[2,1] + freq[2,2])
  F1.measure <- 2/(1/precision + 1/recall)
  predictive.ability1 <- rbind(predictive.ability1, c(precision, recall, 
                                                    F1.measure))
}
colnames(predictive.ability1) <- c("precision", "recall", 'F1.measure')
apply(predictive.ability1, 2, mean)

predictive.ability2 <- NULL
for(i in 1:5){
  test.index <- cut[i] : (cut[i] + 899)
  train.index <- c(1:nrow(mydata))[which(!1:nrow(mydata) %in% 
                                           test.index)]
  mytest <- mydata[test.index,]
  mytrain <- mydata[train.index,] 
  myfit.forest <- randomForest(y ~., data = mytrain, ntree = 500,
                               importance = T)
  forest.pred <- predict(myfit.forest, mytest[, -17], 
                         type = "response")
  freq <- table(mytest[,17], forest.pred)
  precision <- freq[2,2]/(freq[1,2] + freq[2,2])
  recall <- freq[2,2]/(freq[2,1] + freq[2,2])
  F1.measure <- 2/(1/precision + 1/recall)
  predictive.ability2 <- rbind(predictive.ability2, c(precision, recall, 
                                                    F1.measure))
}
colnames(predictive.ability2) <- c("precision", "recall", 'F1.measure')
apply(predictive.ability2, 2, mean)

mydata.dm <- model.matrix(~., data = mydata)[,-1]
str(mydata.dm)
predictive.ability3 <- NULL
for(i in 1:5){
  test.index <- cut[i] : (cut[i] + 899)
  train.index <- c(1:nrow(mydata.dm))[which(!1:nrow(mydata.dm) %in% 
                                           test.index)]
  mytest <- mydata.dm[test.index,]
  mytrain <- mydata.dm[train.index,] 
  xgb.traindata <- xgb.DMatrix(mytrain[, -43], label = mytrain[, 43])
  xgb.testdata <- xgb.DMatrix(mytest[, -43], label = mytest[, 43])
  myfit.xgb <- xgb.train(list("objective" = "binary:logistic", eta = 0.4,
                              max.depth = 7, subsample = 0.8, 
                              colsample_bytree = 4, lambda = 1, 
                              eval_metric = "map"),
                         data = xgb.traindata, prediction = T, 
                         watchlist = list(validation = xgb.testdata),
                                          nrounds = 100)
  xgb.pred <- predict(myfit.xgb, xgb.testdata)
  xgb.pred <- ifelse(xgb.pred > 0.5, 1, 0)
  freq <- table(mytest[, 43], xgb.pred)
  precision <- freq[2,2]/(freq[1,2] + freq[2,2])
  recall <- freq[2,2]/(freq[2,1] + freq[2,2])
  F1.measure <- 2/(1/precision + 1/recall)
  predictive.ability3 <- rbind(predictive.ability3, c(precision, recall, 
                                                      F1.measure))
}
colnames(predictive.ability3) <- c("precision", "recall", 'F1.measure')
apply(predictive.ability3, 2, mean)


myfit.forest <- randomForest(y ~., data = mydata, ntree = 500,
                             importance = T)
