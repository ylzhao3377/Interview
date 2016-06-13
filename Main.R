library(xgboost)
library(Ckmeans.1d.dp)
source('DataProcessing.R')
source('XGBoost.R')
source('param.change.R')
######     Read Data    #########################################
train <- as.data.frame(read.csv('codetest_train.txt',sep = '\t'))
test <- as.data.frame(read.csv('codetest_test.txt',sep='\t'))
###### Using Mean replace NA ####################################
mytrain <- DataProcessing(train)
mytest <- DataProcessing(test)
######  XGBOOST with 5-fold CV  #################################
K <- 5
mytrain_df <- as.data.frame(mytrain)
mytrain_df$id <- sample(1:K, dim(mytrain_df)[1], replace=TRUE)
list <- 1:K
set.seed(123)
dtrain <- xgb.DMatrix(mytrain[,-1], label=mytrain[,1])
dtest <- xgb.DMatrix(mytest)
par(mfrow=c(1, 2))
xg.params=list(
  "eval_metric" = "rmse", 
  'lambda' = 0.05,
  "eta" = 0.03,
  "max_depth" = 6
)
param.list=list(xg.params)
param.list=param.change(param.list, 0.05, 8)
param.list=param.change(param.list, 0.2, 4)
param.list=param.change(param.list, 0.08, 6)
param.list=param.change(param.list, 0.3,2)
final_train <- NULL
final_test <- NULL
final_mse <- NULL
MSE_list <- NULL
#######  Record prediction, mse in each model  ##################
for(idx in seq_along(param.list)){
  TrainPred <- NULL
  TestPred <- NULL
  for (i in 1:K){
    result <- XGBoost(mytrain_df, i, dtrain, dtest, param.list[[idx]])
    pred_train <- result[[1]]
    pred_test <- result[[2]]
    mse_tmp <- result[[3]]
    names <- result[[4]]
    bst.cv <- result[[5]]
    importance_matrix <- xgb.importance(names, model = bst.cv)
    print(xgb.plot.importance(importance_matrix[1:30,]))
    TrainPred <- cbind(TrainPred,pred_train)
    TestPred <- cbind(TestPred,pred_test)
    MSE_list <- c(MSE_list,mse_tmp)
  }
  train_pred <- apply(TrainPred, 1, mean)
  tmp <- cbind(train_pred, mytrain[,1])
  mse_train <- mean((train_pred-mytrain[,1])^2)
  test_pred <- apply(TestPred, 1, mean)
  final_train <- cbind(final_train, train_pred)
  final_mse <- c(final_mse, mse_train)
  final_test <- cbind(final_test, test_pred)
}

tmp <- apply(final_train[,c(1,3,4)], 1, mean)
miao <- cbind(tmp, mytrain[,1])
mean((final_train[,c(1,3,4)]-mytrain[,1])^2)
submission <- apply(final_test[,c(1,3,4)],1 ,mean)
write.table(submission, "Submission.txt", sep="\n",col.names=F,row.names=F)

