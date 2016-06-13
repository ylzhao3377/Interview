XGBoost <- function(mytrain_df, i, dtrain, dtest, param){
  newtrain <- as.matrix(subset(mytrain_df, id %in% list[-i]))
  newtest <- as.matrix(subset(mytrain_df, id %in% c(i)) )
  dmytrain <- xgb.DMatrix(newtrain[,-1], label=newtrain[,1])
  dmytest <- xgb.DMatrix(newtest[,-1], label=newtest[,1])
  cv.nround <- 250
  bst.cv = xgb.train(param=param, data = dmytrain, 
                     nrounds = cv.nround, watchlist=list(validation=dmytest),
                     early.stop.round=10)
  pred_train <- predict(bst.cv, dtrain)
  pred_test <- predict(bst.cv, dtest)
  mse_tmp <- mean((predict(bst.cv,dmytest)-newtest[,1])^2)
  names <- dimnames(newtrain[,-1])[[2]]
  return(list(pred_train,pred_test,mse_tmp,names,bst.cv))
}