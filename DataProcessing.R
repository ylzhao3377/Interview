DataProcessing <- function(input){
  for(i in 1:ncol(input)){
    for(j in 1:nrow(input)){
      if(is.na(input[j,i])){
        input[j,i] = round(mean(input[,i], na.rm=T),3)
      }
    }
  }
  input <- model.matrix(~., data = input)
  input <- input[,-1]
  return(input)
}