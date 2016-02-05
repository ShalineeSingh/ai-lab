#reading the train file

train_rows = as.matrix(read.table("Iris_data_norm_train.txt",sep=","))
x <- train_rows[,1:4]
x_tran <- t(train_rows[,1:4])
last <- train_rows[,5]

B <- x_tran%*%x

#finding the weight for the given data
weight <- solve(B)%*%x_tran%*%last

test <- as.matrix(read.table("iris_data_norm_test.txt",sep=","))
no_err <- 0
for(i in 1:nrow(test)){
  y <- test[i,1:4]
  last_out <- as.numeric(test[i,5])
  y <- as.numeric(y)
  result = y%*%weight
  if(result<0 && last_out == 1){
    no_err <- no_err+1
  }
  if(result>0 && last_out == -1){
    no_err <- no_err+1
  }
}
accuracy <- (no_err/nrow(test))*100
print(weight)
