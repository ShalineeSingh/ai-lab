train_rows = as.matrix(read.table("Iris_data_norm_train.txt",sep=","))
x <- train_rows[,1:4]
last <- train_rows[,5]

rows <- nrow(x)
x_new <- matrix(0,rows,15)
for(i in 1:rows){
  x_new[i,] <- c(1,x[i,1],x[i,2],x[i,3],x[i,4],x[i,1]^2,x[i,2]^2,x[i,3]^2,x[i,4]^2,
                    x[i,1]*x[i,2],x[i,1]*x[i,3],x[i,1]*x[i,4],x[i,2]*x[i,3],x[i,2]*x[i,4],x[i,3]*x[i,4])
}
x_new_tran <- t(x_new)
B <- x_new_tran%*%x_new
#finding the weight vector for linear regression
weight <- solve(B)%*%x_new_tran%*%last

test <- as.matrix(read.table("iris_data_norm_test.txt",sep=","))
no_err <- 0
test_rows <- nrow(test)
for(i in 1:test_rows){
  y <- test[i,1:4]
  ex_out <- as.numeric(test[i,5])
  y <- as.numeric(y)
  y_new <- c(1,y[1],y[2],y[3],y[4],y[1]^2,y[2]^2,y[3]^2,y[4]^2,
                    y[1]*y[2],y[1]*y[3],y[1]*y[4],y[2]*y[3],y[2]*y[4],y[3]*y[4])
  result = y_new%*%weight
  if(result<0 && ex_out == 1){
    no_err <- no_err+1
  }
  if(result>0 && ex_out == -1){
    no_err <- no_err+1
  }
}
print(weight)

