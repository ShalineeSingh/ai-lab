#file <- readline("Enter the file name : \n")
#data = as.matrix(read.table("file",sep=","))

data1 = as.matrix(read.table("Iris_data_norm_train.txt",sep=","))
data2 = as.matrix(read.table("iris_data_norm_test.txt",sep=","))
data <- rbind(data1,data2)

output <- function(sum){
  if(sum > 0){
    return(1) 
    }
  else{
    return(-1) 
  }
}
#uncomment following lines for another data set
#cols <- readline("Enter the number of columns of training data : \n")
#temp <- as.numeric(cols-1)
#weight <- runif(temp,0,1)
weight <- runif(4,0,1)

#train <- readline("Enter the percentage of training data : \n")
#train <- as.numeric(train)
#limit <- floor((train/100)*nrow(data))

limit <-100
for(i in 1:limit){
    rows <- limit
    r_rows <- as.integer(runif(1,1,rows))
    x <- data[r_rows,1:4]
    x <- as.numeric(x)
    in_file <- as.numeric(data[i,5])
    sum = weight%*%x
    error = in_file - output(sum)
    weight <- weight + error*x
}

correct <- 0
test_rows <- nrow(data)-limit
for(i in 1:test_rows){
  y <- data[i,1:4]
  exp_output <- as.numeric(data[i,5])
  y <- as.numeric(y)
  result = y%*%weight
  if(result <0 && exp_output == 1){
    correct <- correct+1
  }
  if(result>0 && exp_output == -1){
    correct <- correct+1
  }
}

accuracy <- (correct/test_rows)*100
print("Correct values : ")
print(correct)
print("Number of test rows: ")
print(test_rows)
print("Accuracy Percent: ")
print(accuracy)
