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

train <- readline("Enter the percentage of training data : \n")
train <- as.numeric(train)
train_rows <- floor((train/100)*nrow(data))
test_rows <- nrow(data)- train_rows
input_error <-c(0)
output_error <- c(0)
error_temp <- 1000000
weight_pre <- 0
for(epoch in 1:100){
  no_err_in <-0
  no_err_out <- 0
  
  #train the data
  
  for(i in 1:train_rows){
    x <- data[i,1:4]
    x <- as.numeric(x)
    in_file <- as.numeric(data[i,5])
    sum = weight%*%x
    error = in_file - output(sum)
    weight <- weight + 0.5*error*x
  }
  
  
  for(j in 1:train_rows){
    x <- data[j,1:4]
    x <- as.numeric(x)
    in_file <- as.numeric(data[j,5])
    sum = weight%*%x
    if(sum<0 && in_file == 1){
      no_err_in <- no_err_in+1
    }
    if(sum>0 && in_file == -1){
      no_err_in <- no_err_in+1
    }
  }
  if(error_temp < no_err_in){
    weight <- weight_pre
  }
  error_temp <- no_err_in
  weight_pre <- weight
  
  #testing the data
  
  
  for(k in (train_rows+1):test_rows){
    y <- data[k,1:4]
    exp_output <- as.numeric(data[k,5])
    y <- as.numeric(y)
    result = y%*%weight
    if(result <0 && exp_output == 1){
      no_err_out <- no_err_out+1
    }
    if(result>0 && exp_output == -1){
      no_err_out <- no_err_out+1
    }
  }
  
  input_error <- c(input_error,no_err_in)
  output_error <- c(output_error,no_err_out)
}
plot(input_error,pch=20,col='blue')
lines(input_error)
plot(output_error,pch=20,col='red')
lines(output_error)
print(weight)
