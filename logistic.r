
data = as.matrix(read.table("ex2data1.txt",sep=","))
rows=nrow(data)
cols=ncol(data)
max_iterations=100
weight <- 0
for (i in 1:(cols-2)){
  weight <- c(weight,0)
}
error <- 0.5
k <- 1
learning_rate <- 0.5
x <-1
z <- 1
while(x <= max_iterations){
  z <- 1
  sq_err <- 0
  while(z <= rows){
    d1 <- as.numeric(data[z,1])
    d2 <- as.numeric(data[z,2])
    d <- rbind(d1,d2)
    result <- weight %*% d
    third <- as.numeric(data[z,3])
    err <- result-third
    weight[1] <- weight[1] + (d1*learning_rate*third*(1/(1+exp(t(weight)*learning_rate))))*1/rows
    weight[2] <- weight[2] + (d2*learning_rate*third*(1/(1+exp(t(weight)*learning_rate))))*1/rows
    #weight[1] <- weight[1]+learning_rate*(1/rows)*third*(d1/(1+exp(third*result)))
    #weight[2] <- weight[2]+learning_rate*(1/rows)*third*(d2/(1+exp(third*result)))
    sq_err <- sq_err+(err*err);
    z <- z+1
  }
  y <- sq_err/max_iterations
  error <- c(error,(y/10))
  x <- x+1
}
#plot(error ,ylab="Error per iteration",xlab="Number of iterations",pch='.',col='red')
#lines(error,col="red")
line <- 1
for(i in rows){
  x=(-weight[1]/weight[2])*data[i,2]
line <-c(line,x)
}


class = as.factor(data[,3])
plot(data[,1],data[,2], col=class, pch=19)
par(new =T)
plot(line,ylab=NA,xlab=NA,pch='.')
lines(line,col="blue")
