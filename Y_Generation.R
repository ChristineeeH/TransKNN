###### 根据x1和x2生成x #################
# x = x1 + x2
combine1 <- function(x1,x2){
  x = matrix(x1 + x2,length(x1),1)
  return(x)
}
# x = x1 - x2
combine2 <- function(x1,x2){
  x = matrix((x1 - x2),length(x1),1)
  return(x)
}

# x = |x1| + |x2|
combine3 <- function(x1,x2){
  x = matrix((abs(x1) + abs(x2)),length(x1),1)
  return(x)
}
# x = |x1| - |x2|
combine4 <- function(x1,x2){
  x = matrix((abs(x1) - abs(x2)),length(x1),1)
  return(x)
}

# x = sqrt(x1^2 + x2^2)
combine5 <- function(x1,x2){
  x = matrix(sqrt(x1*x1 + x2*x2),length(x1),1)
  return(x)
}
# x = sqrt(|x1^2 - x2^2|)
combine6 <- function(x1,x2){
  x = matrix(sqrt(abs(x1*x1 - x2*x2)),length(x1),1)
  return(x)
}

# x = max(x1,x2)
combine7 <- function(x1,x2){
  x = matrix(pmax(x1,x2),length(x1),1)
  return(x)
}
# x = min(x1,x2)
combine8 <- function(x1,x2){
  x = matrix(pmin(x1,x2),length(x1),1)
  return(x)
}

############ 对生成的x进行变换 ############
# 不变换
trans0 <- function(X){
  transx = matrix(X[,3],nrow(X),1)
  return(transx)
}

#Logistic Function
trans1 <- function(X){
  x = matrix(X[,3],nrow(X),1)
  transx = (1/(1+exp(-x)))
  return(transx)
}

# Generalized Logistic Function
trans2 <- function(X){
  x = matrix(X[,3],nrow(X),1)
  transx = (1/sqrt(1+exp(-2*x)))
  return(transx)
}

trans21 <- function(X){
  x = matrix(X[,3],nrow(X),1)
  transx = (1/((1+exp(-2*x))^(1/3)))
  return(transx)
}

trans22 <- function(X){
  x = matrix(X[,3],nrow(X),1)
  transx = (1/sqrt(1+exp(-3*x)))
  return(transx)
}

trans23 <- function(X){
  x = matrix(X[,3],nrow(X),1)
  transx = (1/(1+2*exp(-3*x))^(1/3))
  return(transx)
}

trans24 <- function(X){
  x = matrix(X[,3],nrow(X),1)
  transx = (1/sqrt(1+2*exp(-3*x)))
  return(transx)
}

trans25 <- function(X){
  x = matrix(X[,3],nrow(X),1)
  transx = (1/(1+exp(-3*x))^2)
  return(transx)
}

# Adjusted Arctan Function
trans3 <- function(X){
  x = matrix(X[,3],nrow(X),1)
  transx = (atan(x)/pi) + 0.5
  return(transx)
}

# ReLU Function
trans4 <- function(X){
  x = matrix(X[,3],nrow(X),1)
  transx = (1/(1+pmax(0,x)))
  return(transx)
}

# Softplus Function
trans5 <- function(X){
  x = matrix(X[,3],nrow(X),1)
  transx = (1/(1+log(1+exp(x))))
  return(transx)
}

#将得到的x补充到X最后一列
combine_function <- function(X,type){
  x1 = X[,1]
  x2 = X[,2]
  x = type(x1,x2)
  X = cbind(X,x)
  return(X)
}

#根据变换后的x与阈值进行比较
indicator_function <- function(X,type,threshold){
  trans_x = type(X)
  ind_trans_x = matrix(as.integer(trans_x > threshold),nrow(X),1)
  X = cbind(X,ind_trans_x)
  X = X[,c(1,2,4)]
  return(X)
}
