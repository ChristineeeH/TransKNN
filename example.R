library(Rcpp)
library(transKNN)

# 产生 一个二维的矩阵X
rows <-500
test_rows<-10
cols <- 2
mean_value <- 0
sd_value <- 4
X <- matrix(rnorm(rows * cols, mean = mean_value, sd = sd_value), nrow = rows, ncol = cols)
testX <-matrix(rnorm(test_rows*cols, mean = mean_value, sd = sd_value), nrow = test_rows, ncol = cols)

# 产生Y
Y1 = 1/(1+exp(X[1:250,1]+X[1:250,2]))
Y1 = matrix(as.integer(Y1 > 0.5))
Y2 = 1/sqrt(1+exp(-2*(X[251:500,1]+X[251:500,2])))
Y2 = matrix(as.integer(Y2 > 0.3))
Y = rbind(Y1,Y2)

tag = rep(0,rows)  
tag[251:500] = 1 # 分布标记，Q分布为0
max_k = nrow(X)  # 训练集的数据量
data = cbind(X,Y) #训练集
m = 1 # P分布数量
d=2 # X的维度
pre_y = Trans_KNN(testX, max_k,data, tag, m,d)

  
