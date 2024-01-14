library(Rcpp)
library(ggplot2)


# 产生 X
rows <- 300
cols <- 2
mean_value <- 0
sd_value <- 4
X <- matrix(rnorm(rows * cols, mean = mean_value, sd = sd_value), nrow = rows, ncol = cols)


# 产生Y
funcList <- list(
  gen0 = function(X){
    X = combine_function(X,combine1)
    x = indicator_function(X,trans1,0.5)
    return(x)
  },
  gen1 = function(X){
    X = combine_function(X,combine1)
    x = indicator_function(X,trans2,0.3)
    return(x)
  },
  gen2 = function(X){
    X = combine_function(X,combine2)
    x = indicator_function(X,trans3,0.9)
    return(x)
  },
  gen3 = function(X){
    X = combine_function(X,combine3)
    x = indicator_function(X,trans22,0.55)
    return(x)
  },
  gen4 = function(X){
    X = combine_function(X,combine4)
    x = indicator_function(X,trans23,0.1)
    return(x)
  }
)
  # gen5 = function(X){
  #   X = combine_function(X,combine1)
  #   x = indicator_function(X,trans24,0.65)
  #   return(x)
  # },
  # gen6 = function(X){
  #   X = combine_function(X,combine1)
  #   x = indicator_function(X,trans25,0.35)
  #   return(x)
  # }
  # gen3 = function(X){
  #   X = combine_function(X,combine1)
  #   x = indicator_function(X,trans3,0.7)
  #   return(x)
  # },
  # gen4 = function(X){
  #   X = combine_function(X,combine1)
  #   x = indicator_function(X,trans2,0.6)
  #   return(x)
  # },
  # gen5 = function(X){
  #   X = combine_function(X,combine1)
  #   x = indicator_function(X,trans0,0.25)
  #   return(x)
  # }
  # gen6 = function(X){
  #   X = combine_function(X,combine2)
  #   x = indicator_function(X,trans1,0.35)
  #   return(x)
  # },
  # gen7 = function(X){
  #   X = combine_function(X,combine3)
  #   x = indicator_function(X,trans1,0.75)
  #   return(x)
  # },
  # gen8 = function(X){
  #   X = combine_function(X,combine4)
  #   x = indicator_function(X,trans1,0.9)
  #   return(x)
  # },
  # gen9 = function(X){
  #   X = combine_function(X,combine5)
  #   x = indicator_function(X,trans3,0.8)
  #   return(x)
  # },
  # gen10 = function(X){
  #   X = combine_function(X,combine6)
  #   x = indicator_function(X,trans3,0.1)
  #   return(x)
  # }


# 选择分布
functionSelector <- function(str,X) {
  if (str %in% names(funcList)) {
    return(funcList[[str]](X))
  } else {
    stop("Function not found")
  }
}


# 产生 tag 和 dataset
tag_type = 1 
idx = c(1:rows)
tag_list=c(1:rows)
matrix_list <- list()
for(tag in c(0:tag_type)){
  if(tag==tag_type){
    start = tag*floor(rows/(tag_type+1))+1
    end = rows
    id = c(start:end)
  }else{
    start = tag*floor(rows/(tag_type+1))+1
    end = (tag+1)*floor(rows/(tag_type+1))
    id = c(start:end)
  }
  sub_X = X[id,]
  name = paste0("gen",as.character(tag))
  sub_dataset = functionSelector(name,sub_X)
  tag_list[id]= tag
  matrix_list[[name]] <- sub_dataset
}
data<-do.call(rbind,matrix_list)

# 从Q划分训练集与测试集
split_dataset <- function(data, tag, test_rate) {
  idx_0 = which(tag==0)
  num0 = length(idx_0)
  num = length(tag)
  idx = c(1:num)
  num_test = round(num0*test_rate)
  idx_test = sample(idx_0,size = num_test,replace=F)
  train_set = data[-idx_test,]
  test_set = data[idx_test,]
  train_tag = tag[-idx_test]
  test_tag = tag[idx_test]
  return(list(train = train_set, test = test_set, train_tag = train_tag, test_tag = test_tag))
}
all_data = split_dataset(data, tag_list,0.5)

test_data = all_data$test
test_tag = all_data$test_tag
# plot_scatter(test_data, test_tag)

train_data = all_data$train
train_tag =all_data$train_tag
max_k = nrow(train_data)


# Trans-KNN
d=2
pre_y = Trans_KNN(test_data[,1:2], max_k, train_data, train_tag, tag_type,d)
sum(pre_y != test_data[,3])
1-(sum(pre_y != test_data[,3])/nrow(test_data))


# 绘制散点图
plot_scatter <- function(data, tag) {
  data[,c(3)] = as.factor(data[,c(3)])
  tag = as.factor(tag)
  ggplot(data, aes_string(x = names(data)[1], y = names(data)[2], color = names(data)[3])) +
    geom_point(shape = 1, size = 3, stroke = 1.5) +
    theme_minimal() +
    scale_shape_manual(values = c(1, 3)) +
    labs(color = "y") + 
    xlab(expression("x"[1])) +
    ylab(expression("x"[2])) + 
    theme(
      axis.title = element_text(size = 18), 
      legend.title = element_text(size = 18),  
      legend.text = element_text(size = 18)
    )
}

# 给不同数据分布绘图
n = floor(rows/(tag_type+1))
for (i in 0:tag_type){
  start = i * n + 1
  end = (i + 1) * n
  p = plot_scatter(as.data.frame(data[start:end,]), tag_list[start:end])
  
  ggsave(filename = paste0("run1_gen", i, ".png"), plot = p, width = 8, height = 6)
}
