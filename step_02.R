data <- iris
library("caret"); library("nnet"); library("neuralnet")
head(class.ind(data$Species))
data <- cbind(data, class.ind(data$Species))
formula.bpn <-setosa+versicolor+virginica~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width
View(data)

# 分群
smp.size <- floor(0.7*nrow(data)) 
set.seed(159)                     
train.ind <- sample(seq_len(nrow(data)), smp.size)
# 分成train/test
train <- data[train.ind, ]
test <- data[-train.ind, ]

# tune parameters
model <- train(form=formula.bpn,    
               data=train,          
               method="neuralnet",  
               #找最小的RMSE
               tuneGrid = expand.grid(.layer1=c(1:4), .layer2=c(0:4), .layer3=c(0)),               
               learningrate = 0.01,  # learning rate
               threshold = 0.01,     # partial derivatives of the error function, a stopping criteria
               stepmax = 5e5         # 最大的ieration數 = 500000(5*10^5)
)

# 會告訴你最佳的參數組合是什麼：第一隱藏層1個node，第二隱藏層2個node
model
plot(model)
bpn <- neuralnet(formula = formula.bpn, 
                 data = train,
                 hidden = c(1,3),     # 第一隱藏層1個node，第二隱藏層2個nodes
                 learningrate = 0.01, # learning rate
                 threshold = 0.01,    # partial derivatives of the error function, a stopping criteria
                 stepmax = 5e5        # 最大的ieration數
)

# 檢視模型
plot(bpn)
pred<-compute(bpn,test[,1:4])  
pred$net.result
pred.result <-round(pred$net.result)
pred.result

# 建立一個新欄位，叫做Species
pred.result <- as.data.frame(pred.result)
pred.result$Species <- ""

# 把預測結果轉回Species的型態
for(i in 1:nrow(pred.result)){
  if(pred.result[i,1]==1){ pred.result[i, "Species"] <- "setosa"}
  if(pred.result[i,2]==1){ pred.result[i, "Species"] <- "versicolor"}
  if(pred.result[i,3]==1){ pred.result[i, "Species"] <- "virginica"}
}

pred.result

# 混淆矩陣輸出
confmatrix = function(real = test$Species, predict = pred.result$Species) {
  result = table(real = test$Species, predict = pred.result$Species)  #檢視運算結果
  print(result)
  p = sum(diag(result))/sum(result)*100
  cat("\n\n預測正確率 = ",p,"% \n")
}

confmatrix(real=test$Species, predict=pred.result$Species)
