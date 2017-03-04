# step 1
primary_data = cbind.data.frame(sleep_data$id, sleep_data$sex, sleep_data$age, sleep_data$waist, sleep_data$neck, 
    sleep_data$weight, sleep_data$BMI, sleep_data$Apneas, sleep_data$Hypopneas, sleep_data$NO2, 
    sleep_data$O3, sleep_data$PM10, sleep_data$PM2_5, sleep_data$RH, sleep_data$total_AHI)
# 將要分析的項目，獨立出表格
colnames(primary_data) = c("id", "sex", "age", "waist", "neck", "weight", "BMI", "Apneas", "Hypopneas", 
    "NO2", "O3", "PM10", "PM2.5", "RH", "total_AHI")  # 將項目，加上說明

tmp = is.na(primary_data)  #find the NA in the data
ruleout = apply(tmp, 1, any)  # create the function of the col to rule out the NA
na_out_primary_data = primary_data[!ruleout, ]  # save the data wothout NA (col)

#-------------------------------------------------------------------------#
# step 2 將表格轉成數值
sex = na_out_primary_data[,2]
age = na_out_primary_data[,3]
waist = na_out_primary_data[,4]
neck = na_out_primary_data[,5]
weight = na_out_primary_data[,6]
BMI = na_out_primary_data[,7]
Apneas = na_out_primary_data[,8]
Hypopneas = na_out_primary_data[,9]
NO2 = na_out_primary_data[,10]
O3 = na_out_primary_data[,11]
PM10 = na_out_primary_data[,12]
PM2.5 = na_out_primary_data[,13]
RH = na_out_primary_data[, 14]
total_AHI = na_out_primary_data[,15]

# 運用plot 來顯示出圖表，找出離群數值
plot(sex)
plot(age)
plot(waist)
plot(neck)
plot(weight)
plot(BMI)
plot(total_AHI)
plot(Apneas)
plot(Hypopneas)
plot(NO2)
plot(O3)
plot(PM10)
plot(PM2.5)
plot(RH)
# 發現total_AHI 有錯誤(>200); 發現BMI有錯誤(<10 && >50);發現weight有錯誤(<30);
# 發現Hypopneas有錯誤(>500); 發現有waist錯誤(>150); 發現Neck有錯誤(>60);

# 將資料中不要的資訊排除（離群值）：分項移除
T_na_out_primary_data = subset(na_out_primary_data, na_out_primary_data$total_AHI < 200)
T_na_out_primary_data = subset(T_na_out_primary_data, T_na_out_primary_data$BMI > 10)
T_na_out_primary_data = subset(T_na_out_primary_data, T_na_out_primary_data$BMI < 50)
T_na_out_primary_data = subset(T_na_out_primary_data, T_na_out_primary_data$neck < 60)
T_na_out_primary_data = subset(T_na_out_primary_data, T_na_out_primary_data$weight > 30)
T_na_out_primary_data = subset(T_na_out_primary_data, T_na_out_primary_data$Hypopneas < 500)
T_na_out_primary_data = subset(T_na_out_primary_data, T_na_out_primary_data$waist < 150)

# step 3 將表格轉成數值再次檢查
sex = T_na_out_primary_data[, 2]
age = T_na_out_primary_data[, 3]
waist = T_na_out_primary_data[, 4]
neck = T_na_out_primary_data[, 5]
weight = T_na_out_primary_data[, 6]
BMI = T_na_out_primary_data[, 7]
Apneas = T_na_out_primary_data[, 8]
Hypopneas = T_na_out_primary_data[, 9]
NO2 = T_na_out_primary_data[, 10]
O3 = T_na_out_primary_data[, 11]
PM10 = T_na_out_primary_data[, 12]
PM2.5 = T_na_out_primary_data[, 13]
RH = T_na_out_primary_data[, 14]
total_AHI = T_na_out_primary_data[, 15]

# 運用plot 來顯示出圖表確認
plot(sex)
plot(age)
plot(waist)
plot(neck)
plot(weight)
plot(BMI)
plot(total_AHI)
plot(Apneas)
plot(Hypopneas)
plot(NO2)
plot(O3)
plot(PM10)
plot(PM2.5)
plot(RH)

# 將data 作圖 並標上顏色區分
hist(total_AHI, col = ifelse(total_AHI > median(total_AHI), "red", "blue"))

# 製造表格(ROI)
BMI.total_AHI = cbind.data.frame(T_na_out_primary_data$BMI, T_na_out_primary_data$total_AHI)
tmp = is.na(BMI.total_AHI)
ruleout = apply(tmp, 1, any)
BMI.total_AHI = BMI.total_AHI[!ruleout, ]
ggplot(BMI.total_AHI, aes(x = BMI, y = total_AHI)) + geom_point() + geom_smooth(method = "lm")

# ____________________________________________________________________________________
# 開始進行人工智慧網路建構 install 所需要的packages
install.packages("caret")
install.packages("nnet")
install.packages("neuralnet")

library("caret")
library("nnet")
library("neuralnet")

# 資料加上嚴重程度分類
T_na_out_primary_data$classification <- ""
for (i in 1:nrow(T_na_out_primary_data)) {
    if (T_na_out_primary_data[i, 15] >= 30) {
        T_na_out_primary_data[i, "classification"] <- "Severe"
    } else if (T_na_out_primary_data[i, 15] < 30 && T_na_out_primary_data[i, 15] >= 15) {
        T_na_out_primary_data[i, "classification"] <- "Moderate"
    } else if (T_na_out_primary_data[i, 15] < 15 && T_na_out_primary_data[i, 15] >= 5) {
        T_na_out_primary_data[i, "classification"] <- "Mild"
    } else {
        T_na_out_primary_data[i, "classification"] <- "Normal"
    }
}

# 將分類結果數據化：將結果轉變成啞變數(dummy variables)
head(class.ind(T_na_out_primary_data$classification))
T_na_out_primary_data <- cbind(T_na_out_primary_data, class.ind(T_na_out_primary_data$classification))
head(T_na_out_primary_data)

# 計算母群體嚴重程度分類
normal_p = 0
mild_p = 0
moderate_p = 0
severe_p = 0

for (i in 1:nrow(T_na_out_primary_data))
{
  if(T_na_out_primary_data$Mild[i] == 1){ mild_p= mild_p+1 }
  if(T_na_out_primary_data$Moderate[i] == 1){ moderate_p = moderate_p+1 }
  if(T_na_out_primary_data$Severe[i] == 1){ severe_p = severe_p+1 }
  if(T_na_out_primary_data$Normal[i] == 1){ normal_p = normal_p+1 }
}

population = data.frame( degree=c("normal", "mild", "modreate", "severe"),
                         data = c(normal_p,mild_p,moderate_p,severe_p))
ggplot(data=population) +  # 先畫bar plot
  geom_bar(aes(x=factor(1),
               y=data,
               fill= degree),
           stat = "identity") +coord_polar("y", start=0)   # 再沿著Y，轉軸成圓餅圖

# 建立迴歸分析函式
ahi_class<- as.formula('Mild+Moderate+Normal+Severe~sex+age+waist+neck+weight+
                      BMI+Apneas+Hypopneas+NO2+O3+PM10+PM2.5+RH')
ahi_num<- as.formula('Mild+Moderate+Normal+Severe~sex+age+waist+neck+weight+
                     BMI+Apneas+Hypopneas+NO2+O3+PM10+PM2.5+RH')

# 區分training set and data set
smp.size <- floor(0.8*nrow(T_na_out_primary_data))
set.seed(500)
train.ind <- sample(seq_len(nrow(T_na_out_primary_data)), smp.size)
train <- T_na_out_primary_data[train.ind, ]
test <- T_na_out_primary_data[-train.ind, ]

# 1.建立類神經網路(BPN)
library("nnet") 
library("neuralnet")
library("caret")
  model_1 <- train(form = ahi, 
                   data = train,
                   method='neuralnet',
                   # 觀察不同排列組合(第一層1~7個nodes ; 第二層0~15個nodes;第三層1~7個nodes)
                   # 看何種1排列組合(多少hidden layer、每層多少個node)，會有最小的RMSE
                   tuneGrid=expand.grid(.layer1=c(1:10), .layer2=c(1:8), .layer3=c(1:6)),# hidden layer node number
                   learningrate = 0.01, # learning rate
                   threshold = 0.01,    # partial derivatives of the error function, a s  1 topping criteria
                   stepmax = 10e8        # 最大的ieration數 = 10*10^8)
  )

model_1  # 查看 hidden layer 的選擇
plot(model_1) # 圖表計算結果

#RMSE was used to select the optimal model using  the smallest value.
#The final values used for the model were layer1 = Ｘ, layer2 = Ｙ and layer3 = Ｚ 

#使用backprogation learing
model_train<- neuralnet(
                 form = ahi, 
                 data = train,
                 hidden = c(1,3,1),     # 第一隱藏層1個node，第二隱藏層2個nodes
                 learningrate = 0.02, # learning rate
                 threshold = 0.01,    # partial derivatives of the error function, a stopping criteria
                 stepmax = 10e8        # 最大的ieration數 = 10*10^8)
                 
)

model_train  # 查看模型結果
plot(model_train)  # 圖表計算結果
pred<-compute(model_train, test[ ,2:14])#輸入test set後進行預測(輸入資料只能包含input node的值欄位)
pred$net.result   # 觀看預測結果

#_______________________________________________________________________________#

# 2. 使用SVM模型進行預測分析
library(e1071)
model.svm <- svm(train$classification~., data = train)
model.svm = svm(formula = ahi, data = train)
print(result)
summary(result)
# 訓練樣本預測正確率
pred_result_svm <- predict(model.svm, test)
table(pred_result_svm, test$classification  )

-----------------------------------------------------------------------------
# 進行資料處理
pred.result <- (pred$net.result)
  for(i in 1:nrow(pred.result))
    {
      tmp = max(pred.result[i,]);
      if(pred.result[i,1]< tmp){ pred.result[i,1] = 0}
      if(pred.result[i,2]< tmp){ pred.result[i,2] = 0}
      if(pred.result[i,3]< tmp){ pred.result[i,3] = 0}
      if(pred.result[i,4]< tmp){ pred.result[i,4] = 0}
      tmp2 = which.is.max(pred.result[i,])
      pred.result[i,tmp2] = 1
    }
 
pred.result  # 查看結果
pred.result <- as.data.frame(pred.result)  #將結果轉換成表格輸出
pred.result$classification<-""   #新增空白列

for(i in 1:nrow(pred.result))
  {
  if(pred.result[i,1]==1){ pred.result[i, "classification"] <- "Mild"}
  if(pred.result[i,2]==1){ pred.result[i, "classification"] <- "Moderate"}
  if(pred.result[i,3]==1){ pred.result[i, "classification"] <- "Normal"}
  if(pred.result[i,4]==1){ pred.result[i, "classification"] <- "Severe"}
  }
# 將運算結果，還原為狀況
 
pred.result
result = table(real = test$classification, predict = pred.result$classification)  #檢視運算結果
result
confmatrix(real = test$classification, predict = pred.result$classification)


# 混淆矩陣輸出
confmatrix = function(real = test$classification, predict = pred.result$classification) {
   result = table(real = test$classification, predict = pred.result$classification)  #檢視運算結果
   print(result)
   p = sum(diag(result))/sum(result)*100
   cat("\n\n預測正確率 = ",p,"% \n")
}

