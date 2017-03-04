library(rpart)
load("~/Desktop/Jeffery/程式語言/R/2017_03_03_R_teaching/titanic.raw.rdata")
str(titanic.raw)

set.seed(223)
train.index <- sample(x=1:nrow(titanic.raw), size=ceiling(0.6*nrow(titanic.raw) ))
train <- titanic.raw[train.index, ]
test <- titanic.raw[-train.index, ]

cart.model<- rpart(Survived ~. ,data=train)  # CART模型：Survived作Y，其餘作X

install.packages("rpart.plot")
library("rpart.plot") 
prp(cart.model,         
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    extra=2,    
    faclen=0
    )

pred <- predict(cart.model, newdata=test, type="class")
table(real=test$Survived, predict=pred)
confus.matrix <- table(real=test$Survived, predict=pred)
sum(diag(confus.matrix))/sum(confus.matrix) 
