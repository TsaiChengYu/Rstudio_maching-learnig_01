library(MASS)
data <- Boston
View(data)
tmp = is.na(data)  #find the NA in the data
ruleout = apply(tmp, 1, any)  # create the function of the col to rule out the NA
data = data[!ruleout, ]  # save the data wothout NA (col)

View(data)
# medv:自有住宅比, crim:犯罪率, zn:住宅區比, indus:工業區比, chas:是否靠河
# NOX:氮氧化物濃度, rm:住宅平均客房数, age:住戶自用比, tax:稅金
# partio: 師生比, lstat: 低收入戶, black:黑人比例 

fit <- lm(crim ~ ptratio+black+lstat+tax, data=data)
fit
summary(fit)
plot(fit)

# 製作特定表格
lstattocrim = cbind.data.frame(data$lstat,data$crim)
ggplot(crimtoblack, aes(x =data$lstat, y=data$crim)) + geom_point() + geom_smooth(method = "lm")
blacktocrim = cbind.data.frame(data$black,data$crim)
ggplot(blacktocrim,aes(x =data$black, y=data$crim)) + geom_point() + geom_smooth(method = "lm")
