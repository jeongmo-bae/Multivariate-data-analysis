#ASSIGNMENT3
#산업경영공학부 2014170849 배정모
par(mfrow = c(2,2))
dev.off()

#Q1
house <- read.csv("kc_house_data.csv")
house <- house[,-c(1,2,17)]
View(house)
col<-colnames(house)
str(house)
summary(house)


#Q2
library(MASS)
install.packages("moments")
library(moments)

for (i in 2:18){
  boxplot(house[,i],main=colnames(house)[i],horizontal=T)
  hist(house[,i], prob=TRUE, xlab="x-variable", main=col[i])
  curve(dnorm(x, mean=mean(house[,i]), sd=sd(house[,i])), 
        col="darkblue", lwd=2, add=TRUE, yaxt="n")
  print(col[i])
  print(mean(house[,i]))
  print(sd(house[,i]))
  print(skewness(house[,i]))
  print(kurtosis(house[,i]))
  print('------------------')
}



#Q3
#2013170835 배노협에게 도움 받았습니다. ( outlier를 NA로 만들어주는 함수 생성)
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
for(i in c(1,2,3,4,5,6,9,10,11,12,13,15,16,17,18)){
  house[,i] = remove_outliers(house[,i])
}
house_o<-na.omit(house)
str(house_o)

#Q4
plot(house_o)
cor<-cor(house_o)
cor
install.packages('corrplot')
library(corrplot)
corrplot(cor,method='color',addCoef.col='black')

#Q5,6
library('caTools')
set.seed(123)
split<-sample.split(house_o$price, SplitRatio = 0.7)
house_o_train<-subset(house_o,split==TRUE)
house_o_test<-subset(house_o,split==FALSE)

str(house_o_train)

lm_house<-lm(price~.,data = house_o_train)
summary(lm_house)
plot(lm_house)


#7
perf_eval_reg <- function(tgt_y, pre_y){
  
  # RMSE
  rmse <- sqrt(mean((tgt_y - pre_y)^2))
  # MAE
  mae <- mean(abs(tgt_y - pre_y))
  # MAPE
  mape <- 100*mean(abs((tgt_y - pre_y)/tgt_y))
  
  return(c(rmse, mae, mape))
  
}

perf_mat <- matrix(0, nrow = 2, ncol = 3)
perf_mat

rownames(perf_mat) <- c("all-variables", "7-variables")
colnames(perf_mat) <- c("RMSE", "MAE", "MAPE")
perf_mat

mlr_house_haty <- predict(lm_house, newdata = house_o_test)

perf_mat[1,] <- perf_eval_reg(house_o_test$price, mlr_house_haty)
perf_mat


#8
lm_house2<-lm(price~waterfront+view+condition+grade+yr_built+lat+sqft_living15,data = house_o_train)
summary(lm_house2)


#9
mlr_house_haty <- predict(lm_house2, newdata = house_o_test)

perf_mat[2,] <- perf_eval_reg(house_o_test$price, mlr_house_haty)
perf_mat
