###ASSIGNMENT4
###산업경영공학부 2014170849 배정모

par(mfrow = c(2,2))
dev.off()

#data 파악하기
adm<-read.csv('Admission_Predict.csv')

View(adm)
head(adm,10)
tail(adm,10)
dim(adm)
nrow(adm)
ncol(adm)
names(adm)
str(adm)
summary(adm)


###Q1
df_adm<-adm[,-1]   #Serial.No는 data의 순서를 매긴 것일 뿐 이므로 predict에 유의미하지 않은 변수이다. 제거! 
View(df_adm)


###Q2
library(MASS)
install.packages("moments")
library(moments)

col<-colnames(df_adm[,-8])

for (i in col){
  boxplot(df_adm[,i],main=i,horizontal=T)
  hist(df_adm[,i], prob=TRUE, xlab="x-variable", main=i)
  curve(dnorm(x, mean=mean(df_adm[,i]), sd=sd(df_adm[,i])), col="darkblue", lwd=2, add=TRUE, yaxt="n")
  
  print(i)
  print(paste(c("Mean : ",mean(df_adm[,i])),collapse=""))
  print(paste(c("Standard deviation : ",sd(df_adm[,i])),collapse=""))
  print(paste(c("Skewness : ",skewness(df_adm[,i])),collapse=""))
  print(paste(c("Kurtosis : ",kurtosis(df_adm[,i])),collapse=""))
  print('------------------')
}



###Q3
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
for(i in c(1,2,3,4,5,6)){
  df_adm[,i] = remove_outliers(df_adm[,i])
}
df_adm_o<-na.omit(df_adm)
str(df_adm_o)


###Q4
pairs(df_adm_o[,1:7],main = "variables pair plot")  
     #plot(df_adm_o[,1:7], main = "variables pair plot") 똑같음.  

install.packages('corrplot')
library(corrplot)
cor<-cor(df_adm_o[,1:7])
cor
corrplot(cor,method='color',addCoef.col='black')


###Q5
#<1>chance of admit 0,1로 변환 
input_idx <- c(1,2,3,4,5,6,7)
target_idx <- 8
for (i in 1:nrow(df_adm_o)){
                            if (df_adm_o[i,target_idx]>0.8){df_adm_o[i,target_idx]<-1}
                            else{df_adm_o[i,target_idx]<-0}
                            }
View(df_adm_o)

#<2>input data normalization
df_adm_o[,input_idx]<-scale(df_adm_o[,input_idx], center = TRUE, scale = TRUE)    #scale : 정규화 

#<3>split data into trainingset/validation set
library('caTools')
set.seed(1234)
split<-sample.split(df_adm_o$Chance.of.Admit, SplitRatio = 0.7)
adm_train<-subset(df_adm_o,split==TRUE)
adm_test<-subset(df_adm_o,split==FALSE)

#<4>conduct the Logistic Regression Model with all variables
full_lr <- glm(Chance.of.Admit ~ ., family=binomial, adm_train)
summary(full_lr)


###Q6
#<1>test set으로 예측
lr_response <- predict(full_lr, type = "response", newdata = adm_test)
lr_target<-adm_test$Chance.of.Admit
lr_predicted<-rep(0,length(lr_target))
lr_predicted[which(lr_response>0.8)]<-1
cm_full<-table(lr_target, lr_predicted)
cm_full

#<2>confusion matrix 결과 해석
perf_eval2 <- function(cm){                           #cm = confusion matrix   
  TPR <- cm[2,2]/sum(cm[2,])                          # True positive rate: TPR (Recall)
  
  TNR <- cm[1,1]/sum(cm[1,])                          # True negative rate: TNR
  
  FPR <- cm[1,2]/sum(cm[1,])
  
  FNR <- cm[2,1]/sum(cm[2,])
  
  PRE <- cm[2,2]/sum(cm[,2])                          # Precision
  
  ACC <- (cm[1,1]+cm[2,2])/sum(cm)                    # Simple Accuracy
 
  BCR <- sqrt(TPR*TNR)                              # Balanced Correction Rate
  
  F1 <- 2*TPR*PRE/(TPR+PRE)                         # F1-Measure
  
  return(c(TPR, TNR, FPR, FNR, ACC, BCR, F1))
}

perf_mat <- matrix(0, 1, 7)
colnames(perf_mat) <- c("TPR (Recall)", "TNR", "FPR", "FNR", "ACC", "BCR", "F1")
rownames(perf_mat) <- "Logstic Regression-full"
perf_mat

perf_mat[1,] <- perf_eval2(cm_full)
perf_mat



###Q7
getROC_AUC = function(probs, true_Y){
  probsSort = sort(probs, decreasing = TRUE, index.return = TRUE)
  val = unlist(probsSort$x)
  idx = unlist(probsSort$ix)  
  
  roc_y = true_Y[idx];
  stack_x = cumsum(roc_y == 0)/sum(roc_y == 0)
  stack_y = cumsum(roc_y == 1)/sum(roc_y == 1)    
  
  auc = sum((stack_x[2:length(roc_y)]-stack_x[1:length(roc_y)-1])*stack_y[2:length(roc_y)])
  return(list(stack_x=stack_x, stack_y=stack_y, auc=auc))
}


set.seed(873462)
split<-sample.split(df_adm_o$Chance.of.Admit, SplitRatio = 0.7)
adm_train2<-subset(df_adm_o,split==TRUE)
adm_test2<-subset(df_adm_o,split==FALSE)
full_lr2 <- glm(Chance.of.Admit ~ ., family=binomial, adm_train2)
lr_response2 <- predict(full_lr2, type = "response", newdata = adm_test2)
lr_target2<-adm_test2$Chance.of.Admit


true_Y = lr_target2
probs = lr_response2
aList = getROC_AUC(probs, true_Y) 

stack_x = unlist(aList$stack_x)
stack_y = unlist(aList$stack_y)
auc = unlist(aList$auc)

plot(stack_x, stack_y, type = "l", col = "blue", xlab = "False Positive Rate", ylab = "True Positive Rate", main = "set.seed=873462//ROC")
axis(1, seq(0.0,1.0,0.1))
axis(2, seq(0.0,1.0,0.1))
abline(h=seq(0.0,1.0,0.1), v=seq(0.0,1.0,0.1), col="gray", lty=3)
legend(0.7, 0.3, sprintf("%3.3f",auc), lty=c(1,1), lwd=c(2.5,2.5), col="blue", title = "AUC")


###Q8

loan_data<-read.csv("loan_data.csv")
View(loan_data)
dim(loan_data)
summary(loan_data)

#data 자료형 변형 
loan_data$COMB_COMM<-as.factor(loan_data$COMB_COMM)
loan_data$TARGET<-as.factor(loan_data$TARGET)

str(loan_data)
attach(loan_data)


#training/test set나누기
library(caTools)
set.seed(100)
split <- sample.split(loan_data$TARGET,SplitRatio=0.7)
loan_train <- subset(loan_data, split == TRUE)
loan_test = subset(loan_data, split == FALSE)


#logistic regression full model 구축
modLog = glm(TARGET~. , data=loan_train, family="binomial")
summary(modLog)

#iteration
modLog2 = glm(TARGET ~ TOT_LOAN + LOAN_BNK + LOAN_CPT + CRDT_CNT + GUARN_CNT + INCOME + LATE_RATE_1Y 
              + CANCEL_CNT_1Y + MOBILE_PRICE + SUSP_DAY + LATE_TEL + COMB_COMM + PAY_METHOD , 
              data=loan_train, family="binomial")
summary(modLog2)

#prediction
loan_test$predicted.risk = predict(modLog2, newdata=loan_test, type="response")
mat_loan<-table(loan_test$TARGET, as.numeric(loan_test$predicted.risk >= 0.5))
mat_loan


#cut-off value 조절
mat_loan<-table(loan_test$TARGET, as.numeric(loan_test$predicted.risk >=0.4))
mat_loan

mat_loan<-table(loan_test$TARGET, as.numeric(loan_test$predicted.risk >=0.3))
mat_loan

mat_loan<-table(loan_test$TARGET, as.numeric(loan_test$predicted.risk >=0.2))
mat_loan




#evaluation
perf_eval_loan <- function(cm){                           #cm = confusion matrix   
  TPR <- cm[2,2]/sum(cm[2,])                          # True positive rate: TPR (Recall)
  
  TNR <- cm[1,1]/sum(cm[1,])                          # True negative rate: TNR
  
  FPR <- cm[1,2]/sum(cm[1,])
  
  FNR <- cm[2,1]/sum(cm[2,])
  
  PRE <- cm[2,2]/sum(cm[,2])                          # Precision
  
  ACC <- (cm[1,1]+cm[2,2])/sum(cm)                    # Simple Accuracy
  
  BCR <- sqrt(TPR*TNR)                              # Balanced Correction Rate
  
  F1 <- 2*TPR*PRE/(TPR+PRE)                         # F1-Measure
  
  return(c(TPR, TNR, FPR, FNR, ACC, BCR, F1))
}

perf_mat_loan <- matrix(0, 1, 7)
colnames(perf_mat_loan) <- c("TPR (Recall)", "TNR", "FPR", "FNR", "ACC", "BCR", "F1")
rownames(perf_mat_loan) <- "Logstic Regression-loand_ata"
perf_mat_loan

perf_mat_loan[1,] <- perf_eval_loan(mat_loan)
perf_mat_loan

