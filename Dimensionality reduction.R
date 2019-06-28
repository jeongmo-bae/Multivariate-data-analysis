###ASSIGNMENT5
###산업경영공학부 2014170849 배정모

par(mfrow = c(2,2))
dev.off()



#data 파악하기 
ankara <- read.csv("Weather_Ankara.csv")
View(ankara)
head(ankara,10)
tail(ankara,10)
dim(ankara)
nrow(ankara)
ncol(ankara)
names(ankara)
str(ankara)
summary(ankara)
attach(ankara)

#data split into training/validation set
             
             #install.packages("caTools")
             #library('caTools')
             #set.seed(1234)
             #split<-sample.split(ankara$Mean_temperature, SplitRatio = 0.78)
             #train<-subset(ankara,split==TRUE)
             #test<-subset(ankara,split==FALSE)

trn_index <- sample(1:nrow(ankara),251)
train <- ankara[trn_index,]
train_x <- ankara[trn_index,-10]
valid <- ankara[-trn_index,]
valid_y <- ankara[-trn_index,"Mean_temperature"]



#Q1
perf_eval_reg <- function(tgt_y, pre_y){
  
  # RMSE
  rmse <- sqrt(mean((tgt_y - pre_y)^2))
  # MAE
  mae <- mean(abs(tgt_y - pre_y))
  # MAPE
  mape <- 100*mean(abs((tgt_y - pre_y)/tgt_y))
  
  return(c(rmse, mae, mape))
  
}

perf_mat <- matrix(0,6,4)
colnames(perf_mat) <- c("Time", "RMSE", "MAE", "MAPE")
rownames(perf_mat) <- c("Full MLR","Exhaustive Search","Forward Selection","Backward Elimination","Stepwise Selection","Genetic Algorithm" )


start <- Sys.time()
mlr_full <- lm(Mean_temperature ~ ., train)
end <- Sys.time()
summary(mlr_full)

predict_y <- predict(mlr_full, newdata = valid)

perf_mat[1,1] <- end-start
perf_mat[1,2:4] <- perf_eval_reg(valid_y,predict_y)
perf_mat



#Q2 (DFS 알고리즘을 통한 함수 생성부분은 2013170835 배노협 에게 도움받았습니다.)
iMatrix <<- 1:length(train_x)     
dfs <- function(idx,arr){
  if(idx==length(arr)+1){
    iMatrix <<- cbind(iMatrix,arr)
    return()
  }
  arr[idx] <- 0
  dfs(idx+1, arr)
  arr[idx] <- 1
  dfs(idx+1, arr)
}
dfs(1, 1:length(train_x))
iMatrix <- data.frame(iMatrix[,-c(1,2)]) 

exhaustive_search <- function(){
  start_time<-Sys.time()
  r.squared <- rep(0:9)
  names(r.squared) <- c("Max_termperature","Min_temperature","Dewpoint","Precipitation","Sea_level_pressure",
                        "Standard_pressure","Visibility","Wind_speed","Max_wind_speed","ADJ R")
  for(i in 1:511){
    tmp_x <- paste(colnames(train_x)[which(iMatrix[i]==1)],collapse=" + ")
    string <- paste("Mean_temperature ~ ", tmp_x, collapse = "")
    model <- lm(as.formula(string), data = train)
    
    r.squared <- rbind(r.squared, c(iMatrix[,i],round(summary(model)$adj.r.squared,4)))
  }
  end_time<-Sys.time()
  perf_mat[2,1] <<- end_time-start_time
  return(r.squared)
}

result <- exhaustive_search()
result <- result[-1,]
result
result[order(result[,10] , decreasing = TRUE),]

mlr_exhaustive <- lm(Mean_temperature ~ Max_termperature + Min_temperature + Dewpoint + Sea_level_pressure
                    + Standard_pressure + Visibility + Wind_speed , train)
summary(mlr_exhaustive)
predict_y_exhausted <- predict(mlr_exhaustive, newdata = valid)

perf_mat[2,2:4] <- perf_eval_reg(valid_y,predict_y_exhausted)
perf_mat



#Q3
##Forward selection
start <- Sys.time()
mlr_forward <- step(lm(Mean_temperature ~ 1, data = train), 
                      scope = list(upper = mlr_full , lower = Mean_temperature ~ 1), 
                      direction="forward", trace = 1)
end <-Sys.time()

summary(mlr_forward)

predict_y_forward <- predict(mlr_forward, newdata = valid)

perf_mat[3,1] <- end - start
perf_mat[3,2:4] <- perf_eval_reg(valid_y, predict_y_forward)
perf_mat


##Backward elimination
start <- Sys.time()
mlr_backward <- step(mlr_full, 
                    scope = list(upper = mlr_full , lower = Mean_temperature ~ 1), 
                    direction="backward", trace = 1)
end <-Sys.time()

summary(mlr_backward)

predict_y_backward <- predict(mlr_backward, newdata = valid)

perf_mat[4,1] <- end - start
perf_mat[4,2:4] <- perf_eval_reg(valid_y, predict_y_backward)
perf_mat


## Stepwise selection
start <- Sys.time()
mlr_stepwise <- step(lm(Mean_temperature ~ 1, data = train), 
                     scope = list(upper = mlr_full , lower = Mean_temperature ~ 1), 
                     direction="both", trace = 1)
end <-Sys.time()

summary(mlr_stepwise)

predict_y_stepwise <- predict(mlr_stepwise, newdata = valid)

perf_mat[5,1] <- end - start
perf_mat[5,2:4] <- perf_eval_reg(valid_y, predict_y_stepwise)
perf_mat



#Q4  (함수 작성부분에서 2016170855 김지영 학우의 도움을 받았습니다.)
##Genetic Algorithm
install.packages("GA")
library(GA)
fit_F1 <- function(string){
  sel_var_idx <- which(string == 1) 
 
  sel_x <- x[, sel_var_idx] 
  xy <- data.frame(sel_x, y)
  
  GA_lr <- lm(y ~ ., data = xy)
  GA_lr_prey <- predict(GA_lr, newdata = xy) 
  
  return(summary(GA_lr)$adj.r.squared) 
}

x <- as.matrix(train_x)
y <- train[,10]


start_time <- Sys.time()
ga_f1 <- ga(type = "binary", fitness = fit_F1, nBits=ncol(x),
            names = colnames(x), popSize = 100, pcrossover = 0.5, 
            pmutation = 0.01, maxiter = 5, elitism = 2, seed = 34525)
end_time <- Sys.time()



var_idx <- which(ga_f1@solution == 1)
var_idx

ga_train <- train[,c(var_idx, 10)]
ga_valid <- valid[,c(var_idx, 10)]

ga_model <- lm(Mean_temperature ~ ., ga_train)
summary(ga_model)


predict_y_ga <- predict(ga_model, newdata = ga_valid)

perf_mat[6,1] <- end_time - start_time
perf_mat[6,2:4] <- perf_eval_reg(valid_y, predict_y_ga)



#Q5

ga_1 <- ga(type = "binary", fitness = fit_F1, nBits=ncol(x),
            names = colnames(x), popSize = 10, pcrossover = 0.5, 
            pmutation = 0.01, maxiter = 100, elitism = 2, seed = 123)
var_idx1 <- which(ga_1@solution == 1)
var_idx1



ga_2 <- ga(type = "binary", fitness = fit_F1, nBits=ncol(x),
           names = colnames(x), popSize = 50, pcrossover = 0.5, 
           pmutation = 0.01, maxiter = 100, elitism = 2, seed = 123)
var_idx2 <- which(ga_2@solution == 1)
var_idx2





ga_3 <- ga(type = "binary", fitness = fit_F1, nBits=ncol(x),
           names = colnames(x), popSize = 100, pcrossover = 0.5, 
           pmutation = 0.01, maxiter = 100, elitism = 2, seed = 123)
var_idx3 <- which(ga_3@solution == 1)
var_idx3



ga_4 <- ga(type = "binary", fitness = fit_F1, nBits=ncol(x),
           names = colnames(x), popSize = 100, pcrossover = 0.1, 
           pmutation = 0.01, maxiter = 100, elitism = 2, seed = 123)
var_idx4 <- which(ga_4@solution == 1)
var_idx4





ga_5 <- ga(type = "binary", fitness = fit_F1, nBits=ncol(x),
           names = colnames(x), popSize = 100, pcrossover = 0.3, 
           pmutation = 0.01, maxiter = 100, elitism = 2, seed = 123)
var_idx5 <- which(ga_5@solution == 1)
var_idx5




ga_6 <- ga(type = "binary", fitness = fit_F1, nBits=ncol(x),
           names = colnames(x), popSize = 100, pcrossover = 0.5, 
           pmutation = 0.01, maxiter = 100, elitism = 2, seed = 123)
var_idx6 <- which(ga_6@solution == 1)
var_idx6





ga_7 <- ga(type = "binary", fitness = fit_F1, nBits=ncol(x),
           names = colnames(x), popSize = 100, pcrossover = 0.5, 
           pmutation = 0.01, maxiter = 3, elitism = 2, seed = 123)
var_idx7 <- which(ga_7@solution == 1)
var_idx7






ga_8 <- ga(type = "binary", fitness = fit_F1, nBits=ncol(x),
           names = colnames(x), popSize = 100, pcrossover = 0.5, 
           pmutation = 0.01, maxiter = 5, elitism = 2, seed = 123)
var_idx8 <- which(ga_8@solution == 1)
var_idx8



ga_9 <- ga(type = "binary", fitness = fit_F1, nBits=ncol(x),
           names = colnames(x), popSize = 100, pcrossover = 0.5, 
           pmutation = 0.01, maxiter = 10, elitism = 2, seed = 123)
var_idx9 <- which(ga_9@solution == 1)
var_idx9



ga_10 <- ga(type = "binary", fitness = fit_F1, nBits=ncol(x),
           names = colnames(x), popSize = 100, pcrossover = 0.5, 
           pmutation = 0.01, maxiter = 15, elitism = 2, seed = 123)
var_idx10 <- which(ga_10@solution == 1)
var_idx10


ga_11 <- ga(type = "binary", fitness = fit_F1, nBits=ncol(x),
            names = colnames(x), popSize = 100, pcrossover = 0.5, 
            pmutation = 0.01, maxiter = 100, elitism = 2, seed = 123)
var_idx11 <- which(ga_10@solution == 1)
var_idx11




for (i in 1){
  print("Change pop size : 10 -> 50 -> 100")
  print(var_idx1)
  print(var_idx2)
  print(var_idx3)
}
  

for (i in 1){
  print("Change pcrossover : 0.1 -> 0.3 -> 0.5")
  print(var_idx4)
  print(var_idx5)
  print(var_idx6)
}

for (i in 1){
  print("Change maxiteration : 3 -> 5 -> 10 -> 15 -> 100")
  print(var_idx7)
  print(var_idx8)
  print(var_idx9)
  print(var_idx10)
  print(var_idx11)
}


