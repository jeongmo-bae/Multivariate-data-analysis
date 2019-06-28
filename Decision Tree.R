###ASSIGNMENT6
###산업경영공학부 2014170849 배정모

par(mfrow = c(2,2))
dev.off()


#data 파악하기 
heart <- read.csv("heart.csv")
View(heart)
str(heart)
summary(heart)



#categorical data   (slope과 ca는 수치형데이터로 판단했습니다 : 기울기와 (혈관의)개수 이므로)
heart$sex <- as.factor(heart$sex)
heart$cp <- as.factor(heart$cp)
heart$fbs <- as.factor(heart$fbs)
heart$restecg <- as.factor(heart$restecg)
heart$exang <- as.factor(heart$exang)
heart$thal <- as.factor(heart$thal)
heart$target <- as.factor(heart$target)

str(heart)
summary(heart)


#split dataset into training/validation set
train.idx <- sample(1:nrow(heart),200, replace = FALSE)
train <- heart[train.idx,]
valid <- heart[-train.idx,]

input.idx <- c(1:13)
train.x <-train[,input.idx]
train.y <- train[,-input.idx]
valid.x <- valid[,input.idx]
valid.y <- valid[,-input.idx]


# Performance Evaluation Function/Matrix
perf_eval <- function(cm){
  
  # True positive rate: TPR (Recall)
  TPR <- cm[2,2]/sum(cm[2,])
  #True negative rate: TNR
  TNR <- cm[1,1]/sum(cm[1,])
  # Precision
  PRE <- cm[2,2]/sum(cm[,2])
  # Accuracy
  ACC <- (cm[1,1]+cm[2,2])/sum(cm)
  # Balanced Correction Rate
  BCR <- sqrt(TPR*TNR)
  # F1-Measure
  F1 <- 2*TPR*PRE/(TPR+PRE)
  
  return(c(TPR, PRE, TNR, ACC, BCR, F1))
}

perf <- matrix(0,2,6)
colnames(perf) <- c('TPR', 'TNR', 'Precision', 'Accuracy', 'BCR', 'F1-Measure')
rownames(perf) <- c('CART', 'CART.pruned')
perf


###Q1.Before prunning
install.packages("tree")
library(tree)

# Training the tree
cart.model <- tree(target ~ ., train)
summary(cart.model)

# Plot the tree
plot(cart.model)
text(cart.model, pretty = 1)

#evluation
cart.prey <- predict(cart.model, valid, type = "class")
cart.cfm <- table(valid.y, cart.prey)
cart.cfm
perf[1,] <- perf_eval(cart.cfm)
perf






###Q2.After prunning
# Find the best tree
set.seed(12345)
cart.model.cv <- cv.tree(cart.model, FUN = prune.misclass)

# Plot the pruning result
plot(cart.model.cv$size, cart.model.cv$dev, type = "b")
cart.model.cv

# Select the final model
cart.model.pruned <- prune.misclass(cart.model, best = 8)   #best --> 내가 정한 것 
plot(cart.model.pruned)
text(cart.model.pruned, pretty = 1)

#evluation
cart.prey.pruned <- predict(cart.model.pruned, valid, type = "class")
cart.cfm.pruned <- table(valid.y, cart.prey.pruned)
cart.cfm.pruned
perf[2,] <- perf_eval(cart.cfm.pruned)
perf







###Q3
perf.packages <- matrix(0,4,6)
colnames(perf.packages) <- c('TPR', 'TNR', 'Precision', 'Accuracy', 'BCR', 'F1-Measure')
rownames(perf.packages) <- c('tree.pruned', 'rpart.pruned', 'party.pruned', 'maptree.pruned')
perf.packages

#tree결과저장 
perf.packages[1,] <- perf_eval(cart.cfm.pruned)
perf.packages

#rpart
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

rpart <- rpart(target ~., data = train, method = "class")
rpart
rpart.plot(rpart)

printcp(rpart)    #cp : complexity parameter
plotcp(rpart)

bestcp.all <- rpart$cptable[which.min(rpart$cptable[,"xerror"]),"CP"]
rpart.pruned <- prune(rpart, cp = bestcp.all)
rpart.pruned

plot(rpart.pruned)
text(rpart.pruned, pretty = 1)
rpart.plot(rpart.pruned)

rpart.prey.pruned <- predict(rpart.pruned, valid, type = "class")
rpart.cfm.pruned <- table(valid.y, rpart.prey.pruned)
rpart.cfm.pruned
perf.packages[2,] <- perf_eval(rpart.cfm.pruned)
perf.packages


#party
install.packages("party")
library(party)

party <- ctree(target ~., data = train)
plot(party)
summary(party)
party

party.prey <- predict(party, newdata = valid.x)
party.cfm <- table(valid.y, party.prey)
party.cfm
perf.packages[3,] <- perf_eval(party.cfm)
perf.packages


# MAPTREE
install.packages("maptree")
install.packages("cluster")
library(maptree)
library(cluster)

draw.tree( clip.rpart (rpart ( raw), best=7),
           nodeinfo=TRUE, units="species",
           cases="cells", digits=0)
a = agnes ( raw[2:4], method="ward" )
names(a)
a$diss
b = kgs (a, a$diss, maxclust=20)

plot(names(b), b, xlab="# clusters", ylab="penalty", type="n")
xloc = names(b)[b==min(b)]
yloc = min(b)
ngon(c(xloc,yloc+.75,10, "dark green"), angle=180, n=3)
apply(cbind(names(b), b, 3, 'blue'), 1, ngon, 4) # cbind(x,y,size,color)


#evluation
perf.packages
