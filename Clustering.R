#ASSIGNMENT2
#산업경영공학부 2014170849 배정모

install.packages("ISLR")
library(ISLR)
data(College)
View(College)

##K-means klustering
college_x <- College[,-1]
college_scaled <- scale(college_x, center = TRUE, scale = TRUE)

#Q1_1
install.packages("clValid")
install.packages("plotrix")
library(clValid)
library(plotrix)

start <- Sys.time() #시작시간 체크 
college_clValid <- clValid(college_scaled, 2:10, clMethods = "kmeans", 
                        validation = c("internal", "stability"),maxitems = 800)
end <- Sys.time()   #종료시간 체크 
summary(college_clValid)
end-start


#Q1_2

for (i in 1:10){
  College_kmc <- kmeans(college_scaled,3)
  print(College_kmc$centers)
  print(College_kmc$size)          #for문 내에선 print없이 출력 불가
}


#Q1_3
for (i in 1:10){
  
  College_kmc <- kmeans(college_scaled,10)
  print(College_kmc$centers)
  print(College_kmc$size)          
}


#Q1_4
college_kmc <- kmeans(college_scaled,3)  #k=3으로 군집화 수행

cluster_kmc <- data.frame(college_scaled, clusterID = as.factor(college_kmc$cluster))
kmc_summary <- data.frame()

for (i in 1:(ncol(cluster_kmc)-1)){
  kmc_summary = rbind(kmc_summary,tapply(cluster_kmc[,i], cluster_kmc$clusterID, mean))
}
colnames(kmc_summary) <- paste("cluster", c(1:3))
rownames(kmc_summary) <- colnames(college_x)
kmc_summary


par(mfrow = c(1,3))
for (i in 1:3){
  plot_title <- paste("Radar Chart for Cluster", i, sep=" ")
  radial.plot(kmc_summary[,i], labels = rownames(kmc_summary), 
              radial.lim=c(-2,2), rp.type = "p", main = plot_title, 
              line.col = "red", lwd = 3, show.grid.labels=1)     ### Radar chart        
}
dev.off()


#Q1_5
kmc_cluster1 <- college_x[college_kmc$cluster == 1,]  
kmc_cluster2 <- college_x[college_kmc$cluster == 2,]
kmc_cluster3 <- college_x[college_kmc$cluster == 3,]

#cluster1 vs cluster2 t-test
kmc_t_result <- data.frame()  

for (i in 1:17){
  
  kmc_t_result[i,1] <- t.test(kmc_cluster1[,i], kmc_cluster2[,i], 
                              alternative = "two.sided")$p.value   #양측검정
}

kmc_t_result

# cluster1 vs cluster3 t-test
kmc_t_result <- data.frame()  

for (i in 1:17){
  
  kmc_t_result[i,1] <- t.test(kmc_cluster1[,i], kmc_cluster3[,i], 
                              alternative = "two.sided")$p.value   #양측검정
}

kmc_t_result

#cluster2 vs cluster3 t-test
kmc_t_result <- data.frame()  

for (i in 1:17){
  
  kmc_t_result[i,1] <- t.test(kmc_cluster2[,i], kmc_cluster3[,i], 
                              alternative = "two.sided")$p.value   #양측검정
}

kmc_t_result

#Q1-6
install.packages("ggfortify")
install.packages("colorspace")
library(colorspace)
library(ggplot2)
library(ggfortify)
autoplot(prcomp(cluster_kmc[,1:17]),colour=cluster_kmc$clusterID, loadings = TRUE,
         loadings.colour = 'blue', loadings.label = TRUE, loadings.label.size = 3)



#Hierarchical Clustering 

#Q2_1

start <- Sys.time()
college_clValid <- clValid(college_scaled, 2:10, clMethods = "hierarchical", 
                           validation = c("internal", "stability"),maxitems = 800) 
end <- Sys.time()
summary(college_clValid)
end - start


#Q2-2
cor_Mat <- cor(t(college_scaled), method = "spearman") 
dist_college <- as.dist(1-cor_Mat) # Distance = 1-Correlation 
str(dist_college)


hr <- hclust(dist_college, method = "single", members=NULL)     
plot(hr, labels=FALSE, main="Method = single", xlab ="", ylab = NULL, sub="")

hr <- hclust(dist_college, method = "complete", members=NULL)    
plot(hr, labels=FALSE, main="Method = complete", xlab ="", ylab = NULL, sub="")

hr <- hclust(dist_college, method = "average", members=NULL)     
plot(hr, labels=FALSE, main="Method = average", xlab ="", ylab = NULL, sub="")

hr <- hclust(dist_college, method = "centroid", members=NULL)    
plot(hr, labels=FALSE, main="Method = centroid", xlab ="", ylab = NULL, sub="")

hr <- hclust(dist_college, method = "ward.D", members=NULL)      
plot(hr, labels=FALSE, main="Method = ward.D", xlab ="", ylab = NULL, sub="")




#Q2-3
mycl <- cutree(hr, k=10)  # 10개의 군집 찾기 

plot(hr, labels=FALSE, main="Method = complete", xlab ="", ylab = NULL, sub="")
rect.hclust(hr, k=10, border="red")

college_hc <- data.frame(college_scaled, clusterID = as.factor(mycl))
hc_summary <- data.frame()

for (i in 1:(ncol(college_hc)-1)){
  hc_summary = rbind(hc_summary, 
                     tapply(college_hc[,i], college_hc$clusterID, mean))
}

colnames(hc_summary) <- paste("cluster", c(1:10))
rownames(hc_summary) <- c(colnames(college_scaled))
hc_summary

#Rader chart
par(mfrow = c(2,5))
for (i in 1:10){
  plot_title <- paste("Radar Chart for Cluster", i, sep=" ")
  radial.plot(hc_summary[,i], labels = rownames(hc_summary), 
              radial.lim=c(-2,2), rp.type = "p", main = plot_title, 
              line.col = "red", lwd = 3, show.grid.labels=1)
}
dev.off()

#Cluster2 vs Cluster7 t-test
hc_cluster2 <- college_hc[college_hc$clusterID == 2, c(1:17)]
hc_cluster7 <- college_hc[college_hc$clusterID == 7, c(1:17)]

hc_t_result <- data.frame()
for (i in 1:17){
  hc_t_result[i,1] <- t.test(hc_cluster2[,i], hc_cluster7[,i], 
                             alternative = "two.sided")$p.value
}
hc_t_result 


#Cluster7 vs Cluster8 t-test
hc_cluster7 <- college_hc[college_hc$clusterID == 7, c(1:17)]
hc_cluster8 <- college_hc[college_hc$clusterID == 8, c(1:17)]

hc_t_result <- data.frame()
for (i in 1:17){
  hc_t_result[i,1] <- t.test(hc_cluster7[,i], hc_cluster8[,i], 
                             alternative = "two.sided")$p.value
}
hc_t_result 


#Q2-4
library(gplots)
some_col_func <- colorspace::diverge_hcl
temp <- as.matrix(scale(college_hc[,c(1:17)],T,T))
gplots::heatmap.2(temp, 
                  srtCol = 60,
                  dendrogram = "row",
                  trace="none",          
                  margins =c(6,0.5),
                  denscol = "grey",
                  density.info = "density",
                  col = some_col_func,
                  labRow = F
)

#DBSCAN Clustering
install.packages("factoextra")
install.packages("dbscan")
library(factoextra)
library(dbscan)

ploan <- read.csv("Personal Loan.csv")
ploan_x <- ploan[,-c(1,5,10)]

ploan_x_scaled <- scale(ploan_x, center = TRUE, scale = TRUE)

#Q3-1
for(i in c(1,3,5,7,9)){
  for(j in c(2.2,2.4,2.6,2.8,3.0)){
    DBSCAN<- dbscan(ploan_x_scaled, eps = j, minPts = i)
    print(DBSCAN)
  }
}

#Q3-2
DBSCAN <- dbscan(ploan_x_scaled, eps = 2.6, minPts = 9)
DBSCAN
DBSCAN$cluster

cluster_DB<- data.frame(ploan_x_scaled, clusterID = as.factor(DBSCAN$cluster))


DBSCAN_summary <- data.frame()
for (i in 1:(ncol(cluster_DB)-1)){  
  DBSCAN_summary = rbind(DBSCAN_summary, 
                         tapply(cluster_DB[,i], cluster_DB$clusterID, mean))
}
colnames(DBSCAN_summary) <- paste("cluster", c(0:4))
rownames(DBSCAN_summary) <- colnames(ploan_x_scaled)
DBSCAN_summary

#Radar Chart 
par(mfrow = c(2,3))
for (i in 1:5){
  plot_title <- paste("Radar Chart for Cluster", i-1, sep=" ")
  radial.plot(DBSCAN_summary[,i], labels = rownames(DBSCAN_summary),radlab = T,
              radial.lim=c(-2,2), rp.type = "p", main = plot_title, show.centroid = T,boxed.radial=F,
              line.col = "red", lwd = 3, show.grid.labels=1, mar = c(6,3,6,3))
}
dev.off()


#Q3-3
pca_ploan <- prcomp(ploan_x_scaled,T,T)
summary(pca_ploan)

pca = as.matrix(ploan_x_scaled) %*% pca_ploan$rotation
pca.frame <- data.frame(pca,cluster_DB[,12])

fviz_cluster(DBSCAN, pca[,1:2], ellipse = FALSE, geom = "point",
             show.clust.cent = FALSE)
