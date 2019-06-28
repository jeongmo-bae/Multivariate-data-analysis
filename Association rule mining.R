#ASSIGNMENT1
#산업경영공학부 2014170849 배정모

install.packages("arules")
install.packages("arulesViz")
install.packages("wordcloud")

library(arules)       
library(arulesViz)
library(wordcloud)

######<step1>######
mooc_dataset <- read.csv("big_student_clear_third_version.csv")
View(mooc_dataset)

#Q1-1
Institute<-mooc_dataset$institute
Course<-mooc_dataset$course_id
Region<-mooc_dataset$final_cc_cname_DI
Degree<-mooc_dataset$LoE_DI

#Q1-2
Region<-gsub(" ","",Region)
Region

#Q1-3
RawTransactions<-paste(Institute,Course,Region,Degree,sep='_')
RawTransactions

#Q1-4
MOOC_transactions<-paste(mooc_dataset$userid_DI,RawTransactions,sep=' ')
MOOC_transactions

#Q1-5
write.table(MOOC_transactions,file='MOOC_User_Course.csv',col.names = FALSE,row.names = FALSE,quote = FALSE)



######<step2>######
#Q2-1
MOOC_single <- read.transactions("MOOC_User_Course.csv", 
                                format = "single", cols = c(1,2), rm.duplicates=TRUE)

summary(MOOC_single)
itemInfo(MOOC_single)


#Q2-2
itemName <- itemLabels(MOOC_single)
itemCount <- itemFrequency(MOOC_single)*nrow(MOOC_single)
col <- brewer.pal(9, "Set1")
wordcloud(words = itemName, freq = itemCount, min.freq = 800, scale = c(2.5, 0.1), col = col , random.order = FALSE)

#Q2-3
itemFrequencyPlot(MOOC_single, support = 0.01, cex.names=0.9)



######<step3>######
#Q3-1
rules1 <- apriori(MOOC_single, parameter=list(support=0.0025, confidence=0.05))   
inspect(rules1)
rules2 <- apriori(MOOC_single, parameter=list(support=0.0020, confidence=0.05))   
inspect(rules2)
rules3 <- apriori(MOOC_single, parameter=list(support=0.0015, confidence=0.05))   
inspect(rules3)
rules4 <- apriori(MOOC_single, parameter=list(support=0.0025, confidence=0.04))   
inspect(rules4)
rules5 <- apriori(MOOC_single, parameter=list(support=0.0020, confidence=0.04))   
inspect(rules5)
rules6 <- apriori(MOOC_single, parameter=list(support=0.0015, confidence=0.04))   
inspect(rules6)
rules7 <- apriori(MOOC_single, parameter=list(support=0.0025, confidence=0.03))   
inspect(rules7)
rules8 <- apriori(MOOC_single, parameter=list(support=0.0020, confidence=0.03))   
inspect(rules8)
rules9 <- apriori(MOOC_single, parameter=list(support=0.0015, confidence=0.03))   
inspect(rules9)

#Q3-2-1
rules <- apriori(MOOC_single, parameter=list(support=0.001, confidence=0.05))
inspect(rules)

inspect(sort(rules, by="support"))

#Q3-2-2
inspect(sort(rules, by="confidence"))

#Q3-2-3
inspect(sort(rules, by="lift"))

#Q3-2-4
df_rules<-as(rules,'data.frame')
scl<-NA
df_rules<-cbind(df_rules,scl)
df_rules$scl<-df_rules$support*df_rules$confidence*df_rules$lift
df_rules[order(df_rules$scl,decreasing = T),]    #내림차순 정렬

#Q3-2-5
plot(rules, method="graph",engine='interactive')

