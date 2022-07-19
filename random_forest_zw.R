#混淆矩阵：混淆矩阵的每一列代表了预测类别，每一列的总数表示预测为该类别的数据的数目；
#每一行代表了数据的真实归属类别，每一行的数据总数表示该类别的数据实例的数目。
#每一列中的数值表示真实数据被预测为该类的数目：第一行第一列中的43表示有43个实际归属第一类的实例被预测为第一类，
#同理，第一行第二列的2表示有2个实际归属为第一类的实例被错误预测为第二类。
setwd("C:/Users/zw_sun/Desktop/2022_3rd_term/ML/final_report")
#张晶同学的代码
# MOR
library(tidyverse)
rm(list=ls())
MOR <- read.csv("mor.csv")
# colnames(MOR)
# [1] "HfCl4.mg."             "H2BPDC.mg."           
# [3] "DMF.mL."               "HCO2H.mL."            
# [5] "Formic.acid.purity..." "H2O.mL."              
# [7] "Reaction.time.h."      "Temperature.C."       
# [9] "Phase"   
# 初步探索特征
# MOR %>%
#   group_by(Phase) %>%
#   summarise(cnt=n())


# 整理特征
MOR2 <- MOR
MOR2$t1 <- 0
MOR2$t2 <- 0
MOR2$t1[MOR2$Temperature=="100"] <- 1
MOR2$t2[MOR2$Temperature=="120"] <- 1
MOR2$labels <- as.numeric(as.factor(MOR2$Phase))
MOR3 <- subset(MOR2, select = -c(X,Entry,Phase, t1,t2, Reaction_time, 
                                 Formic_acid_purity) )
MOR4 <- subset(MOR2, select = -c(X,Entry,Phase, t1,t2, Reaction_time, 
                                 Formic_acid_purity) )
#如果不想/想删掉某列，可在上面行调整
for (i in 1:6) {
  max <- max(MOR3[,i])
  min <- min(MOR3[,i])
  MOR3[,i] <- (MOR3[,i]-min)/(max-min)
}

# 设置随机种子
set.seed(20220717)
# 把类别从ABCDEF转成123456
train_ind <- c()
for (i in 1:6) {
  # 提取当前类别的行号
  ind <- which(MOR3$labels == i)
  # 随机抽70%作为训练集
  train_ind <- c(train_ind, sample(ind, round(0.85*length(ind))))
}
ntrain_set <- MOR3[sort(train_ind), ]
ntest_set <- MOR3[sort(-train_ind), ]

#-------------------------------------------------------
# 设置随机种子
set.seed(20220717)
# 把类别从ABCDEF转成123456
train_ind <- c()
for (i in 1:6) {
  # 提取当前类别的行号
  ind <- which(MOR4$labels == i)
  # 随机抽70%作为训练集
  train_ind <- c(train_ind, sample(ind, round(0.85*length(ind))))
}
train_set <- MOR4[sort(train_ind), ]
test_set <- MOR4[sort(-train_ind), ]

#我的代码
#randomForest_zw-s
library(randomForest)
set.seed(20220717)
rf.train<-randomForest(ntree=1000,as.factor(labels)~.,data=train_set,importance=TRUE,na.action = na.pass)
#ntree：决策树个数；as.factor(因变量)；date=训练集；importance:是否显示因子重要性；na.action=处理缺失的数据，这里选的是跳过
#更多内容参考help("randomForest")
rf.test<-predict(rf.train,newdata = test_set,type = "class")
rf.cf<-caret::confusionMatrix(as.factor(rf.test),as.factor(test_set$labels))
rf.cf







