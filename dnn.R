library(nnet) # neural network

MOR <- read.csv("data/mor1.csv")
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
ntrain_set$labels = as.factor(ntrain_set$labels)
ntest_set <- MOR3[sort(-train_ind), ]
ntrain_set$labels = as.factor(ntrain_set$labels)

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
train_set$labels = as.factor(train_set$labels)
test_set <- MOR4[sort(-train_ind), ]
test_set$labels = as.factor(test_set$labels)

# -------------------------------------------------------
# DNN
nX_train <- ntrain_set[ , 1:6] # use normalized data for DNN
y_train_onehot <- class.ind(train_set$labels)

DNN <- nnet(nX_train, y_train_onehot, maxit = 500, size = 9, softmax = TRUE)
DNN_pred_train <- predict(DNN, ntrain_set[ , 1:6], type="class")
DNN_pred_test <- predict(DNN, ntest_set[ , 1:6], type="class")
sprintf("Train set accuracy: %.2f%s", sum(DNN_pred_train == train_set$labels) / length(train_set$labels) * 100, '%')
sprintf("Test set accuracy: %.2f%s", sum(DNN_pred_test == test_set$labels) / length(test_set$labels) * 100, '%')