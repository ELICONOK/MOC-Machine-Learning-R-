install.packages("party")
library(sampling)
library(party)
library(grid)
library(mvtnorm)
library(modeltools)
library(stats4)
library(strucchange)
library(zoo)
str(MOR3)
traindata<- train_set#建立训练集
testdata<- test_set#建立测试集
traindata$labels <- as.factor(traindata$labels)
tree<- ctree(labels ~ HfCl4+ H2BPDC+ DMF+ HCO2H+ H2O+ Temperature,data=traindata)#建立决策树
plot(tree)#决策树直观图
test<- predict(tree,newdata=testdata)#测试集测试
table(test,testdata$labels)#结果图表
#计算成功率是28/42=66.67%