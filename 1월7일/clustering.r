library(sqldf)
library(dplyr)
rm(list=ls())
options("scipen"=100)
getwd()
setwd("C:/choidontouch/Auction_master_kr")
setwd("C:/choi/bigdata/data/auction")
rent <- read.csv("Auction_rent.csv", na.strings=c("", NA))
train <- read.csv("Auction_master_train.csv", na.strings=c("", NA))
test1 <- read.csv("Auction_master_test.csv" , na.strings=c("", NA))
regist <- read.csv("Auction_regist.csv", na.strings=c("", NA))
sub <- read.csv("Auction_submission.csv", na.strings=c("", NA))

str(train_rent_reg)
t <- na.omit(train_rent_reg)
t
dim(iris)
colSums(is.na(train_rent_reg))
str(train)
iris2 <- t
iris2<-iris[,1:4]
k <- c()
km.out.withness<-c()
km.out.between<-c()
for (i in 2:10){
  set.seed(1)
  km.out<-kmeans(iris2, centers=i)
  k[i-1] <- i
  km.out.withness[i-1]<-km.out$tot.withinss
  km.out.between[i-1]<-km.out$betweenss
}
df = data.frame(k, km.out.withness, km.out.between)
df

par(mfrow=c(2,2))
plot(df$k, df$km.out.withness, type='ol',
     xlab="군집의 개수 k",
     ylab="그룹내 제곱합")
plot(df$k, df$km.out.between, type='ol',
     xlab="군집의 개수 k",
     ylab="그룹간 제곱합")
##엘보우 포인트가 3번에 대하여 군집 형성
k3 <- kmeans(iris2, centers=3)
k3$centers <- 평균값
k3$cluster

table(k3$cluster)

