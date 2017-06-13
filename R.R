library(data.table)
setwd("C:/Users/user/Desktop/саша/kagle")
getwd()
test<- fread("test.csv")
train<- fread("train.csv")
test$y <- NA
data <- rbind(train,test)
data$X0 <- as.factor(data$X0)
data$X1 <- as.factor(data$X1)
data$X2 <- as.factor(data$X2)
data$X3 <- as.factor(data$X3)
data$X4 <- as.factor(data$X4)
data$X5 <- as.factor(data$X5)
data$X6<- as.factor(data$X6)
data$X8 <- as.factor(data$X8)
str(data)



# определяет какие столбцы факторы!
# factor<- sapply(data,function(x){is.factor(x)})
# names(which(factor))
# a<- as.numeric(which(factor))

nom<- ncol(data)
for(i in levels(data$X0)){
  data<- cbind(data,data$X0==i)
  names(data)[ncol(data)] <- paste0("X0",i)
}
(ncol(data)-nom)
nom<- ncol(data)
for(i in levels(data$X1)){
  data<- cbind(data,data$X1==i)
  names(data)[ncol(data)] <- paste0("X1",i)
}
(ncol(data)-nom)
nom<- ncol(data)
for(i in levels(data$X2)){
  data<- cbind(data,data$X2==i)
  names(data)[ncol(data)] <- paste0("X2",i)
}
(ncol(data)-nom)
nom<- ncol(data)
for(i in levels(data$X3)){
  data<- cbind(data,data$X3==i)
  names(data)[ncol(data)] <- paste0("X3",i)
}
(ncol(data)-nom)
nom<- ncol(data)
for(i in levels(data$X4)){
  data<- cbind(data,data$X4==i)
  names(data)[ncol(data)] <- paste0("X4",i)
}
(ncol(data)-nom)
nom<- ncol(data)
for(i in levels(data$X5)){
  data<- cbind(data,data$X5==i)
  names(data)[ncol(data)] <- paste0("X5",i)
}
(ncol(data)-nom)
nom<- ncol(data)
for(i in levels(data$X6)){
  data<- cbind(data,data$X6==i)
  names(data)[ncol(data)] <- paste0("X6",i)
}
(ncol(data)-nom)
nom<- ncol(data)
# X7 число
for(i in levels(data$X8)){
  data<- cbind(data,data$X8==i)
  names(data)[ncol(data)] <- paste0("X8",i)
}
(ncol(data)-nom)

data$X0 <- NULL
data$X1 <- NULL
data$X2 <- NULL
data$X3 <- NULL
data$X4 <- NULL
data$X5 <- NULL
data$X6 <- NULL
data$X8 <- NULL


data<- apply(data,2,function(x){ as.numeric(x)})
data <- data.table(data)

