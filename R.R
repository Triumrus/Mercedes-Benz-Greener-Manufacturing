library(data.table)


###### СОЗДАНИЕ ВЫБОРКИ
######
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
# str(data)



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
# fwrite(data,"data.csv")

data$chetnoe <- data$ID%%2




data$ID <- NULL
a<- apply(data[1:4209,],2,function(x){
  sum(x,na.rm = T) == (4209 | 0)
  
})

data <- as.data.frame(data)
data<- data[,-which(a)]
data <- data.table(data)
y<- data$y
data$y <- NULL
is_multicol <- function(data){
  
    d<- data
    d <- abs(cor(d))     
  d[lower.tri(d)] <- 0    
  diag(d) <- 0    
  index <- which((1-d) < 1e-10, arr.ind = T)    
  print(length(index))
  if (length(index) == 0){      
    return(data)
    print('There is no collinearity in the data')
    
  } else {      
   
    data<- subset(data,select = -index[1,2])
    is_multicol(data)
    
  }      

  # data$y
  
  }



data<- is_multicol(data)
data$y <- y

# PCA

get_pca2 <- function(test_data){    
  fit <- prcomp(test_data)    
  cum_prop <- summary(fit)$importance['Cumulative Proportion',]    
  test_data <- cbind(test_data, fit$x[,1:min(which(cum_prop>0.9))])    
  return(test_data)    
}

data<- cbind(data$y,get_pca2(subset(data,select = -y)))

names(data)[1] <- "y"

# K  - means 
# 14 Кластеров
# Запсук расчет для определения

# for(i in 1:100){
#   irisCluster <- kmeans(data[,-1], i, nstart = 100)
#   kluster[i] <- sum(irisCluster$withinss)
# }
# 
# plot(kluster)
# plot(kluster/c(kluster[2:100],kluster[100]))
# save(irisCluster,file="irisCluster.rda")



# Построе кластера

k_cluster <- kmeans(data[,-1], 14, nstart = 100)
data$cluster <-  k_cluster$cluster
plot(y=data$y,x=data$cluster)
data <- data.table(data)
# nrow(data[y<=(IQR(data$y,na.rm = T)*1.5+quantile(x = data$y,probs = 0.75,na.rm = T))])

# Добавление фич PCA в полиноме
transform_x = function(data)
{
  do_transform = function(x, lambd) {
    if (lambd > 0) x ^ lambd else if (lambd < 0) -(x ^ lambd) else log(x) }
  
  x_data = data[,2]
  y_data = data[,1]
  lambdas = seq(-5, 5, 0.1)
  corrs = sapply(lambdas, 
                 function(lambd) cor(do_transform(x_data, lambd), y_data))
  lambda = lambdas[which.max(abs(corrs))]
  return(lambda)
}


nc<- ncol(data)
for(i in min(grep("PC1",names(data))):(nc-1)) {print(i)
  data <- cbind(data,
                subset(data,select = i)^transform_x(subset(data[1:4209,],select = c(1,i)))
  )
  names(data)[ncol(data)] <- paste0(names(data)[i],"_",transform_x(subset(data[1:4209,],select = c(1,i))))
}

# BOOSTRAP

a<- sample(4209,6000,replace = T)

boot<- train_m[a,]
lm_boot<- lm(y~.,boot)
summary(lm_boot)
train_m_boot <- train_m[-unique(a),]

train_m_boot$y_pred<- predict(lm_boot,train_m_boot[,-1])
boot$y_pred<- predict(lm_boot,boot[,-1])




sum((train_m_boot$y_pred-mean(train_m_boot$y))^2)/sum((train_m_boot$y-mean(train_m_boot$y))^2)
R<- sum((boot$y_pred-mean(boot$y))^2)/sum((boot$y-mean(boot$y))^2)
R







######  Построение модели
######







train_m <- data[1:4209,]
test_m <- data[4210:8418,]


hetero_test <-  function(test_data){
  y<- test_data[,1]
  y <- unlist(y)
  test_data[,1] <- NULL
  fit<-  lm(y~.,test_data)
  R2<- lm(fit$residuals^2~.,test_data)
  return(summary(R2)$r.squared)
  
  
}

hetero_test(train_m)

train_m <- as.data.frame(train_m)

smart_model <- function(data)
{
  a<-   sapply(names(data[-1]), function(name) {
    m = lm(as.formula(paste(name, "~ .")), data[-1])
    r2 = summary(m)$r.squared
    1 / (1 - r2)
    
  })
  print(length(a))
  if (a[which.max(a)]>10) (data <- smart_model(data[,-(which.max(a)+1)])) else ( lm(as.formula(paste(names(data[1]), "~ .")), data) )
}

model<- smart_model(train_m)
save(model, file = "model.rda")
load("model.rda")

# предсказаине

test$y<- predict(lm_boot,test_m[,-1])

test[,c(1,378)]
fwrite(test[,c(1,378)],"kaggle_8.csv")
