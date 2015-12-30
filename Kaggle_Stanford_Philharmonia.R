ready.r  <- function(){setwd('C:\\Users\\...')
                     library(reshape2)
                     subscriptions=read.csv('subscriptions_lo.csv', colClasses='character')
                     subscriptionsLong=melt(subscriptions,id=c('account.id', 'season'))
                     subscriptionsWide = dcast(subscriptionsLong, account.id~season+variable,
                                               value.var='value')
                     account=read.csv('account_lo.csv')
                     wide=merge(account, subscriptionsWide, by='account.id', all=T)
                     tickets=read.csv('tickets_lo.csv', colClasses='character')
                     tickets_melt=melt(tickets, id=c('account.id', 'season', 'set'))
                     tickets_melt=subset(tickets_melt, variable=='price.level')
                     tickets_cast=dcast(tickets_melt, account.id~season+set+variable, value.var='value')
                     wider = merge(wide, tickets_cast, by='account.id', all=T)
                     names(wider)
                     train_kaggle=read.csv('train.csv', colClasses='character')
                     test_kaggle=read.csv('test_kaggle.csv', colClasses='character')
}
#
ready.r()
library(tgp)
fried.bool(500)
attack(set)
svm.stuff <- function(x){
  w = x
  library(e1071)
  k=10
  set.seed(1)
  folds = sample(1:k, nrow(x), replace=TRUE)
  cv.errors.svm = matrix(NA,k)
  a = w[,2]
  b = w[,3]
  c = w[,4]
  d = (lm(a~b+c))$fit

  return(d)
}


head(w)
length(w)
prep  <- function(set){
  
  #ready()
  mse = matrix(list())
  w = wider[,c("account.id", set)]
  w[is.na(w)]=0
  #for (i in 2:length(set)) {w[,i]=as.numeric(w[,i])}
  w[,2]=as.numeric(w[,2])
  w[,3]=as.factor(w[,3])
  #w[,3] = ifelse(w[,3]<5,0,1)
  #
  #linear
  #
  #linear.stuff(w)
  #polynomial
  #
  #trees - boost
  #
  #boost.stuff(w)
  #trees - bag
  #
  #bag.stuff(w)
  #
  #svm
  #mse[[1]] = svm.stuff(w)  
  return(w) 
}

merge.train  <- function(x){
  w = x
  w = merge(train_kaggle, w, by="account.id", all=FALSE)
  w[,1] = as.factor(w[,1])
  w[,2] = as.numeric(w[,2])  
  return(w)
}

attack  <- function(set){
  w = merge.train(prep(set))
}

set = c("2013-2014_total", "2013-2014_price.level")
prep(set)

attack(set)
length(w2)
head(w)

attack  <- function(set){
  w = merge.train(prep(set))
}

kaggle12 = attack(set)
str(kaggle12)
colnames(kaggle12) = c("account.id", "total", "total2013", "price2013")

k=10
set.seed(1)
folds = sample(1:k, nrow(kaggle12), replace=TRUE)
cv.errors.svm = matrix(NA,k)
max.points = matrix(NA,k) 

for (j in 1:k){
  
  svm.kaggle = svm(log(1+total)~total2013+price2013, 
                   data=kaggle12[folds!=j,], kernel="linear",
                   cost=.001, verbose=F)
  
  
  yhat.svm = predict(svm.kaggle, newdata=kaggle12[folds==j,])
  
  
  cv.errors.svm[j] = mean((yhat.svm - log(1+kaggle12$total[folds==j]))^2)
  max.points[j] = max(yhat.svm)
}
cv.errors.svm
mean(cv.errors.svm)
max.points
mean(max.points)
exp(max.points)-1


svm.kaggle = svm(total~total2013+price2013, 
                 data=kaggle12[training,], kernel="linear",
                 cost=.001, verbose=F)
yhat.svm = predict(svm.kaggle, newdata=kaggle12[-training,])
mean((yhat.svm - kaggle12$total[-training])^2)
exp(max(yhat.svm))-1

svm.kaggle = svm(log(1+total)~total2013+price2013, 
                 data=kaggle12[training,], kernel="linear",
                 cost=.001, verbose=F)
yhat.svm = predict(svm.kaggle, newdata=kaggle12[-training,])
mean((yhat.svm - log(1+kaggle12$total[-training]))^2)
max(yhat.svm)

svm.kaggle = svm(total~total2013+price2013, 
                 data=kaggle12, kernel="linear",
                 cost=10, verbose=F)


stopifnot(svm.kaggle$fit == yhat.svm)

library(gbm)
training = sample(1:nrow(kaggle12), nrow(kaggle12)*0.80)
boost.kaggle = gbm(total~.-account.id, data=kaggle12[training,], distribution="gaussian", 
                   n.trees = 5000, interaction.depth=4, shrinkage=0.005, verbose=F)
plot(boost.kaggle)
yhat.boost = predict(boost.kaggle, newdata=kaggle12[-training,],  n.trees=5000)
error.boost = mean((yhat.boost-kaggle12[-training,]$total)^2)
error.boost
max(exp(yhat.boost)-1)
max(yhat.boost)

premerge = prep(set)
str(premerge)
kaggle12_up = merge(test_kaggle, premerge, by="account.id", all=FALSE)
str(kaggle12_up)
colnames(kaggle12_up) = c("account.id", "total2013", "price2013")
kaggle12_up$total = predict(svm.kaggle, newdata=kaggle12_up)

max(kaggle12_up$total)
sum(kaggle12_up$total)
head(kaggle12_up$total)

train.merge.m2 = read.csv("nov24set_train_merge_m2.csv")
str(train.merge.m2)
head(train.merge.m2)
training = read.csv("supertraining.csv")
training = as.matrix(training)
training = as.numeric(training)

test.mse.shrink3 = matrix(NA, 100, nc=3)
test.mse.shrink3
library(gbm)

training = as.matrix(training)
training = as.numeric(training)

for (j in 1:3){
  for (i in 1:100) {
    
    boost.nov25 = gbm(log(1+total)~., data=train.merge.m2[training,],
                      distribution="gaussian", 
                      n.trees = 5000, interaction.depth=j, 
                      shrinkage=0.0001*i, verbose=F)
    
    yhat.boost = predict(boost.nov25, newdata=train.merge.m2[-training,], 
                         interaction.depth=j, shrinkage=0.0001*i, n.trees=5000)
    
    test.mse.shrink3[i,j]=  mean( (yhat.boost - log(train.merge.m2[-training,]$total + 1))^2)
  }
}
