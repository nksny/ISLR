library(e1071)
library(ggplot2)
library(MASS)
library(ISLR)



# (4)----
set.seed(1)

X=data.frame(X1=abs(rnorm(100,1.5,1.2)),X2=abs(rnorm(100,1.5,1)+0.2))
y=ifelse(2/3*(X[,1]-3)^2+X[,1]-2-X[2]>0,'red','blue') 

dat <- data.frame(X,y=as.factor(y))
plot(X,xlab='X1',ylab='X2',col=y)

train <- sample(100,80)
test <- -train

## サポートベクター分類器
set.seed(1)
tune.out <- tune(svm, y~., data=dat[train,], kernel='linear', 
                 ranges=list(cost=c(0.001, 0.01,0.1,1,5,10,100)))
summary(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)
plot(bestmod,dat[train,])
ypred <- predict(bestmod, dat[train,1:2])
table(predict=ypred, truth=dat[train,]$y)
### 訓練誤分類率は9/80=0.1125
ypred <- predict(bestmod, dat[test,1:2])
table(predict=ypred, truth=dat[test,]$y)
### テスト誤分類率は3/20=0.15


## SVM(動径基底関数カーネル)
set.seed(1)
tune.out <- tune(svm, y~.,data=dat[train,],kernel='radial',
                 ranges=list(cost=c(0.1,1,10,100,1000),
                             gamma=c(0.5,1,2,3,4)))
summary(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)
plot(bestmod,dat[train,])
ypred <- predict(bestmod, dat[train,1:2])
table(predict=ypred, truth=dat[train,]$y)
### 訓練誤分類率は1/80=0.0125
ypred <- predict(bestmod, dat[test,1:2])
table(predict=ypred, truth=dat[test,]$y)
### テスト誤分類率は0/20=0


## SVM(多項式カーネル)
set.seed(1)
tune.out <- tune(svm, y~.,data=dat[train,],kernel='polynomial',
                 ranges=list(cost=c(0.1,1,10,100,1000),
                             degree=c(0.5,1,2,3,4)))
summary(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)
plot(bestmod,dat[train,])
ypred <- predict(bestmod, dat[train,1:2])
table(predict=ypred, truth=dat[train,]$y)
### 訓練誤分類率は8/80=0.1
ypred <- predict(bestmod, dat[test,1:2])
table(predict=ypred, truth=dat[test,]$y)
### テスト誤分類率は2/20=0.1

## 以上より、テスト誤分類率においてSVMの方が優れている。



# (5)----
## (a)
set.seed(1)
x1 <- runif(500)-0.5
x2 <- runif(500)-0.5
y <- 1*(x1^2-x2^2 > 0.05)

## (b)
x <- matrix(c(x1,x2),ncol=2)
dat <- data.frame(x, y=as.factor(y))
plot(x,col=ifelse(y,'red','blue'))

## (c)
glm.fits <- glm(y~.,data = dat,family = binomial)
summary(glm.fits)

## (d)
glm.probs <- predict(glm.fits, dat[,1:2])
plot(x,col=ifelse(glm.probs>0,'red','blue'), pch=ifelse(as.integer(glm.probs>0)==y,1,4))

## (e)
glm.fits <- glm(y ~ poly(x1,2)+poly(x2,2),data = dat,family = binomial)

## (f)
glm.probs <- predict(glm.fits.2, newdata=list(dat[test,1:2]))
plot(x1,x2,col=ifelse(glm.probs>0,'red','blue'),pch=ifelse(as.integer(glm.probs>0)==y,1,4))

## (g)
set.seed(1)
tune.out <- tune(svm, y~., data=dat, kernel='linear', 
                 ranges=list(cost=c(0.001, 0.01,0.1,1,5,10,100)))
summary(tune.out)

svm.fit <- svm(y~., data=dat, kernel='linear', cost=0.001)
svm.pred <- predict(svm.fit, data.frame(x), type="response")
plot(x1,x2,col=ifelse(svm.pred!=1,'red','blue'),pch=ifelse(svm.pred == y,4,1))
plot(svm.fit, dat)

## (h)
set.seed(1)
tune.out <- tune(svm, y~.,data=dat,kernel='polynomial',
                 ranges=list(cost=c(0.1,1,10,100),
                             degree=c(0.5,1,2)))
summary(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)

svm.fit <- svm(y~., data=dat, kernel="polynomial",cost=10,degree=2)
svm.pred <- predict(svm.fit, data.frame(x), type="response")
plot(x1,x2,col=ifelse(svm.pred!=1,'red','blue'),pch=ifelse(svm.pred == y,4,1))
plot(svm.fit,dat)

## (i)
table(predict=ifelse(glm.probs>0,2,1), truth=dat$y)
### 誤分類率は0/500=0
table(predict=svm.pred, truth=dat$y)
### 誤分類率は2/500=0.004
### 今回のデータでは、非線形カーネルのSVMよりも非線形のロジスティック回帰の方が良いモデルであるといえる。

# (6)----
## (a)
set.seed(1)
x1 <- runif(500)-0.5
x2 <- runif(500)-0.5
y <- 1*(x1-x2 > 0.05)+1

train <- sample(500,400)
test <- -train

x <- matrix(c(x1,x2),ncol=2)
dat <- data.frame(x, y=as.factor(y))
plot(x,col=y)

## (b)
set.seed(1)
tune.out <- tune(svm, y~., data=dat[train,], kernel='linear', 
                 ranges=list(cost=c(0.001, 0.01,0.1,1,5,10,100)))
summary(tune.out)
### costが大きいほど誤分類率が下がる。

## (c)
answer <- matrix(c(0.001, 0.01,0.1,1,5,10,100,0,0,0,0,0,0,0),ncol=2)
set.seed(1)
j <- 1
for (i in c(0.001, 0.01,0.1,1,5,10,100)) {
  svm.fit <- svm(y~., data=dat, kernel='linear', scale=FALSE, cost=i)
  ypred <- predict(svm.fit, dat[train,1:2])
  a <- ypred==dat[train,]$y
  answer[j,2] <- 400 - sum(a)
  j <- j+1
}
answer
### cost=5のときに最も精度がいい

## (d)
### 訓練誤分類率はcostが大きいほどよくなるが、テスト誤分類率はある水準を超えると悪くなる。過学習しているといえる。


# (7)----
## (a)
set.seed(1)
auto <- Auto
auto$mpg_hi <- 1*(auto$mpg>median(auto$mpg))

## (b)
set.seed(1)
tune.out <- tune(svm, mpg_hi~.-mpg-name, data=auto, kernel='linear', 
                 ranges=list(cost=c(0.001, 0.01,0.1,1,5,10,100)))
summary(tune.out)

svm.fit.b <- svm(mpg_hi~.-mpg-name, data=auto, kernel='linear', scale=FALSE, cost=0.01)
plot(svm.fit.c1,auto,weight~year)

## (c)
### 動径基底カーネル
set.seed(1)
tune.out <- tune(svm,mpg_hi~.-mpg-name, data=auto, kernel='radial', 
                 ranges=list(cost=c(0.1,1,10,100,1000),
                             gamma=c(0.5,1,2,3,4)))
summary(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)
svm.fit.c1 <- svm(mpg_hi~.-mpg-name, data=auto, kernel='radial', scale=FALSE, cost=1, gamma=1)

### 多項式基底カーネル
set.seed(1)
tune.out <- tune(svm,mpg_hi~.-mpg-name, data=auto, kernel='polynomial', 
                 ranges=list(cost=c(0.1,1,10,100,1000),
                             degree=c(0.1,1,10)))
summary(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)
svm.fit.c2 <- svm(mpg_hi~.-mpg-name, data=auto, kernel='polynomial', scale=FALSE, cost=0.1, degree=1)

## (d)
plot(svm.fit.c1,auto,displacement~weight)
### 出力が出ない



# (8)----
## (a)
set.seed(1)
train <- sample(nrow(OJ),800)
test <- -train

## (b)
svm.fit <- svm(Purchase~., data=OJ[train,], kernel='linear', scale=FALSE, cost=0.01)
summary(svm.fit)


## (c)
ypred <- predict(svm.fit, OJ[train,])
(800 - sum(ypred==OJ[train,]$Purchase))/800
### 訓練誤分類率は0.2125
ypred <- predict(svm.fit, OJ[test,])
(nrow(OJ) - 800 - sum(ypred==OJ[test,]$Purchase))/(nrow(OJ) - 800)
### テスト誤分類率は0.233333

## (d)
set.seed(1)
tune.out <- tune(svm, Purchase~., data=OJ[train,], kernel='linear', 
                 ranges=list(cost=c(0.001, 0.01,0.1,1,5,10)))
summary(tune.out)
### 最適なcostは0.1である。

## (e)
svm.fit <- svm(Purchase~., data=OJ[train,], kernel='linear', scale=FALSE, cost=0.1)
ypred <- predict(svm.fit, OJ[train,])
(800 - sum(ypred==OJ[train,]$Purchase))/800
### 訓練誤分類率は0.175
ypred <- predict(svm.fit, OJ[test,])
(nrow(OJ) - 800 - sum(ypred==OJ[test,]$Purchase))/(nrow(OJ) - 800)
### テスト誤分類率は0.188888...

## (f)
set.seed(1)
tune.out <- tune(svm, Purchase~., data=OJ[train,], kernel='radial', 
                 ranges=list(cost=c(0.001, 0.01,0.1,1,5,10)))
summary(tune.out)
svm.fit <- svm(Purchase~., data=OJ[train,], kernel='radial', scale=FALSE, cost=1)
ypred <- predict(svm.fit, OJ[train,])
(800 - sum(ypred==OJ[train,]$Purchase))/800
### 訓練誤分類率は0.33625
### 0.22625
ypred <- predict(svm.fit, OJ[test,])
(nrow(OJ) - 800 - sum(ypred==OJ[test,]$Purchase))/(nrow(OJ) - 800)
### テスト誤分類率は0.3444444
### 0.2518519
## (g)
set.seed(1)
tune.out <- tune(svm, Purchase~., data=OJ[train,], kernel='polynomial', degree=2,
                 ranges=list(cost=c(0.001, 0.01,0.1,1,5,10)))
summary(tune.out)
svm.fit <- svm(Purchase~., data=OJ[train,], kernel='radial', scale=FALSE, cost=10)
ypred <- predict(svm.fit, OJ[train,])
(800 - sum(ypred==OJ[train,]$Purchase))/800
### 訓練誤分類率は0.13625
ypred <- predict(svm.fit, OJ[test,])
(nrow(OJ) - 800 - sum(ypred==OJ[test,]$Purchase))/(nrow(OJ) - 800)
### テスト誤分類率は0.2

## (h)
### テスト誤分類率を基準に考えると、サポートベクター分類器が最も最良である。



