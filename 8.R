library(ISLR)
library(MASS)
library(tree)
library(randomForest)
library(ggplot2)
library(gbm)
library(glmnet)
# (7)----
set.seed(1)

mtry.val <- seq(3, 13, 1)
ntree.val <- seq(100, 1500, 100)
err.df <- data.frame(mtry = rep(0, 165), ntree = rep(0, 165), err = rep(NA, 165))

mtry.val <- seq(5, 5, 1)
ntree.val <- c(seq(1, 10, 1), seq(10,100,10)) 
err.df <- data.frame(mtry = rep(0, 20), ntree = rep(0, 20), err = rep(NA, 20))

train <- sample(1:nrow(Boston), nrow(Boston) / 2)
test <- -train
boston.test <- Boston[test, "medv"]

i = 1

for (p in mtry.val) {
  for (q in ntree.val) {
    rf.boston = randomForest(medv ~ ., data=Boston, subset=train, mtry=p, ntrees=q, importance=TRUE)
    yhat.rf.boston <- predict(rf.boston, newdata = Boston[test,])
    err.df$mtry[i] <- p
    err.df$ntree[i] <- q
    err.df$err[i] <- mean((yhat.rf.boston - boston.test) ^ 2)
    i = i + 1
  }
}

ggplot(data = err.df, aes(x = ntree, y = err, color = as.factor(mtry))) + geom_line(aes(group = mtry)) +
  labs(x = 'Number of trees', y = 'Test error', color = 'Number of split vars') +
  theme(legend.position='top',legend.direction='horizontal')


# (8)----
## (a)
train <- sample(1:nrow(Carseats), nrow(Carseats) / 2)
test <- -train

## (b)
set.seed(1)

tree.carseats = tree(Sales ~ ., data=Carseats, subset=train)
plot(tree.carseats)
text(tree.carseats,pretty=0)

carseats.test <- Carseats[test, "Sales"]
yhat.tree <- predict(tree.carseats, newdata = Carseats[test,])
mean((yhat.tree-carseats.test)^2)

## (c)
cv.carseats = cv.tree(tree.carseats)
plot(cv.carseats$size, cv.carseats$dev, type='b')

prune.carseats = prune.tree(tree.carseats,best=6)
plot(prune.carseats)
text(prune.carseats, pretty=0)

yhat.prune <- predict(prune.carseats, newdata = Carseats[test,])
mean((yhat.prune-carseats.test)^2)

## (d)
set.seed(1)
bag.carseats = randomForest(Sales ~ ., data=Carseats, subset=train,importance=TRUE)
importance(bag.carseats)
### Priceが最も重要な変数といえる。

## (e)
set.seed(1)

mtry.val <- seq(3, 10, 1)
ntree.val <- seq(100, 1500, 100)
err.df <- data.frame(mtry = rep(0, 120, ntree = rep(0, 120), err = rep(NA, 120)))

train <- sample(1:nrow(Carseats), nrow(Carseats) / 2)
test <- -train
carseats.test <- Carseats[test, "Sales"]

i = 1

for (p in mtry.val) {
  for (q in ntree.val) {
    rf.carseats = randomForest(Sales ~ ., data=Carseats, subset=train, mtry=p, ntrees=q, importance=TRUE)
    yhat.rf.carseats <- predict(rf.carseats, newdata = Carseats[test,])
    err.df$mtry[i] <- p
    err.df$ntree[i] <- q
    err.df$err[i] <- mean((yhat.rf.carseats - carseats.test) ^ 2)
    i = i + 1
  }
}

ggplot(data = err.df, aes(x = ntree, y = err, color = as.factor(mtry))) + geom_line(aes(group = mtry)) +
  labs(x = 'Number of trees', y = 'Test error', color = 'Number of split vars') +
  theme(legend.position='top',legend.direction='horizontal')

rf.carseats = randomForest(Sales ~ ., data=Carseats, subset=train, mtry=10, ntrees=800, importance=TRUE)

importance(rf.carseats)
### Priceが最も重要な変数であるといえる。

# (9)----
## (a)
train <- sample(1:nrow(OJ), 800)
test <- -train
oj.test <- OJ[test, "Purchase"]

## (b)
tree.oj = tree(Purchase ~ .,data = OJ,subset = train)
summary(tree.oj)
### 誤分類率は0.1675で、終端ノードの数は7つになる。

## (c)
tree.oj
### 0.051325 <= loyalCH < 0.48285かつ、SaleProceMM < 2.04のときMMとなる。


## (d)
plot(tree.oj)
text(tree.oj,pretty=0)


## (e)
pred.oj <- predict(tree.oj, newdata = OJ[test,],type="class")
table(pred.oj, oj.test)
(139+76)/270
### テスト正解率は0.7851852

## (f),(g)
cv.oj <- cv.tree(tree.oj,FUN=prune.misclass)
plot(cv.oj$size, cv.oj$dev, type='b')
### 最適な木のサイズは5といえる。


## (h)


## (i)
prune.oj <- prune.misclass(tree.oj, best=5)
prune.oj <- prune.tree(tree.oj, best=5)
plot(prune.oj)
text(prune.oj, pretty=0)

## (j)
summary(prune.oj)
### 0.18で、刈込を行なった木が大きくなった。


## (k)
pred.prune.oj <- predict(prune.oj, newdata = OJ[test,],type="class")
table(pred.prune.oj, oj.test)
(44+12)/270
### 枝刈りをした方が誤分類率が小さくなった。

# (10)----
## (a)
Hitters <- Hitters[is.na(Hitters$Salary) == FALSE,]
Hitters$Salary <- log(Hitters$Salary)


## (b)
train <- sample(1:nrow(Hitters), 200)
test <- -train
hitters.test <- Hitters[test, "Salary"]


## (c), (d)
set.seed(1)
lambda = 10 ^ seq(-4, -1, 0.5)


i = 1
err.df <- data.frame(lambda = lambda, train.err = rep(NA, length(lambda)), test.err = rep(NA, length(lambda)))
for (l in lambda) {
  boost.hitters <- gbm(Salary ~ . , data = Hitters[train,], distribution = "gaussian", n.trees = 1000, interaction.depth = 4, shrinkage = l)
  yhat.boost <- predict(boost.hitters, newdata = Hitters[train,], n.trees = 1000)
  err.df$lambda[i] = l
  err.df$train.err[i] = mean((yhat.boost - Hitters[train, 'Salary']) ^ 2)
  i = i + 1
}

i = 1
for (l in lambda) {
  boost.hitters <- gbm(Salary ~ . , data = Hitters[train,], distribution = "gaussian", n.trees = 1000, interaction.depth = 4, shrinkage = l)
  yhat.boost <- predict(boost.hitters, newdata = Hitters[ - train,], n.trees = 1000)
  err.df$test.err[i] = mean((yhat.boost - Hitters[ - train, 'Salary']) ^ 2)
  i = i + 1
}

ggplot(data = err.df, aes(x = lambda)) +
  geom_line(aes(y = train.err, color = 'Training MSE')) +
  geom_line(aes(y = test.err, color = 'Test MSE')) +
  labs(x = 'Shrinkage param(lambda)', y = 'MSE') +
  scale_color_manual("Error metrics", values = c('Training MSE' = 'red', 'Test MSE' = 'blue')) +
  theme(legend.position = 'top', legend.direction = 'horizontal') +
  ggtitle("Mean squared error for different values of lambda")

min(err.df$test.err)
lambda[which.min(err.df$test.err)]
### lambdaが0.003162278のときテストMSEは0.1948569となる。

## (e)

### lasso
x = model.matrix(Salary~.,Hitters)[,-1]
y = Hitters$Salary
y.test = y[test]
grid=10^seq(10,-2,length=100)

lasso.mod = glmnet(x[train,],y[train], alpha=1,lambda=grid)
plot(lasso.mod)

set.seed(1)
cv.out = cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
### テストMSEは0.3548958都なり、ブースティングの方がテストMSEは小さくなる。


## (f)
boost.hitters <- gbm(Salary ~ . , data = Hitters[train,], distribution = "gaussian", n.trees = 1000, interaction.depth = 4, shrinkage = 0.003162278)
summary(boost.hitters)
### CAtBatが最も重要な変数となる。

## (g)
set.seed(1)

ntree.val <- seq(100, 1500, 100)
err.df <- data.frame(ntree = rep(0, 15), err = rep(NA, 15))

i = 1


for (q in ntree.val) {
  bag.hitters = randomForest(Salary ~ ., data=Hitters, subset=train, mtry=19, ntrees=q, importance=TRUE)
  yhat.bag.hitters <- predict(bag.hitters, newdata = Hitters[test,])
  err.df$ntree[i] <- q
  err.df$err[i] <- mean((yhat.bag.hitters - hitters.test) ^ 2)
    i = i + 1
}


ggplot(data = err.df, aes(x = ntree, y = err ) )+
  geom_line(aes(y = err, color = 'Training MSE')) +
  labs(x = 'Number of trees', y = 'Test error', color = 'Number of split vars') +
  theme(legend.position='top',legend.direction='horizontal')

min(err.df$err)
### ntreeが1300の時、テストMSEが0.1083145となり、ブースティングより若干性能がよくなる。


# (11)----
## (a)
train <- sample(1:nrow(Caravan), 1000)
test <- -train
Caravan$Purchase <- ifelse(Caravan$Purchase == 'Yes',1,0)
caravan.test <- Caravan[test, "Purchase"]

## (b)
set.seed(1)
boost.mod <- gbm(Purchase ~ ., data = Caravan[train,], distribution = "bernoulli", n.trees = 1000, shrinkage = 0.01)
summary(boost.mod)
### PPERSAUTが最も重要な変数となる。


## (c)
boost.prob <- predict(boost.mod, Caravan[test,], n.trees = 1000, type = 'response')
boost.pred <- ifelse(boost.prob > 0.2, 1, 0)
table(caravan.test, boost.pred)
57 / (184+57)
### 0.2365145

### ロジスティック回帰
lm.mod <- glm(Purchase ~ ., data = Caravan[train,], family = 'binomial')
lm.prob <- predict(lm.mod, Caravan[test,], type='response')
lm.pred <- ifelse(lm.prob > 0.2, 1, 0)
table(caravan.test, lm.pred)
59 / (461+59)
### 0.1134615であり、ブースティングの方が精度がいい