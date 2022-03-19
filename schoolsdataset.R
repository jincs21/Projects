#Viewing and cleaning dataset
stpor = read.csv("/Users/LT/Downloads/student-por.csv", sep = ";", header = TRUE)
stmat = read.csv("/Users/LT/Downloads/student-mat.csv", sep = ";", header = TRUE)
stpor[c(-3, -30, -31, -32, -33)] <- lapply(stpor[c(-3, -30, -31, -32, -33)], as.factor)
stmat[c(-3, -30, -31, -32, -33)] <- lapply(stmat[c(-3, -30, -31, -32, -33)], as.factor)
schools=merge(stpor,stmat,by=c("school","sex","age","address","famsize","Pstatus",
                               "Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
attach(stpor)
head(schools)
View(schools)
str(schools)
index <- 1:ncol(schools)
attach(schools)
#Separating into training and testing data
train = sample(seq(649), 450, replace = FALSE)
train2 = sample(seq(382), 300, replace = FALSE)
schools.x = schools[1:33]
schools.y = schools[,-c(14:33)]

#Linear regression
fit = lm(G3 ~ . - G2 - G1, data = stpor, subset = train)
summary(fit)
mean((G3-predict(fit, stpor))[-train]^2)



#Regularisation
install.packages("glmnet")
library(glmnet)
x = model.matrix(G3 ~ . -G2 -G1, data = schools)
y = schools$G3
fit.lasso = glmnet(x,y, alpha = 1)
plot(fit.lasso, xvar = "lambda", label = TRUE)
cv.lasso = cv.glmnet(x, y, alpha = 1)
cv.lasso
plot(cv.lasso)
coef(cv.lasso)
#Reg2
install.packages("glmnet")
library(glmnet)
x1 = model.matrix(G3.x ~ . -G2.x -G1.x, data = schools.x)
y1 = schools.x$G3.x
fit.lasso = glmnet(x1,y1, alpha = 1)
plot(fit.lasso, xvar = "lambda", label = TRUE)
cv.lasso = cv.glmnet(x1, y1, alpha = 1)
cv.lasso
plot(cv.lasso)
coef(cv.lasso)



#Finding lambda by splitting into training and testing data
lasso.tr = glmnet(x[train,], y[train])
pred = predict(lasso.tr, x[-train,])
pred
rmse = sqrt(apply((y[-train]-pred)^2, 2, mean))
rmse
plot(log(lasso.tr$lambda), rmse, type = "b", xlab = "Log(Lambda)")
lambda.best = lasso.tr$lambda[order(rmse)[1]]
lambda.best
coef(lasso.tr, s=lambda.best)

#We compare ridge, lasso and elastic-net regression
#1)Ridge
alpha0.fit = cv.glmnet(x[train,], y[train], type.measure = "mse",
                       alpha = 0, family = "gaussian")
plot(alpha0.fit)
alpha0.pred = predict(alpha0.fit, s = alpha0.fit$lambda.1se, newx = x[-train,])
mean((y[-train] - alpha0.pred)^2)
#2) Lasso
alpha1.fit = cv.glmnet(x[train,], y[train], type.measure = "mse",
                       alpha = 1, family = "gaussian")

alpha1.pred = predict(alpha1.fit, s = alpha1.fit$lambda.1se, newx = x[-train,])
mean((y[-train] - alpha1.pred)^2)
#3)Elastic-net r
alpha0.5.fit = cv.glmnet(x[train,], y[train], type.measure = "mse",
                         alpha = 0.5, family = "gaussian")
alpha0.5.pred = predict(alpha0.5.fit, s = alpha0.5.fit$lambda.1se, newx = x[-train,])
mean((y[-train] - alpha0.5.pred)^2)
# we try a few more values for alpha
list.of.fits = list()
for (i in 0:10) {
  fit.name = paste0("alpha", i/10)
  list.of.fits[[fit.name]]= cv.glmnet(x[train,], y[train], type.measure = "mse",
                                      alpha = i/10, family = "gaussian")
}

results = data.frame()
for (i in 0:10) {
  fit.name = paste0("alpha", i/10)
  predicted = predict(list.of.fits[[fit.name]],
                      s = list.of.fits[[fit.name]]$lambda.1se, newx = x[-train,])
  mse = mean((y[-train] - predicted)^2)
  temp = data.frame(alpha = i/10, mse = mse, fit.name = fit.name)
  results = rbind(results, temp)
}
results
results[order(mse)]



#Tree based method

require(tree)
#Plotting whole tree
tree.schools = tree(G3 ~ . -G2 -G1, data = schools)
summary(tree.schools)
plot(tree.schools)
text(tree.schools, pretty = 0)
#Fitting tree on test and train data
tree.schools = tree(G3 ~ . -G2 -G1, data = schools, subset = train)
plot(tree.schools);text(tree.schools, pretty = 0)
tree.pred = predict(tree.schools, schools[-train,])
mse.tree <- mean((tree.pred -schools$G3[-train])^2)
mse.tree
#Cross-validation to prune tree
cv.schools = cv.tree(tree.schools, FUN = prune.tree, K = 10)
cv.schools
plot(cv.schools)
prune.schools = prune.tree(tree.schools, best = 3)
plot(prune.schools)
text(prune.schools, pretty = 0)
tree.pred1 = predict(prune.schools, schools[-train,])
mse.tree1 = mean((tree.pred1 -schools$G3[-train])^2)
print(mse.tree1)


#Random forests 
install.packages("randomForest")
require(randomForest)
rf.schools = randomForest(G3 ~ . -G2 -G1, data = schools, subset = train)
print(rf.schools)
oob.err = double(30)
test.err = double(30)
for(mtry in 1:30){
  fit.rf = randomForest(G3 ~ . -G2 -G1, data = schools, subset = train, mtry = mtry, ntree = 400)
  oob.err[mtry]=fit.rf$mse[400]
  prd = predict(fit.rf, schools[-train,])
  test.err[mtry] = with(schools[-train,], mean((G3 - prd)^2))
}
matplot(1:mtry, cbind(test.err,oob.err),pch = 19, col = c("red","blue"),type = "b", ylab  =
          "MEAN SQUARED ERROR")
legend("topright", legend = c("OOB", "Test"), pch = 19, col = c("red","blue"))


#Boosting
install.packages("gbm")
require("gbm")
boost.schools = gbm(G3 ~ . -G2 -G1, data = schools[train,], distribution = "gaussian",
                    n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
summary(boost.schools)                    

n.trees = seq(from =100, to=10000, by = 100)
predmat = predict(boost.schools, newdata  = schools[-train,], n.trees = n.trees)
berr = with(schools[-train,], apply((predmat - G3)^2,2,mean))
plot(n.trees,berr,pch = 19, ylab = "Mean Squared error", xlab = "# of trees", main = "Boosting test error")
abline(h = min(test.err),col = "red")
#Now lets vary lambda 

lambda = c(0.00001, 0.0001, 0.001, 0.01, 0.1)
mse.boost = rep(0,5)
Counter = 0
for (i in lambda){
  boost.schools.lmb = gbm(G3 ~ . -G2 -G1, data = schools[train,], distribution = "gaussian",
                          n.trees = 10000, shrinkage = i, interaction.depth = 4)
  pred.boost = predict(boost.schools.lmb, newdata = schools[-train,], n.trees = 10000 )
  mse.boost[Counter] = mean((pred.boost - schools$G3[-train])^2)
  Counter  = Counter + 1
}

plot(lambda, mse.boost, type = "b",col = "red")
min(mse.boost)

#Now given our lambda is 0.1, we vary interaction depth
Inter.depth = seq(1:5)
Counter = 1
mse.boost.id = rep(0,5)
for (i in Inter.depth){
  boost.schools.id= gbm(G3 ~ . -G2 -G1, data = schools[train,], distribution = "gaussian",
                        n.trees = 10000, shrinkage = 0.1, interaction.depth = i)
  pred.boost = predict(boost.schools.id, newdata = schools[-train,], n.trees = 10000 )
  mse.boost.id[Counter] = mean((pred.boost - schools$G3[-train])^2)
  Counter  = Counter + 1
}
plot(Inter.depth, mse.boost.id,type = "b", col = "blue" )

#We now copy the and apply the code for the merged dataset, for both duplicate variables

#LR FIT1
fitx = lm(G3.x ~ . -G1.x -G2.x, data = schools.x, subset = train2)
summary(fit)
mean((G3.x-predict(fitx, schools.x))[-train2]^2)
#LR FIT2
fity = lm(G3.y ~ . -G1.y -G2.y, data = schools.y, subset = train2)
summary(fit)
mean((G3.y-predict(fitx, schools.y))[-train2]^2)

#Regularisation for portuguse grade
library(glmnet)
x1 = model.matrix(G3.x ~ . -G2.x -G1.x, data = schools.x)
y1 = schools.x$G3.x
fit.lasso = glmnet(x1,y1, alpha = 1)
plot(fit.lasso, xvar = "lambda", label = TRUE)
cv.lasso = cv.glmnet(x1, y1, alpha = 1)
cv.lasso
plot(cv.lasso)
coef(cv.lasso)



#Finding lambda by splitting into training and testing data
lasso.tr = glmnet(x1[train2,], y1[train2], alpha = 0.8)
pred = predict(lasso.tr, x1[-train2,])
pred
rmse = sqrt(apply((y1[-train2]-pred)^2, 2, mean))
mean((y1[-train2]-pred)^2,)
rmse
plot(log(lasso.tr$lambda), rmse, type = "b", xlab = "Log(Lambda)")
lambda.best = lasso.tr$lambda[order(rmse)[1]]
lambda.best
coef(lasso.tr, s=lambda.best)




#We compare ridge, lasso and elastic-net regression
#1)Ridge
alpha0.fit = cv.glmnet(x1[train2,], y1[train2], type.measure = "mse",
                       alpha = 0, family1 = "gaussian")
plot(alpha0.fit)
alpha0.pred = predict(alpha0.fit, s = alpha0.fit$lambda.1se, newx = x1[-train2 ,])
mean((y1[-train2 ] - alpha0.pred)^2)
#2) Lasso
alpha1.fit = cv.glmnet(x1[train2,], y1[train2], type.measure = "mse",
                       alpha = 1, family1 = "gaussian")

alpha1.pred = predict(alpha1.fit, s = alpha1.fit$lambda.1se, newx = x1[-train2,])
mean((y1[-train2] - alpha1.pred)^2)
#3)Elastic-net r
alpha0.5.fit = cv.glmnet(x1[train2 ,], y1[train2 ], type.measure = "mse",
                         alpha = 0.5, family1 = "gaussian")
alpha0.5.pred = predict(alpha0.5.fit, s = alpha0.5.fit$lambda.1se, newx = x1[-train2,])
mean((y1[-train2] - alpha0.5.pred)^2)
# we try a few more values for alpha
list.of.fits = list()
for (i in 0:10) {
  fit.name = paste0("alpha", i/10)
  list.of.fits[[fit.name]]= cv.glmnet(x1[train2 ,], y1[train2 ], type.measure = "mse",
                                      alpha = i/10, family1 = "gaussian")
}

results = data.frame()
for (i in 0:10) {
  fit.name = paste0("alpha", i/10)
  predicted = predict(list.of.fits[[fit.name]],
                      s = list.of.fits[[fit.name]]$lambda.1se, newx = x1[-train2 ,])
  mse = mean((y1[-train2 ] - predicted)^2)
  temp = data.frame(alpha = i/10, mse = mse, fit.name = fit.name)
  results = rbind(results, temp)
}
results

#Reg for math grade


library(glmnet)
x2 = model.matrix(G3.y ~ . -G2.y -G1.y, data = schools.y)
y2 = schools.y$G3.y
fit.lasso = glmnet(x2,y2, alpha = 1)
plot(fit.lasso, xvar = "lambda", label = TRUE)
cv.lasso = cv.glmnet(x2, y2, alpha = 1)
cv.lasso
plot(cv.lasso)
coef(cv.lasso)



#Finding lambda by splitting into training and testing data
lasso.tr = glmnet(x2[train2,], y2[train2], alpha = 0)
pred = predict(lasso.tr, x2[-train2,])
pred
mean((y2[-train2]-pred)^2,)

rmse = sqrt(apply((y2[-train2]-pred)^2, 2, mean))
rmse
plot(log(lasso.tr$lambda), rmse, type = "b", xlab = "Log(Lambda)")
lambda.best = lasso.tr$lambda[order(rmse)[1]]
lambda.best
coef(lasso.tr, s=lambda.best)




#We compare ridge, lasso and elastic-net regression
#1)Ridge
alpha0.fit = cv.glmnet(x2[train2,], y2[train2], type.measure = "mse",
                       alpha = 0, family = "gaussian")
plot(alpha0.fit)
alpha0.pred = predict(alpha0.fit, s = alpha0.fit$lambda.1se, newx = x2[-train2,])
mean((y2[-train2] - alpha0.pred)^2)
#2) Lasso
alpha1.fit = cv.glmnet(x2[train2,], y2[train2], type.measure = "mse",
                       alpha = 1, family = "gaussian")

alpha1.pred = predict(alpha1.fit, s = alpha1.fit$lambda.1se, newx = x2[-train2,])
mean((y2[-train2] - alpha1.pred)^2)
#3)Elastic-net r
alpha0.5.fit = cv.glmnet(x2[train2,], y2[train2], type.measure = "mse",
                         alpha = 0.5, family = "gaussian")
alpha0.5.pred = predict(alpha0.5.fit, s = alpha0.5.fit$lambda.1se, newx = x2[-train2,])
mean((y2[-train2] - alpha0.5.pred)^2)
# we try a few more values for alpha
list.of.fits = list()
for (i in 0:10) {
  fit.name = paste0("alpha", i/10)
  list.of.fits[[fit.name]]= cv.glmnet(x2[train2,], y2[train2], type.measure = "mse",
                                      alpha = i/10, family = "gaussian")
}

results = data.frame()
for (i in 0:10) {
  fit.name = paste0("alpha", i/10)
  predicted = predict(list.of.fits[[fit.name]],
                      s = list.of.fits[[fit.name]]$lambda.1se, newx = x2[-train2,])
  mse = mean((y2[-train2] - predicted)^2)
  temp = data.frame(alpha = i/10, mse = mse, fit.name = fit.name)
  results = rbind(results, temp)
}
results




#Tree based method 2

require(tree)
#Plotting whole tree
tree.schools = tree(G3.x ~ . -G2.x -G1.x, data = schools.x)
summary(tree.schools)
plot(tree.schools)
text(tree.schools, pretty = 0)
#Fitting tree on test and train data
tree.schools = tree(G3.x ~ . -G2.x -G1.x, data = schools.x, subset = train2)
plot(tree.schools);text(tree.schools, pretty = 0)
tree.pred = predict(tree.schools, schools.x[-train2,])
mse.tree <- mean((tree.pred -schools.x$G3.x[-train2])^2)
mse.tree
#Cross-validation to prune tree
cv.schools = cv.tree(tree.schools, FUN = prune.tree, K = 10)
cv.schools
plot(cv.schools)
prune.schools = prune.tree(tree.schools, best = 3)
plot(prune.schools)
text(prune.schools, pretty = 0)
tree.pred1 = predict(prune.schools, schools.x[-train2,])
mse.tree1 = mean((tree.pred1 -schools$G3.x[-train2])^2)
print(mse.tree1)


#Random forests 2
require(randomForest)
rf.schools = randomForest(G3.x ~ . -G2.x -G1.x, data = schools.x, subset = train2)
print(rf.schools)
oob.err = double(30)
test.err = double(30)
for(mtry in 1:30){
  fit.rf = randomForest(G3.x ~ . -G2.x -G1.x, data = schools.x, subset = train2, mtry = mtry, ntree = 400)
  oob.err[mtry]=fit.rf$mse[400]
  prd = predict(fit.rf, schools.x[-train2,])
  test.err[mtry] = with(schools.x[-train2,], mean((G3.x - prd)^2))
}
matplot(1:mtry, cbind(test.err,oob.err),pch = 19, col = c("red","blue"),type = "b",xlab = "Number of 
        variables considered at each node", ylab  =
          "MEAN SQUARED ERROR")
legend("topright", legend = c("OOB", "Test"), pch = 19, col = c("red","blue"))


#Tree based method 3

require(tree)
#Plotting whole tree
tree.schools = tree(G3.y ~ . -G2.y -G1.y, data = schools.y)
summary(tree.schools)
plot(tree.schools)
text(tree.schools, pretty = 0)
#Fitting tree on test and train data
tree.schools = tree(G3.y ~ . -G2.y -G1.y, data = schools.y, subset = train2)
plot(tree.schools);text(tree.schools, pretty = 0)
tree.pred = predict(tree.schools, schools.y[-train2,])
mse.tree <- mean((tree.pred -schools.y$G3.y[-train2])^2)
mse.tree
#Cross-validation to prune tree
cv.schools = cv.tree(tree.schools, FUN = prune.tree, K = 10)
cv.schools
plot(cv.schools)
prune.schools = prune.tree(tree.schools, best = 3)
plot(prune.schools)
text(prune.schools, pretty = 0)
tree.pred1 = predict(prune.schools, schools.y[-train2,])
mse.tree1 = mean((tree.pred1 -schools.y$G3.y[-train2])^2)
print(mse.tree1)


#Random forests 3
require(randomForest)
rf.schools = randomForest(G3.y ~ . -G2.y -G1.y, data = schools.y, subset = train2)
print(rf.schools)
oob.err = double(30)
test.err = double(30)
for(mtry in 1:30){
  fit.rf = randomForest(G3.y ~ . -G2.y -G1.y, data = schools.y, subset = train2, mtry = mtry, ntree = 400)
  oob.err[mtry]=fit.rf$mse[400]
  prd = predict(fit.rf, schools.y[-train2,])
  test.err[mtry] = with(schools.y[-train2,], mean((G3.y - prd)^2))
}
matplot(1:mtry, cbind(test.err,oob.err),pch = 19, col = c("red","blue"),type = "b",xlab = "Number of variables
        considered at each node", ylab  =
          "MEAN SQUARED ERROR")
legend("topright", legend = c("OOB", "Test"), pch = 19, col = c("red","blue"))




#Boosting 2
require("gbm")
boost.schools = gbm(G3.x ~ . -G2.x -G1.x, data = schools.x[train2,], distribution = "gaussian",
                    n.trees = 400, shrinkage = 0.1, interaction.depth = 1)
summary(boost.schools)                    
#We vary number of trees
n.trees = seq(from = 100, to=10000, by = 100)
predmat = predict(boost.schools, newdata  = schools.x[-train2,], n.trees = n.trees)
berr = with(schools.x[-train2,], apply((predmat - G3.x)^2,2,mean))
plot(n.trees,berr,pch = 19, ylab = "Mean Squared error", xlab = "# of trees", main = "Boosting test error")
#Now lets vary lambda given 400 trees

lambda = c(0.00001, 0.0001, 0.001, 0.01, 0.1)
mse.boost = rep(0,5)
Counter = 0
for (i in lambda){
  boost.schools.lmb = gbm(G3.x ~ . -G2.x -G1.x, data = schools.x[train2,], distribution = "gaussian",
                          n.trees = 400, shrinkage = i, interaction.depth = 4)
  pred.boost = predict(boost.schools.lmb, newdata = schools.x[-train2,], n.trees = 10000 )
  mse.boost[Counter] = mean((pred.boost - schools.x$G3.x[-train2])^2)
  Counter  = Counter + 1
}

plot(lambda, mse.boost, type = "b",col = "red")


#Now given our lambda is 0.1, we vary interaction depth
Inter.depth = seq(1:5)
Counter = 1
mse.boost.id = rep(0,5)
for (i in Inter.depth){
  boost.schools.id= gbm(G3.x ~ . -G2.x -G1.x, data = schools.x[train2,], distribution = "gaussian",
                        n.trees = 400, shrinkage = 0.1, interaction.depth = i)
  pred.boost = predict(boost.schools.id, newdata = schools.x[-train2,], n.trees = 400 )
  mse.boost.id[Counter] = mean((pred.boost - schools.x$G3.x[-train2])^2)
  Counter  = Counter + 1
}
plot(Inter.depth, mse.boost.id,type = "b", col = "blue" )
#Min interaction depth is 1

#Boosting 3
require("gbm")
boost.schools = gbm(G3.y ~ . -G2.y -G1.y, data = schools.y[train2,], distribution = "gaussian",
                    n.trees = 1100, shrinkage = 0.1, interaction.depth = 4)
summary(boost.schools)                    

n.trees = seq(from =100, to=10000, by = 100)
predmat = predict(boost.schools, newdata  = schools.y[-train2,], n.trees = n.trees)
berr = with(schools.y[-train2,], apply((predmat - G3.y)^2,2,mean))
plot(n.trees,berr,pch = 19, ylab = "Mean Squared error", xlab = "# of trees", main = "Boosting test error")
min(berr)
#optimal number of trees is 1100
#Now lets vary lambda 

lambda = c(0.00001, 0.0001, 0.001, 0.01, 0.1)
mse.boost = rep(0,5)
Counter = 0
for (i in lambda){
  boost.schools.lmb = gbm(G3.y ~ . -G2.y -G1.y, data = schools.y[train2,], distribution = "gaussian",
                          n.trees = 1100, shrinkage = i, interaction.depth = 4)
  pred.boost = predict(boost.schools.lmb, newdata = schools.y[-train2,], n.trees = 10000 )
  mse.boost[Counter] = mean((pred.boost - schools.y$G3.y[-train2])^2)
  Counter  = Counter + 1
}

plot(lambda, mse.boost, type = "b",col = "red")
min(mse.boost)
#0.1
#Now given our lambda is 0.1, we vary interaction depth
Inter.depth = seq(1:5)
Counter = 1
mse.boost.id = rep(0,5)
for (i in Inter.depth){
  boost.schools.id= gbm(G3.y ~ . -G2.y -G1.y, data = schools.y[train2,], distribution = "gaussian",
                        n.trees = 1100, shrinkage = 0.1, interaction.depth = i)
  pred.boost = predict(boost.schools.id, newdata = schools.y[-train2,], n.trees = 10000 )
  mse.boost.id[Counter] = mean((pred.boost - schools.y$G3.y[-train2])^2)
  Counter  = Counter + 1
}
plot(Inter.depth, mse.boost.id,type = "b", col = "blue" )
min(mse.boost.id)
#4 inter.depth






