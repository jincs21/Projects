bank = read.csv("/Users/LT/Downloads/bank.csv", sep = ";")
str(bank)
summary(bank)
str(bank1)
bank[c(-1, -6, -10, -12, -13, -14, -15)] <- lapply(bank[c(-1, -6, -10, -12, -13, -14, -15)], as.factor)
sum(is.na(bank$duration))
bank[bank == -1] = NA
names(bank)[17] = 'Subscribed'
bank1 = subset(bank, select = -c(duration, pdays))
dim(bank1)
bank1 = na.omit(bank1)
#pairs(bank1, col = bank1$Subscribed)
#plot(bank1)
prop.table(table(Subscribed))
require(pROC)
ls(rf)
attach(bank1)

#Logistic regression
glm.fit = glm(Subscribed ~ ., data = bank1, family = binomial)
summary(glm.fit)
glm.probs = predict(glm.fit, type = "response")
glm.pred = ifelse(glm.probs > 0.5, "yes", "no")
glm.probs[1:5]
attach(bank1)
contrasts(Subscribed)
table(glm.pred, Subscribed)
mean(glm.pred == Subscribed)

#TRAINING AND TEST SET
train = day < 30

#sum(day == 30)
glm.fit = glm(Subscribed ~ ., data = bank1, family = binomial, subset = train)
glm.probs = predict(glm.fit, newdata = bank1[!train,], type = "response")
glm.pred = ifelse(glm.probs > 0.5, "yes", "no")
bank1.30 = bank1$Subscribed[!train]
table(glm.pred,bank1.30)
mean(glm.pred == bank1.30)
#Plotting logistic regresison
pred.data <- data.frame(
  prob.subs=glm.probs,
  subs=bank1$Subscribed)
pred.data <- pred.data[
  order(pred.data$prob.subs, decreasing=FALSE),]
pred.data$rank <- 1:nrow(pred.data)
library(ggplot2)
library(cowplot)
ggplot(data=pred.data, aes(x=rank, y=prob.subs)) +
  geom_point(aes(color=Subscribed), alpha = 1, shape = Subscribed, stroke = 1) +
  scale_shape_manual(values = c(3,4)) +
  xlab("Index") +
  ylab("Predicted probability of subscribing")


#Tree based method
train = sample(1:nrow(bank1), 3000)
require(tree)
#Plotting whole tree
tree.bank1 = tree(Subscribed ~., data = bank1)
summary(tree.bank1)
plot(tree.bank1)
text(tree.bank1, pretty = 0)
#Fitting tree on test and train data
tree.bank1 = tree(Subscribed ~., data = bank1, subset = train)
plot(tree.bank1);text(tree.bank1, pretty = 0)
tree.pred = predict(tree.bank1, bank1[-train,], type = "class")


with(bank1[-train,], table(tree.pred, Subscribed))
(1310 + 33)/(1310 + 142 + 31 + 33)

#Cross-validation to prune tree

cv.bank1 = cv.tree(tree.bank1, FUN = prune.misclass)
cv.bank1
plot(cv.bank1)
prune.bank1 = prune.tree(tree.bank1, best = 3)
plot(prune.bank1)
text(prune.bank1, pretty = 0)
tree.pred = predict(prune.bank1, bank1[-train,], type = "class")
with(bank1[-train,], table(tree.pred, Subscribed))
(1316 + 30)/(1316 + 30 + 153 + 17)


#Random forests
library(ggplot2)
library(cowplot)
require(randomForest)
rf = randomForest(Subscribed ~ ., data = bank1, proximity = TRUE)
print(rf)
oob.error.data <- data.frame(
  Trees=rep(1:nrow(rf$err.rate), times=3),
  Type=rep(c("OOB", "yes", "no"), each=nrow(rf$err.rate)),
  Error=c(rf$err.rate[,"OOB"], 
          rf$err.rate[,"yes"], 
          rf$err.rate[,"no"]))
ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type)) +
  scale_fill_discrete(name = "Type", labels = c("Unsubscribed", "OOB", "Subscribed"))
  + labs(title = "Error rate by number of trees")
rf2 = randomForest(Subscribed ~ ., data = bank1, proximity = TRUE, ntree = 1000)
print(rf)
(3932 + 87)/(3923 + 63 + 434 + 87)
#We now consider number of variables at each internal node of tree
oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(Subscribed ~ ., data = bank1, mtry=i)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values
#Two variables, gives us lowest error rate, i.e mtry = 2
rf2 = randomForest(Subscribed ~ ., data = bank1, proximity = TRUE, ntree = 1000, mtry = 2)
print(rf)




#Support vector machine
require(e1071)
train = sample(dim(bank1)[1], 3000)
bank1.train = bank1[train, ]
bank1.test = bank1[-train, ]

svm.bank1 = svm(Subscribed ~ ., kernel = "linear", data = bank1.train, cost = 0.01)
summary(svm.bank1)


tune.bank1 = tune(svm, Subscribed ~ ., data = bank1.train, kernel = "linear", ranges = list(cost = 10^seq(-2, 1, by = 0.25)))
summary(tune.bank1)

svm2 = svm(Subscribed ~ ., kernel = "linear", data = bank1.train, cost = tune.bank1$best.parameters$cost)


test.pred = predict(svm2, bank1.test)
table(bank1.test$Subscribed, test.pred)
(146 + 13) / (1329 + 13 + 146 + 28)
#Both training and test error have decreased by about 1% by using cost = 0.1

#Polynomial and radial kernel
svm.radial = svm(Subscribed ~ ., data = bank1.train, kernel = "radial")
summary(svm.radial)




tune.svm = tune(svm, Subscribed ~ ., data = bank1.train, kernel = "radial", gamma = c(0.1, 1),
ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100)))
                                                                                                   
summary(tune.svm)

svm.radial = svm(Subscribed ~ ., data = bank1.train, kernel = "radial", cost = tune.svm$best.parameters$cost,
                 gamma = tune.svm$best.parameters$gamma) 
              


test.pred = predict(svm.radial, bank1.test)

table(bank1.test$Subscribed, test.pred)
(146 + 12)/(1330 + 12 + 146 + 28)
#0.1042216 test error, a 1% improvement over linear kerner


tune.out = tune(svm, Subscribed ~ ., data = bank1.train, kernel = "poly", degree = c(2, 3, 4), 
                ranges = list(cost = 10^seq(-2, 1, by = 0.25)))
summary(tune.out)

svm.poly = svm(Subscribed ~ ., data = bank1.train, kernel = "poly", degree = tune.out$best.parameters, cost = tune.out$best.parameters$cost)

test.pred = predict(svm.poly, bank1.test)
table(bank1.test$Subscribed, test.pred)
(171 + 2)/(171 + 2 + 1340 + 3)
#0.1141161, again small difference between test and train error


#Overall, radial basis kernel seems to be producing minimum misclassification
#error on both train and test data, and polynomial kernel seems to do the same
#as linear kernel in terms of misclassification errors 

#Linear Discriminant Analysis
require(MASS)
attach(bank1)
train = day < 30
lda.fit = lda(Subscribed ~ ., data = bank1, subset = train)
lda.fit
plot(lda.fit)
bank1.30 = subset(bank1, day == 30)
lda.pred = predict(lda.fit, bank1.30)
table(lda.pred$class, bank1.30$Subscribed) 
mean(lda.pred$class == bank1.30$Subscribed)
ls(lda.fit)
lda.fit$counts

#ROC and AUC curves to compare results
library(pROC)
par(pty = "s")
roc.logit = roc(bank1.30, glm.probs, plot = TRUE, legacy.axes = TRUE
                ,percent =  TRUE, xlab = "False Positive Percentage", ylab = "True Positive Percentage",
                print.auc = TRUE ,lwd = 4, col = "blue", main = "ROC Curves",print.auc.x = 25)
roc.rf = plot.roc(Subscribed, rf$votes[,1], percent = TRUE, col = "green",
                  lwd = 4, print.auc = TRUE, add = TRUE, print.auc.y = 40, print.auc.x = 25)
roc.lda = plot.roc(bank1.30$Subscribed, lda.pred$posterior[,2], percent = TRUE, col = "red",
                  lwd = 4, print.auc = TRUE, add = TRUE, print.auc.y = 60, print.auc.x = 25)
roc.svm = plot.roc(bank1.test$Subscribed, as.numeric(test.pred), percent = TRUE, col = "yellow",
                   lwd = 4, print.auc = TRUE, add = TRUE, print.auc.y = 35,print.auc.x = 25)

legend("bottomright", legend = c("LDA", "Logistic regression", "Random forests", "SVM"),
       col = c("red", "blue", "green", "yellow"), lwd = 4)




par(pty = "s")
roc.tree = roc(tree.bank1, tree.pred, plot = TRUE, legacy.axes = TRUE
                ,percent =  TRUE, xlab = "False Positive Percentage", ylab = "True Positive Percentage",
                print.auc = TRUE ,lwd = 4, col = "blue", main = "ROC Curves",print.auc.x = 25)