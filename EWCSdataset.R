#Attaching and cleaning data
ewcs1 <- read.csv("/Users/LT/Downloads/EWCS_2016.csv")
ewcs = ewcs1[2:11]
head(ewcs)
attach(ewcs)
names(ewcs)[1] <- 'Age'
View(ewcs)
ewcs[ewcs == "-999"] <- NA
for(i in 1:ncol(ewcs)){
  ewcs[,i][is.na(ewcs[,i])] = median(ewcs[,i], na.rm = TRUE)
}
dim(ewcs)

#CHECKING DATA
apply(ewcs,2,mean)
apply(ewcs,2,var)
#PRINCIPAL COMPONENTS ANALYSIS
pca.out=prcomp(ewcs,scale=TRUE)
pca.out
summary(pca.out)
pca.out$rotation=-pca.out$rotation
pca.out$x=-pca.out$x
biplot(pca.out,scale=0,cex = 0.5, main="PCA Biplot for all the
observations",xlab="First Principal Component", ylab="Second Principal
Component")
pca.out100=prcomp(ewcs[1:100,],scale=TRUE)
pca.out100$rotation=-pca.out100$rotation
pca.out100$x=-pca.out100$x
biplot(pca.out100,scale=0,main="PCA Biplot for the first 100
observations",xlab="First Principal Component", ylab="Second Principal
Component")
plot(pca.out, type = "l", main = "Elbow plot", col = "red")
pr.var=pca.out$sdev ^2
pr.var
pve=pr.var/sum(pr.var)
pve
par(mfrow = c(1,2))
plot(pve,xlab=" Principal Component", ylab="Proportion of Variance Explained",
     ylim=c(0,1),type="b")
plot(cumsum(pve), xlab="Principal Component ", ylab="
Cumulative Proportion of Variance Explained ", ylim=c(0,1),
       type="b")
par(mfrow = c(1,1))

#K-means clustering
set.seed(1)
km.out=kmeans(ewcs,3, nstart = 50)
km.out$tot.withinss
set.seed(2)
km.out=kmeans(ewcs,3, nstart = 20)
km.out$tot.withinss

plot(pca.out$x[,1:2], col=(km.out$cluster +1), main="K-Means Clustering
Results with K=3", xlab="", ylab="", pch=20, cex=2)
#Choosing K
k = list()
for (i in 1:10){
  k[[i]] = kmeans(ewcs, i, nstart = 20)
}
ss = list()
for (i in 1:10){
  ss[[i]] = k[[i]]$betweenss/k[[i]]$totss
}
plot(1:10, ss, type = "b", ylab = "Between ss / Total ss", xlab = "K Clusters")

#Hierarchical clustering
hc.complete =hclust(dist(ewcs), method="complete")
hc.average =hclust(dist(ewcs), method="average")
par(mfrow=c(1,2))
plot(hc.complete ,main="Complete Linkage ", xlab="", sub="",
       cex=.9)
cutree(hc.complete , 2)
plot(hc.average , main="Average Linkage", xlab="", sub="",
       cex=.9)





