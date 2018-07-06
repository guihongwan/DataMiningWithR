library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data
sd.data=scale(nci.data)
print(nci.labs)
par(mfrow=c(1,3))
data.dist=dist(sd.data)
plot(hclust(data.dist, method="complete"), labels=nci.labs, main="Complete")
plot(hclust(data.dist, method="average"), labels=nci.labs, main="Average")
plot(hclust(data.dist, method="single"), labels=nci.labs, main="Single")

hc.out=hclust(dist(sd.data))
hc.clusters = cutree(hc.out,4)
table(hc.clusters,nci.labs)

par(mfrow=c(1,1))
plot(hc.out, labels=nci.labs)
abline(h=139, col='red')

set.seed(2)
km.out=kmeans(sd.data, 4, nstart=20)
table(km.out$cluster)

km.clusters = km.out$cluster
table(km.clusters, hc.clusters)
