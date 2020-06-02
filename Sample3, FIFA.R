library(psych)
library(GPArotation)

fifa=read.csv(file.choose())
summary(fifa)
is.numeric(fifa)
str(fifa)
fifa_2=fifa[c(55:88)]
# cleam the rows with null cells
fifa_fin=fifa_2[rowSums(is.na(fifa_2)) == 0,]

cortest.bartlett(fifa_fin, n=nrow(fifa_fin))
#reject H0

class(fifa_fin)
fifa_fin1=as.matrix(fifa_fin)

# Factor Analysis
library(stats)
f = factanal(fifa_fin1,factors=4, rotation="none")
print(f,cutoff=.4)

f = factanal(fifa_fin1,factors=4, rotation="varimax")
print(f,cutoff=.4)

f = factanal(fifa_fin1,factors=4, rotation="promax")
print(f,cutoff=.4)
 
#split fifa into left and right foot
fifa_left = filter(fifa, `Preferred.Foot` == "Left")
fifa_right = filter(fifa, `Preferred.Foot` == "Right")
#analysis left foot players
fifaleft1=fifa_left[c(55:88)]
fifaleft2=fifaleft1[rowSums(is.na(fifaleft1)) == 0,]
cortest.bartlett(fifaleft2, n=nrow(fifaleft2))
fifaleft3=as.matrix(fifaleft2)
f = factanal(fifaleft3,factors=4, rotation="promax")
print(f,cutoff=.4)

#analysis right foot plyers
fifaright1=fifa_right[c(55:88)]
fifaright2=fifaright1[rowSums(is.na(fifaright1)) == 0,]
cortest.bartlett(fifaright2, n=nrow(fifaright2))
fifaright3=as.matrix(fifaright2)
g = factanal(fifaright3,factors=5, rotation="promax")
print(g,cutoff=.4)

# Final starts here
na.omit(fifa)
fifa2=na.omit(fifa[,c(55:83)])
colnames(fifa2)
summary(fifa2)
str(fifa2)
d <- dist(as.matrix(fifa2)) 
d
hc <- hclust(d) 
plot (hc, main="Complete Linkage Dendrogram")
# K-Means Cluster Analysis
library(cluster) # needed for clusplot
library(fpc)  # needed for plotcluster
ff <- kmeans(fifa2, 4) # 4 cluster solution
aggregate(fifa2,by=list(ff$cluster),FUN=mean)

vcol <- c("blue","green","red","orange")
plotcluster(fifa2, ff$cluster, col=vcol[ff$cluster],main="KMeans Cluster FIFA")

