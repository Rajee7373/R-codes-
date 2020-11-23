# Loading wine data
winedata<-read.csv("C:/Users/Admin34/Desktop/winedata.csv") ## use read.csv for csv files
View(winedata)

help(princomp) ## to understand the api for princomp

## the first column in winedata has number 
View(winedata[-1]) 
# mydata[-1] -> Considering only numerical values for applying PCA
data <- winedata[,-1]
attach(data)
cor(data)
# cor = TRUE use correlation matrix for getting PCA scores
?princomp
pcaObj<-princomp(data, cor = TRUE, scores = TRUE, covmat = NULL)

str(pcaObj)
## princomp(mydata, cor = TRUE) not_same_as prcomp(mydata, scale=TRUE); similar, but different
summary(pcaObj)



####

str(pcaObj)
loadings(pcaObj)

plot(pcaObj) # graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)

biplot(pcaObj)

# Showing the increase of variance with considering principal components
# Which helps in choosing number of principal components
plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type="b")
#pcaObj$loadings

pcaObj$scores[,1:3] # Top 3 PCA Scores which represents the whole data

# cbind used to bind the data in column wise
# Considering top 3 principal component scores and binding them with winedata
winedata<-cbind(winedata,pcaObj$scores[,1:3])
View(winedata)


##### H CLUSTERS ON PCA SCORE###

# preparing data for clustering (considering only pca scores as they represent the entire data)
Hclus_data<-winedata[,15:17]
View(Hclus_data)

# Normalizing the data 
norm_clus<-scale(Hclus_data) # Scale function is used to normalize data
dist1<-dist(norm_clus,method = "euclidean") # method for finding the distance
# by Euclidean distance

# Clustering the data using hclust function --> Hierarchical
fit1<-hclust(dist1,method="complete") # method here is complete linkage

plot(fit1) # Displaying Dendrogram

groups<-cutree(fit1,5) # Cutting the dendrogram for 5 clusters

HC<-as.matrix(groups) # cluster numbering 

View(HC)

final1<-cbind(HC,winedata) # binding column wise with orginal data
View(final1)
View(aggregate(final1[,-c(2,16:18)],by=list(HC),FUN=mean)) 

write.csv(final1,file="PCA_HC.csv",row.names = F,col.names = F)
getwd()

### kmeans CLUSTERS ON PCA SCORES######

kc_data<-winedata[,15:17]
View(kc_data)

norm_clus<-scale(kc_data) # Scale function is used to normalize data
dist1<-dist(norm_clus,method = "euclidean") # method for finding the distance
# by Euclidean distance


wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 1:18) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:18, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

### frm the scree plot , 4 clusters 

fit <- kmeans(normalized_data,4) # 4 cluster solution
final2<- data.frame(winedata, fit$cluster) # append cluster membership
final2
aggregate(winedata[,3:16], by=list(fit$cluster),FUN=mean)

write.csv(final2,file="pca_kc.csv",row.names = F,col.names = F)
getwd()

#### K means cluster plot####


#######  
