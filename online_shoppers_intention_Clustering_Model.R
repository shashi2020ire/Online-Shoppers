#K-mens Clustering
#Loading required packages
library(factoextra)
library(cluster)
library(ggplot2)

#Impoting data file
data_onlineint=read.csv("G:/NCI/Data Mining/Project/Online Shopers_Clustering/online_shoppers_intention.csv")
factor1<-factor(data_onlineint$Revenue)
factor2<-factor(data_onlineint$Weekend)
factor3<-factor(data_onlineint$VisitorType)
factor4<-factor(data_onlineint$TrafficType)
factor5<-factor(data_onlineint$Region)
factor6<-factor(data_onlineint$Browser)
factor7<-factor(data_onlineint$OperatingSystems)
factor8<-factor(data_onlineint$Month)

# missing values
missmap(data_onlineint, main = "Missing values vs observed")
is.na(data_onlineint)

#Ploting graphs of factors
par(mfcol=c(3,3))
plot(factor1,main="Histogram of Revenue")
plot(factor2,main="Histogram of Weekend")
plot(factor3,main="Histogram of VisitorType")
plot(factor4,main="Histogram of TrafficType")
plot(factor5,main="Histogram of Region")
plot(factor6,main="Histogram of Browser")
plot(factor7,main="Histogram of OperatingSystems")
plot(factor8,main="Histogram of Month")

#Delete missing variable
data_onlineint=data_onlineint[,-11]
data_onlineint=data_onlineint[,-15]


#Identifying the missing data and delete the rows
data_onlineint=data_onlineint[complete.cases(data_onlineint),]
data_onlineint<-scale(data_onlineint)
summary(data_onlineint)

#Elbow method for clustering
elbow_data<-NULL
for (i in 1:10) {
  clustering<-kmeans(data_onlineint,centers=i,nstart=30)
  elbow_data<-rbind(elbow_data,data.frame(total_ss = clustering$tot.withinss,k=i))
}
ggplot(data=elbow_data,aes(x=k,y=total_ss))+geom_line()+geom_point()+
  ylab("Total within cluster sum of squares")+xlab("Number of clusters")

#Elbow method with k=4
k<-4
clustering<-kmeans(data_onlineint,centers=k,nstart=30)
data_onlineint1<-cbind(data_onlineint,clustering$cluster)
summary(data_onlineint1)

#Results of Elbow method with k=4
table(data_onlineint1[,17])
fviz_cluster(clustering,data_onlineint)

#Elbow method with k=3
k<-3
clustering<-kmeans(data_onlineint,centers=k,nstart=30)
data_onlineint1<-cbind(data_onlineint,clustering$cluster)
summary(data_onlineint1)

#Results of Elbow method with k=4
table(data_onlineint1[,17])
fviz_cluster(clustering,data_onlineint)

