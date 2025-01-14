
setwd("~/path/to/your/project/directory")

credit_card<-read.csv("CC GENERAL.csv")

dim(credit_card)
names(credit_card)
str(credit_card)
summary(credit_card)
#finding NA values
sum(is.na(credit_card))
#minimum payments has 313 na values. Replacing them with 0
library("tidyr")
library("DataExplorer")
plot_missing(credit_card)
#replacing NA for minimum payments with 0
credit_card$MINIMUM_PAYMENTS[which(is.na(credit_card$MINIMUM_PAYMENTS))]<- 0
summary(credit_card)
#there is one NA in Credit_limit . replacing it with mean of credit_limit
credit_card$CREDIT_LIMIT[which(is.na(credit_card$CREDIT_LIMIT))] <- mean(credit_card$CREDIT_LIMIT, na.rm=TRUE) 
summary(credit_card)
#CHECKING IF THERE ARE STILL ANY NA VALUES
plot_missing(credit_card)
# NO MISSING VALUES NOW
str(credit_card)
#removing first column

credit_card <- credit_card[, -1]
str(credit_card)
#scaling the data
credit_card_scaled <- scale(credit_card)
#calculating distance
credit_card_dist <- dist(credit_card_scaled , method="euclidean")
#hierarchial clustering
credit_card_hclust <- hclust(credit_card_dist, method="complete")
#plotting dendrogram
plot(credit_card_hclust, hang=-1)
#getting clusters 
credit_card_hclusters <- cutree(credit_card_hclust, h=40)
credit_card_hclusters
#creating new data frame having credit card details with the cluster that it belongs to
library("dplyr")
credit_card_withclusters<- mutate(credit_card, credit_card_hclusters)
credit_card_withclusters
count(credit_card_withclusters, credit_card_hclusters)
# hierarchical gave us 3 clusters
#checking through k means
#plotting scree or elbow plot
library("purrr")
tot_withinss <- map_dbl(1:20, function(k){
  model<- kmeans(x= credit_card_scaled, centers=k)
  model$tot.withinss
})

df_tot_withinss<- data.frame(
  k=1:20,
  tot.withinss <- tot_withinss
)
library("ggplot2")
ggplot(df_tot_withinss, aes(x=k, y=tot.withinss))+
  geom_line()+
  scale_x_continuous(breaks = 1:20)
#elbow method gave us 14
#let us try through kselection function
library("kselection")
k_selection<- kselection(credit_card_scaled,parallel=TRUE)
k_selection
#k selection gives only one cluster

#since elbow plot gave us k as 14, creating k means model with k as 14
final_k_clusters<- kmeans(credit_card_scaled, centers=14)
final_k_clusters
#trying with silinfo
library("purrr")
library("cluster")
width<- map_dbl(2:20, function(k){
  pamwidth <- pam(x=credit_card_scaled, k= k)
  pamwidth$silinfo$avg.width
})
sil_df <- data.frame(
  k = 2:20,
  sil_width = width
)

# Plot the relationship between k and sil_width
ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 2:10)

head(mtcars) # explore the dataset
d <- dist(mtcars, method = "euclidean") # calculate Euclidean distances
hc <- hclust(d) # hierarchical cluster analysis
plot(hc) # show dendogram

set.seed(20) # random-number seed / reproducibility
irisCluster <- kmeans(iris[, 3:4], 3) # three clusters based on petal size
table(irisCluster$cluster, iris$Species) # show clusters
plot(iris$Petal.Length, iris$Petal.Width, col=iris$Species, pch=19) # plot
points(irisCluster$centers) # show cluster centers

#k means on random data set
d <- data.frame(x=runif(100,min=0,max=1), y=runif(100, min=0, max=1))
#k means with k=5 to the data
cl <- kmeans (d, centers=5, nstart = 100)
#plot with cluster colours
data <- data.frame (d, cluster=factor( cl$cluster ))
plot(data[,1:2], pch =21, cex=1.5, bg = c("red","green","blue","black","grey"))

library("seriation") # also requires package scales
### create uniformly distributed data
dat <- data.frame(feature1=runif(100,min=0,max=10), feature2=runif(100,min=0,max=10))
plot(dat, xlab ="feature 1", ylab ="feature 2")
distanceMatrix <- dist(dat) # calc . diss . matrix
dissplot(distanceMatrix) # plot VAT

library("seriation") # also requires package scales
### create data set with two Gaussian distributions
x <- c(rnorm(cnt,mean =2.5), rnorm(cnt,mean = 7.5))
y <- c(rnorm(cnt,mean =2.5), rnorm(cnt,mean = 7.5))
dat <- data.frame(feature1=x,feature2=y)
plot(dat, xlim =c(0,10), ylim =c(0,10), xlab ="feature 1", ylab ="feature")
distanceMatrix <- dist(dat)
dissplot(distanceMatrix)

