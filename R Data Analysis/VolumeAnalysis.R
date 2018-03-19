library(openxlsx)

mydf <- read.xlsx("ConsolidatedData.xlsx", 
                  sheet = 1, 
                  cols = (1:3), colNames = TRUE)

data = as.matrix(mydf)

volumes = as.matrix(data[,1] * data[,2] * data[,3] / 1000000)

volumes <- scale(volumes)

wss <- (nrow(volumes)-1)*sum(apply(volumes,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(volumes, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(volumes, 5) # 5 cluster solution
# get cluster means 
aggregate(volumes,by=list(fit$cluster),FUN=mean)
# append cluster assignment
volumesClustered <- data.frame(volumes, fit$cluster)
clusters = as.matrix(fit$cluster)

write.xlsx(clusters,"Results.xlsx")

mydf <- read.xlsx("ConsolidatedData.xlsx", 
                  sheet = 2, 
                  cols = (1:5), colNames = TRUE)

volPercentageData = as.matrix(mydf)
volPercentageData <- scale(volPercentageData)

wss <- (nrow(volPercentageData)-1)*sum(apply(volPercentageData,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(volPercentageData, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(volPercentageData, 2) # 5 cluster solution
# get cluster means 
aggregate(volPercentageData,by=list(fit$cluster),FUN=mean)
# append cluster assignment
volPercentageData <- data.frame(volPercentageData, fit$cluster)
clusters = as.matrix(fit$cluster)

######################################

