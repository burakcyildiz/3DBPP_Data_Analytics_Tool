library(openxlsx)

mydf <- read.xlsx("ConsolidatedDataNew.xlsx", 
                  sheet = 1, 
                  cols = (4:7), colNames = TRUE)


CoGx = mean(mydf[,1])
CoGy = mean(mydf[,2])
CoGz = mean(mydf[,3])
CoGv = mean(mydf[,4])

mydf <- scale(as.matrix(mydf))

xAvg = mean(mydf[,1])
yAvg = mean(mydf[,2])
zAvg = mean(mydf[,3])
vAvg = mean(mydf[,4])

eucDistances = vector(,length(mydf[,1]))

for(i in 1:length(mydf[,1]))
{
  eucDistances[i] = sqrt((xAvg - mydf[i,1])^2 + (yAvg - mydf[i,2])^2 + (zAvg - mydf[i,3])^2 + (vAvg - mydf[i,4])^2);
}

avgError = sum(eucDistances) / length(mydf[,1])

#####################################################################


mydf <- read.xlsx("ConsolidatedGeneratedData.xlsx", 
                  sheet = 4, 
                  cols = (1:4), colNames = TRUE)

CoGx = mean(mydf[,1])
CoGy = mean(mydf[,2])
CoGz = mean(mydf[,3])
CoGv = mean(mydf[,4])

mydf <- scale(as.matrix(mydf))

xAvg = mean(mydf[,1])
yAvg = mean(mydf[,2])
zAvg = mean(mydf[,3])
vAvg = mean(mydf[,4])

eucDistances = vector(,length(mydf[,1]))

for(i in 1:length(mydf[,1]))
{
  eucDistances[i] = sqrt((xAvg - mydf[i,1])^2 + (yAvg - mydf[i,2])^2 + (zAvg - mydf[i,3])^2 + (vAvg - mydf[i,4])^2);
}

avgError = sum(eucDistances) / length(mydf[,1])