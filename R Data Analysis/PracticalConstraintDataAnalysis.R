library(openxlsx)
library(plotly)

mydf <- read.xlsx("ConsolidatedDataNew.xlsx", 
                  sheet = 1, 
                  cols = (1:8), colNames = TRUE)
mydf[,5] <- mydf[,5] 
vol <- mydf[,1] * mydf[,2] * mydf[,3]
vol <- vol/1000000
area <- mydf[,1] * mydf[,2]
area <- area/1000

correlation <- cor(mydf[,4]/vol,mydf[,5])
cor(mydf[,4],mydf[,5])
correlation <- cor(vol, mydf[,4])
cor(mydf[,4]/area,mydf[,5])
volScaled <- scale(vol)
weightScaled <- scale(mydf[,4])

newMat <- cbind(mydf[,4]/vol,mydf[,5])
newMat <- newMat[order(newMat[,1]),]
plot(newMat[,2],type='p',col="red",xlab="foo",ylab="bar")
lines(newMat[,1],col="blue")

xScaler = 1000;
yScaler = 100;
mat <- matrix(, nrow = (2600 - 200) / yScaler, ncol = 60700 / xScaler);
ct = integer((2600 - 200) / yScaler);
counter = 0;
matCounter = 1;
for (i in 1:length(newMat2[,1])) {
  if(i > 15000 && newMat2[i,2] < 600)
  {
  }
  else
  {
    ct[ceiling((newMat2[i,2] - 200) / yScaler)] = ct[ceiling((newMat2[i,2] - 200) / yScaler)] + 1;
  }
  counter = counter + 1;
  if(counter == xScaler){
    mat[,matCounter] <- ct;
    ct = integer((2600 - 200) / yScaler);
    counter = 0;
    matCounter = matCounter + 1;
  }
}

p <- plot_ly(z=mat, type = "heatmap")
p

mat2 = mat
mat2[mat2 < 60] = 0;

p <- plot_ly(z=mat2, type = "heatmap")
p

#Correlation between density and loadcap = 0.71
#Correlation between volume and weight = 0.21
#Generate weight alongside a distribution,
#then generate loadCap in relation to density

maxLoadCap <- max(mydf[,5])
coefficients <- runif(length(mydf[,4]),min=0.7,max=1.3)*(mydf[,4])
for(i in 1:length(coefficients))
{
  if(coefficients[i] > maxLoadCap)
    coefficients[i] = maxLoadCap
}

newMat <- cbind(mydf[,4]/vol,coefficients)
newMat <- newMat[order(newMat[,1]),]
plot(newMat[,2],type='p',col="blue")
lines(newMat[,1],col="red")


newMat <- cbind(mydf[,4]/vol,mydf[,5])
newMat <- newMat[order(newMat[,1]),]
plot(newMat[,2])

straight1 <- integer()
coord <- integer()
for(i in 1:35000)
{
  if(newMat[i,2] == 2000)
  {
    straight1 <- c(straight1, newMat[i,2])
    coord <- c(coord, i)
  }
}
strMat1 <- cbind(coord,straight1)
plot(strMat1)

straight11 <- integer()
coord <- integer()
for(i in 35001:length(newMat[,2]))
{
  if(newMat[i,2] == 2000)
  {
    straight11 <- c(straight11, newMat[i,2])
    coord <- c(coord, i)
  }
}
strMat11 <- cbind(coord,straight11)
plot(strMat11)

straight4 <- integer()
coord <- integer()
for(i in 35001:length(newMat[,2]))
{
  if(newMat[i,2] == 2600)
  {
    straight4 <- c(straight4, newMat[i,2])
    coord <- c(coord, i)
  }
}
strMat4 <- cbind(coord,straight4)
plot(strMat4)

straight2 <- integer()
coord <- integer()
for(i in 1:length(newMat[,2]))
{
  if(newMat[i,2] == 2600)
  {
    straight2 <- c(straight2, newMat[i,2])
    coord <- c(coord, i)
  }
}
strMat2 <- cbind(coord,straight2)
plot(strMat2)

loadCapSplice <- newMat[(1:20000),2]
plot(loadCapSplice)

straight3 <- integer()
coord <- integer()
for(i in 1:length(newMat[,2]))
{
  if(newMat[i,2] == 250)
  {
    straight3 <- c(straight3, newMat[i,2])
    coord <- c(coord, i)
  }
}
strMat3 <- cbind(coord,straight3)
plot(strMat3)

curve1 <- integer()
coord <- integer()
for(i in 1:length(loadCapSplice))
{
  if(loadCapSplice[i] > 300 && loadCapSplice[i] > (0.039*i) + 206 && loadCapSplice[i] > (0.143*i)-1252)
  {
    curve1 <- c(curve1, loadCapSplice[i])
    coord <- c(coord, i)
  }
}
crvMat1 <- cbind(coord, curve1)
plot(crvMat1)

curve2 <- integer()
coord <- integer()
for(i in 1:length(loadCapSplice))
{
  if(loadCapSplice[i] > 300 && (loadCapSplice[i] < (0.039*i) + 206 || loadCapSplice[i] < (0.143*i)-1252) && (loadCapSplice[i] > (0.045*i) + 20) && loadCapSplice[i] > (0.093*i)-620)
  {
    curve2 <- c(curve2, loadCapSplice[i])
    coord <- c(coord, i)
  }
}
crvMat2 <- cbind(coord, curve2)
plot(crvMat2)

curve3 <- integer()
coord <- integer()
for(i in 1:length(loadCapSplice))
{
  if(loadCapSplice[i] > 300 && (loadCapSplice[i] < (0.035*i) +50 || loadCapSplice[i] < (0.080*i)-600) && loadCapSplice[i] > (0.080*i)-750 && loadCapSplice[i] > (0.035*i) -50)
  {
    curve3 <- c(curve3, loadCapSplice[i])
    coord <- c(coord, i)
  }
}
crvMat3 <- cbind(coord, curve3)
plot(crvMat3)

curve4 <- integer()
coord <- integer()
for(i in 20000:35000)
{
  if(newMat[i,2] < 2000 && newMat[i,2] < 0.027*i + 750 && newMat[i,2] > 0.02*i + 550)
  {
    curve4 <- c(curve4, newMat[i,2])
    coord <- c(coord, i)
  }
}
crvMat4 <- cbind(coord, curve4)
plot(crvMat4)

curve51 <- integer()
coord <- integer()
for(i in 19001:35000)
{
  if(newMat[i,2] < 2000 && newMat[i,2] < 0.015*i + 660 && newMat[i,2] > 0.02*i + 300)
  {
    curve51 <- c(curve51, newMat[i,2])
    coord <- c(coord, i)
  }
}
crvMat51 <- cbind(coord, curve51)
plot(crvMat51)

curve52 <- integer()
coord <- integer()
for(i in 35001:length(newMat[,2]))
{
  if(newMat[i,2] < 2000 && newMat[i,2] < 0.015*i + 660 && newMat[i,2] > 0.02*i + 300)
  {
    curve52 <- c(curve52, newMat[i,2])
    coord <- c(coord, i)
  }
}
crvMat52 <- cbind(coord, curve52)
plot(crvMat52)

curve5 <- integer()
coord <- integer()
for(i in 19001:length(newMat[,2]))
{
  if(newMat[i,2] < 2000 && newMat[i,2] < 0.015*i + 660 && newMat[i,2] > 0.02*i + 300)
  {
    curve5 <- c(curve5, newMat[i,2])
    coord <- c(coord, i)
  }
}
crvMat5 <- cbind(coord, curve5)
plot(crvMat5)

finMat <- strMat1
finMat <- rbind(finMat, strMat11)
finMat <- rbind(finMat, strMat2)
finMat <- rbind(finMat, strMat3)
finMat <- rbind(finMat, strMat4)
finMat <- rbind(finMat, crvMat1)
finMat <- rbind(finMat, crvMat2)
finMat <- rbind(finMat, crvMat3)
finMat <- rbind(finMat, crvMat4)
finMat <- rbind(finMat, crvMat51)
finMat <- rbind(finMat, crvMat52)
plot(finMat, col="red", xlab="index",ylab="load capacity (g/mm^2)")

finMat <- finMat[order(finMat[,1]),]
coord <- 1:length(mydf[,4])
densityMat <- cbind(coord,sort(mydf[,4]/vol))

plot(newMat[,2],type='p',col="red",xlab="index",ylab="value")
lines(newMat[,1],col="blue",lwd=3)

plot(x=finMat[,1],y=scale(finMat[,2]),type='p', col="red",xlab="index",ylab="value")
lines(x=densityMat[,1],y=scale(densityMat[,2]),col="blue",lwd=3)

finMatSec1 <- integer()
coord <- integer()
for(i in 1:length(finMat[,2]))
{
  if(finMat[i,1] > 19000 && finMat[i,1] <= 35000)
  {
    coord <- c(coord, finMat[i,1])
    finMatSec1 <- c(finMatSec1, finMat[i,2])
  }
}
finMatSec1 <- cbind(coord,finMatSec1)
densityMatSec1 <- integer()
coord <- integer()
for(i in 1:length(densityMat[,2]))
{
  if(densityMat[i,1] > 19000 && finMat[i,1] <= 35000)
  {
    coord <- c(coord, densityMat[i,1])
    densityMatSec1 <- c(densityMatSec1, densityMat[i,2])
  }
}
densityMatSec1 <- cbind(coord,densityMatSec1)

plot(x=finMatSec1[,1],y=scale(finMatSec1[,2]),type='p', col="red",xlab="index",ylab="value")
lines(x=densityMatSec1[,1],y=scale(densityMatSec1[,2]),col="blue",lwd=3)

plot(finMat[19001:35000],type='p', col="red",xlab="foo",ylab="bar")
lines(densityMat[19001:35000],col="blue",lwd=3)

plot(finMat[1:19000],type='p', col="red",xlab="foo",ylab="bar")
lines(densityMat[1:19000],col="blue",lwd=3)



#width-depth reduce
widthReduce <- mydf[,6]
depthReduce <- mydf[,7]
wdMat <- cbind(widthReduce,depthReduce)
newWR <- integer()
newDR <- integer()
for(i in 1:length(wdMat[,1]))
{
  if(wdMat[i,1] > 0)
  {
    newWR <- c(newWR,wdMat[i,1])
    newDR <- c(newDR, wdMat[i,2])
  }
}
wdMat <- cbind(newWR,newDR)
wdMat <- wdMat[order(wdMat[,1]),]

cor(widthReduce,depthReduce)
plot(sort(widthReduce),type='l', col="red",xlab="index",ylab="value")
lines(sort(depthReduce),col="blue",lwd=3)

difference <- widthReduce - depthReduce


library(fitdistrplus)
x <- as.vector(as.matrix(difference))

fitn <- fitdist(x, "norm", discrete = TRUE)
fitg <- fitdist(x, "gamma", discrete = TRUE)
fitw <- fitdist(x, "weibull", discrete = TRUE)
fitln <- fitdist(x, "lnorm", discrete = TRUE)
#fitp <- fitdist(x, "pois", discrete = TRUE)
fite  <- fitdist(x, "exp", discrete = TRUE)

gofstat(list(fitg,fitn,fitw,fitln),fitnames=c("gamma","normal","weibull","lnorm"))

plot(fitg)
plot(fitn)
plot(fitw)
plot(fitln)
plot(fite)

summary(fitln)
plot(fite)
plot(fitw, demp = TRUE)
plot(fitn, histo = FALSE, demp = TRUE)
cdfcomp(fitln)
denscomp(fitw, addlegend=FALSE)
ppcomp(fitg, addlegend=FALSE)
qqcomp(fitg, addlegend=FALSE)

gofstat(fitg)



library(ecdf)
newVec <- sort(mydf[,4]/vol)
x <- as.vector(as.matrix(newVec[1:19000]))
y <- as.vector(as.matrix(newVec[19001:length(newVec)]))
z <- as.vector(as.matrix(newVec[58001:length(newVec)]))

descdist(x, discrete = TRUE, boot = NULL, method = "unbiased",
         graph = TRUE, obs.col = "darkblue", obs.pch = 16, boot.col = "orange")

fit <- fitdist(y, "lnorm", method = c("mle", "mme", "qme", "mge"),
               start=NULL, fix.arg=NULL, discrete = TRUE)
plot(fit)
gofstat(fit)
cdfcomp(fit, addlegend=FALSE)
denscomp(fit, addlegend=FALSE)


Fn <- ecdf(x)
density(Fn)

Fn     # a *function*
Fn(x)  # returns the percentiles for x
tt <- seq(-2, 2, by = 0.1)
12 * Fn(tt) # Fn is a 'simple' function {with values k/12}
summary(Fn)
##--> see below for graphics
knots(Fn)  # the unique data values {12 of them if there were no ties}