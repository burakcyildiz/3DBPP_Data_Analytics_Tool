library(openxlsx)

mydf <- read.xlsx("ConsolidatedData.xlsx", 
                  sheet = 1, 
                  cols = (1:3), colNames = TRUE);

library(fitdistrplus)

mydf2 <- read.xlsx("ConsolidatedGeneratedData.xlsx",
                   sheet = 1,
                   cols = (1:3), colNames = TRUE);

ks.test(mydf[,2]/mydf[,1],mydf2[,2]/mydf2[,1])
ks.test(mydf[,3]/mydf[,1],mydf2[,3]/mydf2[,1])
ks.test(mydf[,1]*mydf[,2]*mydf[,3],mydf2[,1]*mydf2[,2]*mydf2[,3])


fitn <- fitdist(as.vector(as.matrix(mydf)), "norm", discrete = TRUE)
fitg <- fitdist(as.vector(as.matrix(mydf)), "gamma", discrete = TRUE)
fitw <- fitdist(as.vector(as.matrix(mydf)), "weibull", discrete = TRUE)
fitln <- fitdist(as.vector(as.matrix(mydf)), "lnorm", discrete = TRUE)
fitp <- fitdist(as.vector(as.matrix(mydf)), "pois", discrete = TRUE)
fite  <- fitdist(as.vector(as.matrix(mydf)), "exp", discrete = TRUE)

gofstat(list(fitg,fitn,fitw,fitln),fitnames=c("gamma","normal","weibull","lnorm"))

plot(fitg)
plot(fitn)
plot(fitw)
plot(fitln)

summary(fitn)
plot(fitn)
plot(fitn, demp = TRUE)
plot(fitn, histo = FALSE, demp = TRUE)
cdfcomp(fitln, addlegend=FALSE)
denscomp(fitln, addlegend=FALSE)
ppcomp(fitn, addlegend=FALSE)
qqcomp(fitn, addlegend=FALSE)

gofstat(fitg)



library(ecdf)

x <- as.vector(as.matrix(mydf))

descdist(x, discrete = TRUE, boot = NULL, method = "unbiased",
         graph = TRUE, obs.col = "darkblue", obs.pch = 16, boot.col = "orange")

fit <- fitdist(x, "lnorm", method = c("mle", "mme", "qme", "mge"),
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