
library(data.table)
library(igraph)
library(splines)

ends_all<- fread('Ends-All_1990-2015_Hash.csv')

## GC
ends_unique<- ends_all[,.(.N),by=.(Company.F,Company.T)]
colnames(ends_unique)[3]<- 'Moves'
setorder(ends_unique,-Moves)
g_all<- graph_from_data_frame(ends_unique,directed = F)
E(g_all)$weight<- ends_unique$Moves

com<- components(g_all)
id<- which(com$membership!=which.max(com$csize))
g_all<- g_all-vertices(V(g_all)$name[id])

ends_all<- ends_all[which(Company.F%in%V(g_all)$name)]
ends_all<- ends_all[which(Company.T%in%V(g_all)$name)]

##
work_period<- ends_all[,.(.N),Month.F]
id<- which(work_period$Month.F>730)
work_period<- work_period[-id,]
work_period$dis<- work_period$N/sum(work_period$N)
data.frame(work_period)

n<- 50
source('BIN.R')
dis_bin<- bin(work_period$Month.F,work_period$dis,n)

x<- dis_bin$bin_midpoint
y<- mm
plot(x,y,xlab='Work Period(Month)',ylab='Distributin')
fit <- lm(formula = y ~ bs(x, degree = 1, knots = c(100)))
x.predict <- seq(min(x),max(x),len = 100)
lines(x.predict, predict(fit, data.frame(x = x.predict)), col =2, lwd = 2)
summary(fit)

knot.boundary.left <- min(x)
knot <- 100
knot.boundary.right <- max(x)
slope.1 <- summary(fit)$coefficients[2,1] /(knot - knot.boundary.left)
slope.2 <- (summary(fit)$coefficients[3,1] - summary(fit)$coefficients[2,1]) / (knot.boundary.right - knot)
