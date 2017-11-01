#Answer of Q9.1
set.seed(1000)
norm = rnorm(100)
mean(norm)
sd(norm)
var(norm)
median(norm)
quantile(norm)


#Answer of Q9.2
png('Histogram.png')
hist(norm, breaks=15)
dev.off()


#Answer of Q9.3
qqnorm(norm);qqline(norm)


#Answer of Q9.4
dat=data.frame(rnorm(100,68,8),rnorm(100,60,10))
colnames(dat) = c("Height","Weight")
summary(dat)				#For finding summary
png('Bargraph.png')
barplot(as.matrix(dat))			#For Bar Graph
dev.off()
png('Boxplot.png')
boxplot(dat)				#For Box Plot
dev.off()
png('Scatterplot.png')
plot(dat)					#For Scatter Plot
dev.off()


#Answer of Q9.5
hdensity=dnorm(dat[,1],68,8)
png('NormalDistribHeight.png')
plot(dat[,1],hdensity, xlab="Height", ylab="Density")
dev.off()
wdensity=dnorm(dat[,2],60,10)
png('NormalDistribWeight.png')
plot(dat[,2],wdensity, xlab="Weight", ylab="Density")
dev.off()


#Answer of Q9.6
library(mvtnorm)
x = sort(dat[,1])
y = sort(dat[,2])
z = matrix(0,nrow=100,ncol=100)
mu=c(68,60)
sigma <- matrix(c(2,1,1,1),nrow=2)
for (i in 1:100) {
for (j in 1:100) {
z[i,j] <- dmvnorm(c(x[i],y[j]),
mean=mu,sigma=sigma)
}
}
png('Bivariate.png')
contour(x,y,z)
dev.off()
