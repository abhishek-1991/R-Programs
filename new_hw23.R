require(data.table)
require(biganalytics)

#Q1 
# return: float of the cumulative proportion on tails
CoinFlip <- function(){
prop<-numeric()

for(i in 1:10000)
{
	trail<-sample(c("Heads","Tails"),i,rep=T)
	prop<-c(prop,(sum(trail=="Tails")/i))
}


plot(prop,type="l",col="blue",xlab="Number of Trails",ylab="Cumulative Proportion of Tails")
abline(0.5,0,col="red")

return(tail(prop,n=1))
}

#Q2
# populationDistribution: string('uniform','normal')
# sampleSize: integer (~30)
# numberOfSamples: integer (>100)
# return: list of two variables, mean and se
CLT <- function(populationDistribution, sampleSize, numberOfSamples){

	if(!populationDistribution %in% c("uniform","normal"))
	{
		return("Give population distribution as uniform or normal only")
	}
	else 
	{
		if(!(sampleSize > 29 && sampleSize <= 39))
		{
			return("Sample Size should be of the order of 30")
		}
		else if(numberOfSamples <= 100)
		{return("Number of samples should be greater than 100")}
	}
  	sampleMean<-numeric()
	for(i in 1:numberOfSamples)
	{
		if(populationDistribution == "uniform")
		{
			distrib <- runif(sampleSize)
		}
		if(populationDistribution == "normal")
		{
			distrib <- rnorm(sampleSize)
		}
		sampleMean<-c(sampleMean,mean(distrib))
	}
	hist(sampleMean)

  # fill in the list with the mean and std you compute
  result <- list("mean"= mean(sampleMean), "se"= sd(sampleMean))
  return(result)
}

#Q3a
# return: string('TV', 'Radio', 'Newspaper'), represents the covariate providing the best prediction
SLR <- function(path='../data/hw23R-Advertising.csv'){

  data<-read.csv(path)

  par(mfrow=c(2,2))

  fit1<-lm(data$Sales ~ data$TV, data=data)
  plot(data$TV, data$Sales, xlab="TV",ylab="Sales",main="Sales vs TV")
  abline(fit1,col="red")

  fit2<-lm(data$Sales ~ data$Radio, data=data)
  plot(data$Radio, data$Sales, xlab="Radio",ylab="Sales",main="Sales vs Radio")
  abline(fit2,col="red")

  fit3<-lm(data$Sales ~ data$Newspaper, data=data)
  plot(data$Newspaper, data$Sales, xlab="Newspaper",ylab="Sales", main="Sales vs Newspaper")
  abline(fit3,col="red")

  ls1 = cor(data$TV,data$Sales)
  ls2 = cor(data$Radio,data$Sales)
  ls3 = cor(data$Newspaper,data$Sales)
  var<-max(ls1,ls2,ls3)
  if(var == ls1)
	return("TV")
  else if(var == ls2)
	return("Radio")
  else
	return("Newspaper")
}

#Q3b 
# return: list of four variables, Intercept， TvCoeff，NewspaperCoeff，RadioCoeff
MLR <- function(path='../data/hw23R-Advertising.csv'){

  data<-read.csv(path)

  fit<-lm(data$Sales ~ data$TV + data$Radio + data$Newspaper,data=data)
  summary(fit)

  coeff<-matrix(coef(fit), dimnames=NULL)

  eq <- paste0("Sales = ", coeff[1],
             ifelse(sign(coeff[2])==1, " + ", " - "), abs(coeff[2]), " TV ",
             ifelse(sign(coeff[3])==1, " + ", " - "), abs(coeff[3]), " Radio",
      	 	 ifelse(sign(coeff[4])==1, " + ", " - "), abs(coeff[4]), " Newspaper")

  print("Equation is:")
  print(eq)

  # fill in the list with the coeffes you compute
  result <- list("Intercept"=coeff[1], "TVCoeff"=coeff[2], "NewspaperCoeff"=coeff[4], "RadioCoeff"=coeff[3])
  return(result)
}
  
#Q4
# return: list of four variables, Intercept， X1Coeff，X2Coeff，X3Coeff
LogisticRegression <- function(path='../data/hw23R-q4data.txt'){
  
  data<-read.table(path,header=TRUE)

  lfit<-glm(Y ~ ., family=binomial(link='logit'), data=data)

  lcoeff<-matrix(coef(lfit),dimnames=NULL)

  # fill in the list with the coeffes you compute
  result <- list("Intercept"=lcoeff[1], "X1Coeff"=lcoeff[2], "X2Coeff"=lcoeff[3], "X3Coeff"=lcoeff[4])
  return(result)
}

#Q5
# return: float of training accuracy 
LogisticRegressionImproved <- function(path='../data/hw23R-q4data.txt'){
 
  data<-read.table(path,header=TRUE)

  #removing outlier (row number 9)
  new.data<-data[data$X1 != 20.8,]

  lfit<-glm(Y ~ ., family=binomial(link='logit'), data=new.data)

  # Measuring accuracy
  fit.result<-predict(lfit,data.frame(X1=new.data$X1,X2=new.data$X2,X3=new.data$X3))
  fit.result<-ifelse(fit.result > 0.5,1,0)
  error<-mean(fit.result != new.data$Y)

  return(1-error)

}

#Q6
# return: list of two variables, Intercept， xCoeff
BigSLR <- function(path='../data/slr-90m-data.csv'){
  
  data<-fread(path)

  fit<-biglm.big.matrix(y~x,data=data)

  coeff<-matrix(coef(fit),dimnames=NULL)
  
  #************************6.b answer - START***********************************************************
  set.seed(123)

  sample1<-data[sample(nrow(data),(nrow(data)*0.01))]
  fit1<-lm(y~x,data=sample1)

  sample2<-data[sample(nrow(data),(nrow(data)*0.02))]
  fit2<-lm(y~x,data=sample2)

  sample3<-data[sample(nrow(data),(nrow(data)*0.03))]
  fit3<-lm(y~x,data=sample3)

  sample4<-data[sample(nrow(data),(nrow(data)*0.04))]
  fit4<-lm(y~x,data=sample4)

  sample5<-data[sample(nrow(data),(nrow(data)*0.05))]
  fit5<-lm(y~x,data=sample5)

  par(mfrow=c(3,2))
  plot(1, type="n", xlab="x", ylab="y", xlim=c(0, 10), ylim=c(0, 10),main="x vs y for 1% Sample Size")
  abline(fit1, col="red")
  plot(1, type="n", xlab="x", ylab="y", xlim=c(0, 10), ylim=c(0, 10),main="x vs y for 2% Sample Size")
  abline(fit2, col="blue")
  plot(1, type="n", xlab="x", ylab="y", xlim=c(0, 10), ylim=c(0, 10),main="x vs y for 3% Sample Size")
  abline(fit3, col="green")
  plot(1, type="n", xlab="x", ylab="y", xlim=c(0, 10), ylim=c(0, 10),main="x vs y for 4% Sample Size")
  abline(fit4, col="orange")
  plot(1, type="n", xlab="x", ylab="y", xlim=c(0, 10), ylim=c(0, 10),main="x vs y for 5% Sample Size")
  abline(fit5, col="purple")

  #****************************6.b answer - END*********************************************************

  # fill in the list with the coeffes you compute
  result <- list("Intercept"=coeff[1], "xCoeff"=coeff[2])
  return(result)
}

