require(ISLR)

#Q1
# Inputs:
#  train_data: dataframe, including training samples
#  validation_data: dataframe, including validation samples
#  include_student: boolean, indicating whether using the student variable in logistic regression
# return: 
#  result: float, test error(validation error), range in [0, 1]
LogisticRegression <- function(train_data, validation_data, include_student=FALSE){
  #Will execute when include_student is False
  if(include_student == FALSE)
  {
    fit <- glm(default ~ income + balance, data=train_data,family=binomial (link="logit"))
  
    fitted_result <- predict(fit, validation_data,type="response")
  
    fitted_result <- ifelse(fitted_result > 0.5,"Yes","No")
  }
  else
  {
    #Will execute when include_student is False
    fit <- glm(default ~ student + income + balance, data=train_data,family=binomial (link="logit"))
    
    fitted_result <- predict(fit, validation_data,type="response")
    
    fitted_result <- ifelse(fitted_result > 0.5,"Yes","No")
  }
  
  result <- mean(fitted_result != validation_data$default)
  return(result)
}

# This function is for splitting the data into Training Set and Testing Set
# Inputs:
#  Data: The data that is to be splitted
#  fraction: The fraction of data that will be Training Set
# return:
#  list: The list containing two data frames training.data and test.data 
splitData <- function(Data,fraction)
{
  if(fraction <= 0 || fraction > 1)
  {
    print("Insert Proper fraction !!!")
    return(0)
  }
  set.seed(101)
  dataIndex <- sample(1:nrow(Data),size=fraction*nrow(Data),replace = FALSE)
  trainingData <- Data[dataIndex,]
  testData <- Data[-dataIndex,]
  return(list("training.data"=trainingData,"test.data"=testData))
}

print("Accuracy without Student")
data1 <- splitData(Default,0.70)
acc1 <- LogisticRegression(data1$training.data,data1$test.data)
print(acc1)


data2 <- splitData(Default,0.60)
acc2 <- LogisticRegression(data2$training.data,data2$test.data)
print(acc2)

data3 <- splitData(Default,0.50)
acc3 <- LogisticRegression(data3$training.data,data3$test.data)
print(acc3)

print("Accuracy with Student")
data4 <- splitData(Default,0.60)
acc4 <- LogisticRegression(data4$training.data,data4$test.data,TRUE)
print(acc4)