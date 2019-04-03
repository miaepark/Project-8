---
title: "Course8"
author: "Miae Park"
date: "April 1, 2019"
output: html_document
---
# Download Data
```{r}
training <- read.csv("~/Desktop/training.csv")
testing <- read.csv("~/Desktop/testing.csv")
```

# Cross Validation
Cross validation will be performed by subsampling our training dataset randomly without replacement into 2 subsamples: TrainTrainingSet data (75% of the original training dataset) and TestTrainingSet data (25% of the original training dataset). Our models will be fitted on the TrainTrainingSet, and tested on the TestTrainingSet. Once the most accurate model is choosen, it will be tested on the original test dataset. 

# Expected out-of-sample error
The expected out-of-sample error will corresond to 1 - accuracy in the cross validation data. Accuracy is the proportion of correct classified observation over the total sample in the TestTrainingSet dataset. Expected accuracy is the expected accracy in the out-of-sample dataset, which is the origianl testing dataset. Therefore, the expected value of the out-of-sample error will correspond to the expected number of missclassified observations/total observations in the Test dataset, which is the quantity: 1-accuracy found from the cross validation dataset. 

Our outcome variable is a factor variable. We split the training dataset into TrainTrainingSet and TestTrainingSet dataset.

```{r}
# Basic Explanatory Analysis
set.seed(1234)
dim(training)
dim(testing)
summary(training)
summary(testing)
str(training)
str(testing)

# Delete columns with all missing values 
training <- training[,colSums(is.na(training)) == 0]
testing <- testing[,colSums(is.na(testing)) == 0]

# Delete irrelevant variables to our current project
training <- training[,-c(2:7)]
testing <- testing[,-c(2:7)]

# Partition the data so that 75% of the training dataset into training and the remaining 25% to testing 
library(caret)
traintrainset <- createDataPartition(y=training$classe, p=0.75, list=FALSE)
TrainTrainingSet <- training[traintrainset,]
TestTrainingSet <- training[-traintrainset,]

# Plot of the outcome variable to see the frequency of each level in the TrainTrainingSet. 
plot(TrainTrainingSet$classe, main="Plot of levels of variable classe within the TrainTrainingSet", xlab="classe", ylab="Frequency")
```
# Prediction model 1: Decision Tree 
```{r}
library(rpart)
model1 <- rpart(classe ~ ., data=TrainTrainingSet, method="class")

prediction1 <- predict(model1, TestTrainingSet, type="class")

# Plot the decision tree
rpart.plot(model1, main="Classification Tree", extra=102, under=TRUE, faclen=0)

# Test results on our TestTrainingSet 
library(caret)
confusionMatrix(prediction1, TestTrainingSet$classe)
```

# Prediction model 2: Random Forest 
```{r}
library(randomForest)
model2 <- randomForest(classe~., data=TrainTrainingSet, method="class")

prediction2 <- predict(model2, TestTrainingSet, type="class")

confusionMatrix(prediction2, TestTrainingSet$classe)
```

# Decision on which Prediction Model to Use 
Random Forest algorithm and Decision Trees performed the same. Both have a accracy of 1. As shown below, they both produce the same result. 
```{r}
predictionFinal1 <- predict(model1, testing, type="class")
predictionFinal1

predictionFinal2 <- predict(model2, testing, type="class")
predictionFinal2  

```
