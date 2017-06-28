##About the Project

The main aim of this project is to quantify how well people perform certain exercise activities. We use machine learning to predict the manner in which they did exercise. Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. We use data from accelorometer on the belt, forearm, arm and dumbbell of 6 participants.

##Required R libraries

library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(knitr)
library(doMC)
##Getting and loading data

The data for this project was provided by http://groupware.les.inf.puc-rio.br/har.
trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

training <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
testing <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))
##Cleaning data

In order for the machine learning algorithms to work, we have to remove features which have null values. For this lets find null values percent in each column and we will subset the data.

naVal <- round(colMeans(is.na(training)), 2)
index <- which(naVal==0)[-1]
training <- training[, index]
testing <- testing[, index]
Also the 1st 6 columns are not very useful for our analysis. Also making all columns as numeric.

training <- training[, -(1:6)]
testing <- testing[, -(1:6)]
for(i in 1:(length(training)-1)){
    training[,i] <- as.numeric(training[,i])
    testing[,i] <- as.numeric(testing[,i])
}
##Cross validation (Partitioning data)

The training data is to be partioned into 2 parts: data to train a model and data to test the model. I prefer partitioning at 60% for training and remaining for testing.

inTrain <- createDataPartition(training$classe, p=0.6, list=FALSE)
myTraining <- training[inTrain, ]
myTesting <- training[-inTrain, ]
dim(myTraining); dim(myTesting)
##Machine Learning Models

Lets now build the machine learning models using two widely used algorithms.

###Random Forest

registerDoMC(cores = 8)
rfFit <- randomForest(classe~., data = myTraining , method ="rf", prox = TRUE)
rfFit
rfPred <- predict(rfFit, myTesting)
confusionMatrix(rfPred, myTesting$classe)
As seen from the summary above the accuracy is 99%.

Lets now use another algorithm and verify its accuracy before deciding the algorithm to use on test data.

###Generalized Boosted Regression Models

gbmFit <- train(classe~., data = myTraining, method ="gbm", verbose = FALSE)
gbmFit
gbmPred <- predict(gbmFit, myTesting)
confusionMatrix(gbmPred, myTesting$classe)
The accuracy of above algorithm is at 96%.

Let's plot the 2 models.

plot(rfFit)
plot(gbmFit)
We can apply the Random Forest model for the testing set.

predict(rfFit, testing)
