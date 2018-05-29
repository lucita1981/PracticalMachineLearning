title	author	date	output
Practical Machine Learning - Prediction Assignment Writeup
Lucia Martinez
28/5/2018
html_document
##Executive Summary In this course project we attempt to build a machine learning model capable of predicting which of 5 possible ways a person was performing a dumbell curl exercise using the data took of devices such as Jawbone Up, Nike FuelBand, and Fitbit. Once the best model based on the accuracy measurement is selected, its performance is confirmed using a validation set. The conclusion is a random forest was able to generate better predictions with 99.64% of accuracy.

##Libraries

library(caret)
library(randomForest)
library(corrplot)
library(ggplot2)
set.seed(12345)
##Data Loading, Cleaning & Exploratory Analysis First, we load the datasets and the training dataset is partinioned in 2 to create a training set for the modeling process and a test set for the predicting process. The original testing dataset (from now on validation set in order to prevent confusion) is not changed and will only be used for the quiz results generation.

urlTraining <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
urlValidation <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
Training <- read.csv(url(urlTraining))
Validation  <- read.csv(url(urlValidation))
inTrain  <- createDataPartition(Training$classe, p=0.7, list=FALSE)
TrainingSet <- Training[inTrain, ]
TestSet  <- Training[-inTrain, ]
dim(TrainingSet)
dim(TestSet)
Now, in the cleasing process we went to remove that variables with NA. Also, the Near Zero variance (NZV) variables are also removed and the ID variables as well.

remNA <- sapply(TrainingSet, function(x) mean(is.na(x))) > 0.95
TrainingSet <- TrainingSet[, remNA==FALSE]
TestSet  <- TestSet[, remNA==FALSE]
NZV <- nearZeroVar(TrainingSet)
TrainingSet <- TrainingSet[, -NZV]
TestSet  <- TestSet[, -NZV]
TrainingSet <- TrainingSet[, -(1:5)]
TestSet  <- TestSet[, -(1:5)]
dim(TrainingSet)
dim(TestSet)
Finally, in this step we analysed the correlation among variables. The highly correlated variables are shown in dark colors in the graph below. (For visual understanding, only 20 variables will be plotted.)

corvar <- cor(TrainingSet[,-54])
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
                           "cyan", "#007FFF", "blue", "#00007F"))
corrplot(corvar[1:20,1:20], type = "lower" ,order = "hclust", col= col4(10))
qplot(roll_belt, total_accel_belt, data=TrainingSet, color=classe)
Both graph suggests that it is a good idea to use methods like Random Forest.

##Prediction and Modelling We used two methods will be applied to model the regressions: Random Forests and Decision Tree, because as result of the exploratory analysis seems the methods that can work well. Only, the method with higher accuracy (via Confusion Matrix visualizes the accuracy of the models) will be used for the quiz predictions.

####a. Random Forest The accuracy of the model is 99.64% and the estimated out-of-sample error is 0.36%

controlRF <- trainControl(method="cv", 3)
fitRF <- train(classe ~ ., data= TrainingSet, method="rf", trControl=controlRF)
fitRF
predictRF <- predict(fitRF, newdata=TestSet)
confusionMatrix(TestSet$classe, predictRF)
####b. Decision Tree The accuracy of the model is 52.37% which is pretty bad! Note that the decision tree never predict a D classe.

fitDT <- train(classe ~ ., data=TrainingSet, method="rpart", trControl=controlRF)
predictDT <- predict(fitDT, newdata=TestSet)
confusionMatrix(TestSet$classe,predictDT)
##Conclusion & Predicting Results on the Test Data The random forest is well suited to generating accurate predictions for this dataset. While a decision tree wasn't able to accurately predict the response.

The Random Forest model will be applied to predict the 20 quiz results (validation dataset) as shown below.

validationTest <- predict(fitRF, newdata=Validation)
validationTest
