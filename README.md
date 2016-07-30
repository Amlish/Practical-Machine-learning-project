# Practical-Machine-learning-project

# Practical Machine Learning Project

### By Amlish
#### Date 07/30/2016


##Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).


### Some R packages 
```{r,echo=FALSE}
library(knitr)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
library(corrplot)
set.seed(777)
```

## Downloading and Cleaning the Data

```{r}
training<-read.csv("pml-training.csv", header=TRUE)
testing<-read.csv("pml-testing.csv", header=TRUE)
```



### Data partitionn into two sets
```{r}
set.seed(777)
intrain <- createDataPartition(training$classe, p=0.7, list=FALSE)
train1<-training[intrain,]
train2<- training[-intrain,]
dim(train1)
dim(train2)
```

The training data set is divided into two datasets, train1  data set and train2 dataset.
Both data subsets have 160 columns, that is number of variables.In the validation data set only the non zero values are needed.Thus we have to remove the NA.The near zero variance are also removed.

### Removing near zero variance 

```{r}
NZV <- nearZeroVar(train1)
train1<- train1[,-NZV]
train2<-train2[,-NZV]
dim(train1)
dim(train2)
```

### Removing all NAs and almost NAs

```{r}
set.seed(777)
AllNA <- sapply(train1, function(x) mean(is.na(x))) > 0.95
train1 <- train1[, AllNA==F]
train2 <- train2[, AllNA==F]
dim(train1)
dim(train2)
```

### Removing variables that do not make intuitive sense for prediction
```{r}
set.seed(777)
train1 <- train1[, -(1:5)]
train2 <- train2[, -(1:5)]
dim(train1)
dim(train2)
```

## Model Building and Prediction with Random Forest

```{r}
set.seed(777)
library(randomForest)
library(ggplot2)
fitControl <- trainControl(method="cv", number=3, verboseIter=F)
fit <- train(classe ~ ., data=train1, method="rf", trControl=fitControl)
fit$finalModel
```


```{r}
set.seed(123)
modFit1 <- randomForest(classe ~ ., data=train1)
prediction1 <- predict(modFit1, train2, type = "class")
cmrf <- confusionMatrix(prediction1, train2$classe)
cmrf
```

### Prediction with Decision Trees

```{r}
set.seed(123)
modFit2 <- rpart(classe ~ ., data=train1, method="class")
prediction2 <- predict(modFit2, train2, type = "class")
cmtree <- confusionMatrix(prediction2, train2$classe)
cmtree
```

###Prediction with Generalized Boosted Regression

 
```{r}
library(gbm)
set.seed(12345)
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 1)

GbmFit1 <- train(classe ~ ., data=train1, method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE)


GbmFinMod1 <- GbmFit1$finalModel

GbmPredTest <- predict(GbmFit1, newdata=train2)
GbmAccuracyTest <- confusionMatrix(GbmPredTest, train2$classe)
GbmAccuracyTest
```

From the three algorithms the random forest has the greatest accuracy which is 99.8%.That means,the random forset model is selected from the other two.Thus,the model fit on train is used to predict the label for the observations in testing data set.Then those predictions  are writen to individual files.

##Predicting Results on the Testing Data set

```{r}
prediction3<- predict(modFit1, testing, type = "class")
prediction3
```


## Writing the results to a text file for submission

```{r}
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=T)
    }
}
pml_write_files(prediction3)

```









