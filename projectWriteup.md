### Project: The prediction of exercise quality: how well did you exercise? 
### Catherine Li

### Overview
In this project, I used data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants, who were asked to perform barbell lifts correctly and incorrectly in 5 different ways. My primary goal is to predict their exercise quality, a categorical variable named "classe" in the dataset.

### Data Analysis

#### Step 1. load the data and replace all the missing values as NAs. 


```r
training<-read.csv("pml-training.csv", header=TRUE,sep=",", na.strings=c("", "NA", "#DIV/0!"))
testing<-read.csv("pml-testing.csv", header=TRUE,sep=",", na.strings=c("", "NA", "#DIV/0!"))
```

#### Step 2: Split the training dataset into two parts: a trainingSub dataset and a validtion dataset, for cross validation purpose. 


```r
library(caret)
inTrain<-createDataPartition(y=training$class, p=0.75,list=FALSE)
trainingSub<-training[inTrain,]
validation<-training[-inTrain,]
```

#### Step 3. Let's examine the trainingSub dataset. 

```r
beltVarN<-sum(grepl("_belt", names(trainingSub)))
armVarN<-sum(grepl("_arm", names(trainingSub)))
forearmVarN<-sum(grepl("_forearm", names(trainingSub)))
dumbbellVarN<-sum(grepl("_dumbbell", names(trainingSub)))
featureVarN<-sum(beltVarN, armVarN, forearmVarN, dumbbellVarN)
```

There are 160 variables in total. Among those, 152 variabls are feature variables: 38 variables are arm-relevant variables; 38 are belt-relevant variables; 38 are forarm-relevant variables; and 38 are dumbbell-relevant variables. The remaining 8 variables include our outcome "class" variable, the variable that we plan to predict, and the other 7 identification variables such as X (sequence), user_name (a participant's name), and five time variables. I intend to use feature variables only to predict outcome variable in this project. 

#### Step 4. clean up the trainingSub dataset.
In this step, I eliminated variables that have more than 97.5% of NA values. Please note 97.5% is subjective as I do not want to throw away a lot of data.  


```r
col <- colSums(is.na(trainingSub))/nrow(trainingSub)<=0.975
trainingSubNew <- trainingSub[,col]
trainingSubNew2<-trainingSubNew[,-c(1:7)]
```

I started with 160 variables in total for my trainingSub dataset. After running the above procedure, I ended up with 53 variables: 52 of them are feature variables, and "classe" is the outcome variable. The latest dataset name is labeled "trainingSubNew2".

#### Step 5. Fit the model on the trainingSubNew2 dataset. 


```r
set.seed(1)
library(randomForest)
modFit<-randomForest(classe~., data=trainingSubNew2)
modFit
```

```
## 
## Call:
##  randomForest(formula = classe ~ ., data = trainingSubNew2) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 7
## 
##         OOB estimate of  error rate: 0.42%
## Confusion matrix:
##      A    B    C    D    E  class.error
## A 4181    3    0    0    1 0.0009557945
## B   11 2831    6    0    0 0.0059691011
## C    0   11 2556    0    0 0.0042851578
## D    0    0   22 2387    3 0.0103648425
## E    0    0    2    3 2701 0.0018477458
```

```r
plot(modFit, log="y", main="OOB and Label Prediction Error of the Random Forest Model")
legend("topright", colnames(modFit$err.rate), fill=1:6)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

I used random forest method to predict the outcome "class" variable due to its high accuracy. It turns out the accuracy rate is indeed very high  

#### Step 6. Use the validation dataset to evaluate the model fit


```r
validationNew <- validation[, col]
validationNew2<-validationNew[,-c(1:7)]
confusionMatrix(validationNew2$classe,predict(modFit, validationNew2) )
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1395    0    0    0    0
##          B    2  946    1    0    0
##          C    0    7  848    0    0
##          D    0    0    7  796    1
##          E    0    0    4    1  896
## 
## Overall Statistics
##                                         
##                Accuracy : 0.9953        
##                  95% CI : (0.993, 0.997)
##     No Information Rate : 0.2849        
##     P-Value [Acc > NIR] : < 2.2e-16     
##                                         
##                   Kappa : 0.9941        
##  Mcnemar's Test P-Value : NA            
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9986   0.9927   0.9860   0.9987   0.9989
## Specificity            1.0000   0.9992   0.9983   0.9981   0.9988
## Pos Pred Value         1.0000   0.9968   0.9918   0.9900   0.9945
## Neg Pred Value         0.9994   0.9982   0.9970   0.9998   0.9998
## Prevalence             0.2849   0.1943   0.1754   0.1625   0.1829
## Detection Rate         0.2845   0.1929   0.1729   0.1623   0.1827
## Detection Prevalence   0.2845   0.1935   0.1743   0.1639   0.1837
## Balanced Accuracy      0.9993   0.9959   0.9922   0.9984   0.9988
```

The accuracy rate for my validation dataset is about 99.6%.

#### Step 7. Use the testing dataset to predict the classe value.

```r
testingNew<-testing[,col]
testingNew2<-testingNew[,-c(1:7)]
answers<-predict(modFit, testingNew2[-53])
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)
```

