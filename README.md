# PackageRegLog

The package "PackageRegLog" is used to fit a logistic regression model with a stochastic gradient descent (SGD). It offers a function "fit" to ajust the model on data and gives coefficients and a function "predict" to predict the value of y with coefficients. 

## Installing the package 

```
devtools::install_github("inesk99/projet_R")
```

## Loading the package 

After installation, you have to load the package to use it. 

```
library(PackageRegLog)
```

## Datasets import 

First, you need to import a dataset. It can include numerical, categorial variables or both. We choose the one you want. In this example, the dataset "Breast Cancer" will be used. This dataset is included 

```
cancer = PackageRegLog::cancer
```

We can open the dataframe "cancer" to see the differents variables. The dataset contains informations on women who have breast tumors and the variable "diagnosis" includes two modalities : M (malignant) or B (benign). It contains 569 observations and 33 variables : 30 of type "numeric", 1 of type "integer" : "ID" , 1 of type "factor" : "diagnosis" and 1 of type "logical" : "X". 

## Splitting the dataset

After data import, it's useful to split the data set into two samples : train and test. The training data set represents 70% of the data and the test data set 30%. 
You have to install the package "caret" and load it. 

```
install.packages("caret")
library(caret)
```

Then, you split the cancer data set with the command "createDataPartition". 

```
index = createDataPartition(cancer$diagnosis,p=0.70,list = FALSE)
train = cancer[index,]
test = cancer[-index,]
```

The training data has 399 rows and 170 rows for the testing data. 

## Tutorial for using the package

Now, you can use our package. 
"
### Fit a SGD logistic regression 

For that, we can use the function "fit". If you want to see the seeting of this function, you can run __help(fit)__.   
*fit(formula, data, epsilon, learning_rate, max_iter, batch_size, ncores)*

You apply the fit on the training data. 

```
a = fit(diagnosis~.,data=train,epsilon = 1e-04,learning_rate = 0.01,max_iter = 1000,batch_size = nrow(cancer),ncores=0)
```

To display coefficients, you can use :

```
a$coef
```

Or, to display more informations, you can use "print" and "summary". The functions have been overloaded.

```
print(a)
summary(a)
```

Now that we have coefficients, we can use our prediction function. 

### Prediction of target variable y

You will use the function "predict". If you want to see the seeting of this function, you can run __help(predict)__. 
*predict(objet, newdata, type)*

As we are in the case of supervised learning, you must remove the target variable y from our testing data : "diagnosis". 

```
y_test = test$diagnosis
X_test = test[,-2]
```
You apply the function predict on the X_test. You use the object a and the type is the class of affiliation (0 ou 1).

```
y_pred = predict(a,newdata = X_test,type = "class")
```

You obtain a prediction vector.

### Performance evaluation

To explore further, you can compare the actual data and the predictions made. You will make a confusion matrix. 
As the target variable y "diagnosis" is qualitative, it must be coded in 0 and 1. The package used is "fastDummies".

```
install.packages("fastDummies")
library(fastDummies)
y_test  = dummy_columns(y_test,remove_first_dummy = TRUE,remove_selected_columns = TRUE)
```
Then, you display the confusion matrix between the predictions and the actual data. 

```
p =cbind("Y_test"= y_test,"y_pred"= y_pred)
p = as.data.frame(p)
confusion_matrix = table(p$y_pred,p$.data_M) ; confusion_matrix
```

You can calculate the accuracy and the prediction error rate. 

```
accuracy=(confusion_matrix[1,1]+confusion_matrix[2,2])/sum(confusion_matrix) ; accuracy
error_rate = 1-accuracy ; error_rate
```
















