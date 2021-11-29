# PackageRegLog

The package "PackageRegLog" is used to fit a logistic regression model with a stochastic gradient descent (SGD). It offers a function "fit" to ajust the model on data and gives coefficients and a function "predict" to predict the value of y with coefficients. 

## Installing the package 

```
devtools::install_github("inesk99/PackageRegLog")
```

## Loading the package 

After installation, you have to load the package to use it. 

```
library(PackageRegLog)
```

## Datasets import 

First, you need to import a dataset. It can include numerical, categorial variables or both. We choose the one you want. In this example, the dataset "Breast Cancer" will be used. This dataset is included 

```
b_cancer = PackageRegLog::cancer
```

We can open the dataframe "b_cancer" to see the differents variables. The dataset contains informations on women who have breast tumors and the variable "diagnosis" includes two modalities : M (malignant) or B (benign). It contains 569 observations and 33 variables : 30 of type "numeric", 1 of type "integer" : "ID" , 1 of type "factor" : "diagnosis" and 1 of type "logical" : "X". 

## Splitting the dataset

After data import, 

## Tutorial for using the package

Now, you can use our package. 

### Fit a SGD logistic regression 

For that, we can use the function "fit". If you want to see the seeting of this function, you can run __help(fit)__.   
*fit(formula, data, epsilon, learning_rate, max_iter, batch_size, ncores)*

```
a = fit(diagnosis~.,data=cancer,epsilon = 1e-04,learning_rate = 0.01,max_iter = 1000,batch_size = nrow(cancer),ncores=0)
```
This 






