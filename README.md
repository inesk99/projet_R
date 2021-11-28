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

## Tutorial for using the package





