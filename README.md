# Robust association network construction between any two omics data type, including fecal microbial MGSs



#### Description

This script is used to find associations between any two omics data, including clinical metadata and potentially high-dimensional omics data, by a cross validation function for random forest.



#### Usage

R  omics.R  omics_input1  omics_input2  out.prefix

* omics_input1

  It is required to be in csv format. Rows represent samples, and columns represent metadata or features.

  Here is an example:


|   | BMI | Age  | Hip_circumference |
| ------------- | ------------- | ------------- | ------------- |
| Sample1  | 20  | 26  | 83.2  |
| Sample2  | 19  | 30  | 90.2  |
| Sample3  | 27  | 32  | 82.5  |
| Sample4  | 18  | 28  | NA  |
  


* omics_input2

  The format of omics_input2 is the same as omics_input1.

* out.prefix

  Specify the prefix of output file.
  
  

#### Output files

The following files will be generated after the analysis.

* $out.prefix.imp_omics.csv
	
	This file shows the RFCV importance.
	
* $out.prefix.crsq_omics.csv
	The meaning of each column in this file is as followed:
	```
	"var" : Variable names
	"cut": Best number of predictors used in the model
	"sample": Shared sample numbers of target and predict dataset
	"n.var": Same as original rfcv, that is number of varialbes used in the model
	"error.cor": Spearman correlation between prediction and true measurments
	"error.nmse": Normalized MSE (NMSE), also known as Normalized Ensemble Variance (NEV)
	"error.r2": Same as original rfcv, that is rsqure.
	"error.mape": Mean Absolute Percentage Error (MAPE) 
	"error.mse": Mean Squared Error Mean Squared Error, or MSE for short
	"error.mae": Mean Absolute Error (MAE), which measures the average magnitude of the errors in a set of predictions, without considering their direction
	"error.MedianAE": Median absolute error
	"error.RAE": Relative Absolute Error (RAE)
	"error.RMSE": The Root Mean Squared Error, or RMSE
	```
	
* $out.prefix.pred_omics.csv

  Represents the result for RFCV prediction.

###### Notice
You should specify the path where input files(eg, omics_input2 and omics_input1)  locate in the script line 135 and line 141.

