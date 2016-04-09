# ExporeOutliers
Outlier detection using R-Studio manipulate and ggplot2
The most common strategy for detecting outliers in a given empirical distribution 
is in terms of the “fences” or cutoff values that label the data points that lie outside as outliers. 
John Tukey in 1977 invented the box and whisker plot and chose 1.5*IQR as the upper cutoff value. 
Alternatively data points that lie above the 99% percentile might be called outliers.
This article presents concrete examples that show limitations of this approach. In particular, 
if an empirical distribution is close to a uniform distribution then from users perspective 
it's hard to call data points exceeding the 99% percentile outliers. 
Rather such points deserve to be called edge points or border points. 
Also if the number of data points increases so does the number of outliers 
and that may not seem quite right from the users perseption. 
To address this possible issue the Trimmed-LogN method is suggested.
The examples related to normal and log-normal distributions show 
that cutoff approach can be seen as too crude and needs to be adjusted. 
An adjustment method based on  Cramér–von Mises criterion and called the r-method is suggested. 
In a nutshell, the r-method computes the ratio between eucleadian norms 
of the empirical distribution quantiles, e. g. (25%, 50%, 75%, 95%, 99%) percentile vector 
and the corresponding uniform distribution percentile vector. 
R-coeficient then is calculated as a value between 0 and 1. 
This number measures the degree of difference between the empirical distribution and the corresponding uniform distribution. 
The closer the empirical distribution approximate the uniform one, the greater is the r-value. 
An adjusted cutoff percentile is calculated as (0.99 + r*0.01). 
Also it is shown how to calculate the p-Value that checks the NULL hypothesis.
All examples are given in R. Ggplot2 library was used for graphing and R-Studio matipulate for interactive GUI. 
I used set.seed(123456789) command that makes all the pictures fully reproducible.
