---
output: html_document
---
title:"Exponentials"
author: "issa fall"
date: "September 7, 2015"
keep_md:true
```



          DISTRIBUTION OF THE MEANS OF 40 EXPONENTIALS
          
INTRODUCTION

We investigate the distribution of exponentials in R and compare it with the central limit
theorem.
The mean is 1/lambda
The standard deviation is also 1/lambda set to 0.2 for all the simulations.
It investigates the distribution of average of 40 exponentials.

##                                           SIMULATIONS
##Lambda

```r
lambda =0.2
```
##Number of exponentials

```r
n =40
```
##Number of simulations

```r
nsims =1:1000
```
##Set the seed

```r
set.seed(820)
means<-data.frame(x =sapply(nsims, function(x){
                      mean(rexp(n,lambda))}))
head(means)
```

```
##          x
## 1 5.750000
## 2 3.808205
## 3 4.058154
## 4 3.999241
## 5 4.312532
## 6 4.418246
```
##The sample mean

```r
mean(means$x)
```

```
## [1] 4.998812
```
##Confidence intervals

```r
quantile(mean(means$x), c(0.025, 0.975))
```

```
##     2.5%    97.5% 
## 4.998812 4.998812
```
##The theoretical mean

```r
1/lambda
```

```
## [1] 5
```
##CONCLUSION

The sample mean and the theoretical mean are almost the same.

##The sample variance

```r
var(means$x)
```

```
## [1] 0.6255895
```
##Confidence interval

```r
quantile(var(means$x), c(0.025, 0.975))
```

```
##      2.5%     97.5% 
## 0.6255895 0.6255895
```
##Theoretical variance

```r
((1/lambda)/sqrt(40))^2
```

```
## [1] 0.625
```
The variance of the sample and the theoretical variance are almost the same.
Let's show that the distribution is approimately normal.
##Let's upload ggplot2

```r
library(ggplot2)
ggplot(data = means,aes(x = means))+geom_histogram(color ="black",fill ="lightblue",binwidth = 0.05)
```

```
## Don't know how to automatically pick scale for object of type data.frame. Defaulting to continuous
```

![plot of chunk histogram](figure/histogram-1.png) 


##Loading Tooth Growth

```r
library(datasets)
data("ToothGrowth")
names(ToothGrowth)
```

```
## [1] "len"  "supp" "dose"
```

```r
head(ToothGrowth)
```

```
##    len supp dose
## 1  4.2   VC  0.5
## 2 11.5   VC  0.5
## 3  7.3   VC  0.5
## 4  5.8   VC  0.5
## 5  6.4   VC  0.5
## 6 10.0   VC  0.5
```

```r
tail(ToothGrowth)
```

```
##     len supp dose
## 55 24.8   OJ    2
## 56 30.9   OJ    2
## 57 26.4   OJ    2
## 58 27.3   OJ    2
## 59 29.4   OJ    2
## 60 23.0   OJ    2
```

```r
str(ToothGrowth)
```

```
## 'data.frame':	60 obs. of  3 variables:
##  $ len : num  4.2 11.5 7.3 5.8 6.4 10 11.2 11.2 5.2 7 ...
##  $ supp: Factor w/ 2 levels "OJ","VC": 2 2 2 2 2 2 2 2 2 2 ...
##  $ dose: num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
```

```r
summary(ToothGrowth)
```

```
##       len        supp         dose      
##  Min.   : 4.20   OJ:30   Min.   :0.500  
##  1st Qu.:13.07   VC:30   1st Qu.:0.500  
##  Median :19.25           Median :1.000  
##  Mean   :18.81           Mean   :1.167  
##  3rd Qu.:25.27           3rd Qu.:2.000  
##  Max.   :33.90           Max.   :2.000
```

```r
with(ToothGrowth,plot(dose,len))
with(subset(ToothGrowth, supp =="OJ"),points(dose,len, col ="blue"))
```

![plot of chunk loading](figure/loading-1.png) 

```r
boxplot(len ~ supp,ToothGrowth,xlab ="supp",ylab ="len", main = "ToothGrowth")
```

![plot of chunk loading](figure/loading-2.png) 

```r
fit<-lm(len ~., data = ToothGrowth)
summary(fit)$coef
```

```
##              Estimate Std. Error   t value     Pr(>|t|)
## (Intercept)  9.272500  1.2823649  7.230781 1.312335e-09
## suppVC      -3.700000  1.0936045 -3.383307 1.300662e-03
## dose         9.763571  0.8768343 11.135025 6.313519e-16
```

```r
table(ToothGrowth$supp)
```

```
## 
## OJ VC 
## 30 30
```

```r
oj<-subset(ToothGrowth, supp == "OJ")
head(oj)
```

```
##     len supp dose
## 31 15.2   OJ  0.5
## 32 21.5   OJ  0.5
## 33 17.6   OJ  0.5
## 34  9.7   OJ  0.5
## 35 14.5   OJ  0.5
## 36 10.0   OJ  0.5
```

```r
hist(oj$len, col = "red")
```

![plot of chunk loading](figure/loading-3.png) 

```r
vc<-subset(ToothGrowth, supp == "VC")
head(vc)
```

```
##    len supp dose
## 1  4.2   VC  0.5
## 2 11.5   VC  0.5
## 3  7.3   VC  0.5
## 4  5.8   VC  0.5
## 5  6.4   VC  0.5
## 6 10.0   VC  0.5
```

```r
hist(vc$len, col = "red")
```

![plot of chunk loading](figure/loading-4.png) 

```r
qq<-qplot(dose, len, colour = supp, data = ToothGrowth)
qq + geom_smooth(method = "lm", formula = y ~ x)
```

![plot of chunk loading](figure/loading-5.png) 


##Bootstrapping the statistics(dose) at a 95% CI

```r
library(boot)
```
##Function to obtain regression coef

```r
bs<-function(formula, data, indices){
  
  d<-data[indices, ] # allow boot to select sample
  
  fit<-lm(formula, data = d)
  return(coef(fit))
}
```
##Bootstrapping with 1000 replications

```r
results<-boot(data = ToothGrowth, statistic = bs, R = 1000, formula = len ~.)
```
##View results

```r
plot(results, index =1) # intercept
```

![plot of chunk results](figure/results-1.png) 

```r
plot(results, index =2) #
```

![plot of chunk results](figure/results-2.png) 

```r
plot(results, index =3)
```

![plot of chunk results](figure/results-3.png) 


##Get the 95% CI

```r
boot.ci(results, type = "bca", index = 1)
```

```
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 1000 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = results, type = "bca", index = 1)
## 
## Intervals : 
## Level       BCa          
## 95%   ( 6.681, 11.905 )  
## Calculations and Intervals on Original Scale
```

```r
boot.ci(results, type = "bca", index = 2)
```

```
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 1000 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = results, type = "bca", index = 2)
## 
## Intervals : 
## Level       BCa          
## 95%   (-5.829, -1.609 )  
## Calculations and Intervals on Original Scale
```

```r
boot.ci(results, type = "bca", index = 3)
```

```
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 1000 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = results, type = "bca", index = 3)
## 
## Intervals : 
## Level       BCa          
## 95%   ( 8.145, 11.515 )  
## Calculations and Intervals on Original Scale
```
