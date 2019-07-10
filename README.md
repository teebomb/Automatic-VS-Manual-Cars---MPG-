# Automatic-VS-Manual-Cars---MPG-
---
title: "MPG Analysis - AUTO VS MANUAL - CRS7 PROJ"
author: "Tommy Brant"
date: "February 4, 2019"
output: html_document
---




##EXECUTIVE SUMMARY
The intent of this analysis is to explore and evaluate the relationship between transmission and miles per gallon(MPG) in the mtcars data set. Firstly, we work to identify a good model that includes transmission type, and infer from said model to determine if transmission type is an appropriate variable to predict miles per gallon. This will include different modeling techniques, such as Linear Regression Models and Generalize Linear Regression Models. With these models, we determine that transmission type is not a good variable to determine MPG, nor can the difference in MPG between transmission types be quantified solely based on transmission type.



##LOAD DATA and EXPLORATORY ANALYSIS
Since the mtcars data set is already part of R, loading the data is simple:
```{r} 
library(ggplot2)
library(GGally)
data(mtcars)
```


Exploratory Analysis

1. Look at data frame variables, and linear model factoring in all variables
```{r echo=FALSE}
names(mtcars)
#?mtcars # DETERMINE WHICH VARIABLE AND HOW TO INTERPRET
#SHOWS THAT "am"" is the variable name we want to use as the regressor variable, and the outcome variable is "mpg"
fit0 <- lm(mpg ~ . ,data=mtcars)
summary(fit0)
```

With linear regression modeling, including more predictors in the same model will yield different results on the outcome. This is explained by Simpson's Paradox. As we narrow in on some variables, we will discover different relationships. This exploratory analysis gives us information relative to other predictors. We can look at variables that have low P values relative to others. For now, make note of the transmission type(am), number of cylinders(cyl), with respect to miles per gallon(mpg)



#Model Creation
The following models are used in this analysis 

1. Linear Regression Model

2. GLM  

##Linear Regression Model 
Using the Linear Regressions Models, let us see what kind of relationship `r 'am'` and `r 'mpg'` have.
```{r}
fit1 <- lm(mpg ~ as.factor(am) ,data=mtcars)
coef(fit1)
#P value < 0.05, so am is an appropriate interaction term.
summary(fit1)
```
The intercept is `r coef(fit1)[1]`. This is the the baseline mpg. 
The slope is `r coef(fit1)[2]`. This suggests that there is a 7 mpg increase when going from automatic to manual transmission.

The P value for am less than 0.05, which suggests am is a siginificant predictor. However, the low R-squared value, `r summary(fit1)$r.squared` is a cause for concern. This is an indicator of poor model fit.
We will proceed to investigate the residual plot.



See Appendix A, Figure 1 for the residual plot for this model.



```{r fig1, echo=FALSE, eval=FALSE}
#FIGURE 1 - RESIDUAL PLOT AM VS MPG
x1<-mtcars$am
y1<-mtcars$mpg
n1<-length(mtcars$mpg)
e1<-resid(fit1)
plot(x1, e1, xlab="Transmission Type", ylab="Residuals (in MPG)", bg="lightblue", col="black", cex=2, pch=21, main="Residual Plot")
abline(h=0,lwd=2)
for (i in 1:n1)
 lines(c(x1[i], x1[i]), c(e1[i],0), col="red", lwd=2)
```


Note the gap between the predicted values and the actual results. We will compare this to our next model, as this one is a poor fit.


Let's try looking at a model with an added variable for comparison. We see from our exploratory analysis that the number of cylinders appears to be a more significant variable relative to others. `r 'cyl'` will be included in our next model.

```{r}
fit2 <- lm(mpg ~ as.factor(am) + cyl ,data=mtcars)
coef(fit2)
summary(fit2)
```

For this model, note the R squared value, `r summary(fit2)$r.squared` is substantially higher than model fit1. Although we would like R squared to to be higher, we will conclude this model is sufficent for progressing our analysis.



See Appendix A, Figure 2 for the residual plot for this model. 



```{r fig2, echo=FALSE, eval=FALSE}
##FIGURE 2 - RESIDUAL PLOT FOR AM + CYL VS MPG
x2<-mtcars$am
y2<-mtcars$mpg
n2<-length(mtcars$mpg)
e2<-resid(fit2)
plot(x2, e2, xlab="Transmission Type", ylab="Residuals (in MPG)", bg="lightblue", col="black", cex=2, pch=21, main="Residual Plot")
abline(h=0,lwd=2)
for (i in 1:n2)
 lines(c(x2[i], x2[i]), c(e2[i],0), col="red", lwd=2)
```



In comparing Figure 1 to Figure 2, we can see that the difference in fitted values to actual values is much less in this model. 

Since we've found our model of choice, let's determine if the transmission type is a good predictor for miles per gallon. We can do this by looking at the P value. 

If the number of cylinders, `r 'cyl'`, is held constant, P value for `r 'am'` is greater than 0.05, which suggests that `r 'am'` is not a significant interaction term to influence the outcome miles per gallon. 


##Generalized Linear Models
Using Generalized Linear Models, let's see use the same models and predictors as our previous linear regression models.
 


```{r warning=FALSE} 
glm1 <- glm(mpg ~ as.factor(am), family="gaussian", data=mtcars)
summary(glm1)
glm2 <- glm(mpg ~ as.factor(am) + cyl, family="gaussian", data=mtcars)
summary(glm2)
```

Although with GLM we don't have an absolute model indicator like R squared to help determine if this is good or bad model fit, we can look at the AIC rating between the two models. 
AIC for glm1 is `r summary(glm1)$aic`.
AIC for glm2 is `r summary(glm2)$aic`.
AIC rating of the first model, glm1, is higher than the AIC rating of the second model, glm2. This indicates that glm2 has better model fit. To confirm this, let's look at the residual plots of glm1 and glm2, and see which one has the lowest differences in actual and fitted values.


See Appendix A, Figure 3 and Figure 4, respectively, for the residual plot for this model. 

```{r fig3, echo=FALSE, eval=FALSE}
##FIGURE 3 - RESIDUAL PLOT FOR AM VS MPG
x3<-mtcars$am
y3<-mtcars$mpg
n3<-length(mtcars$mpg)
e3<-resid(glm1)
plot(x3, e3, xlab="Transmission Type", ylab="Residuals (in MPG)", bg="lightblue", col="black", cex=2, pch=21, main="Residual Plot")
abline(h=0,lwd=2)
for (i in 1:n1)
 lines(c(x3[i], x3[i]), c(e3[i],0), col="red", lwd=2)
```



```{r fig4, echo=FALSE, eval=FALSE}
##FIGURE 4 - RESIDUAL PLOT FOR AM + CYL VS MPG
x4<-mtcars$am
y4<-mtcars$mpg
n4<-length(mtcars$mpg)
e4<-resid(glm2)
plot(x4, e4, xlab="Transmission Type", ylab="Residuals (in MPG)", bg="lightblue", col="black", cex=2, pch=21, main="Residual Plot")
abline(h=0,lwd=2)
for (i in 1:n1)
 lines(c(x4[i], x4[i]), c(e4[i],0), col="red", lwd=2)
```



In comparing Figure 3 to Figure 4, we can see that the difference in fitted values to actual values is much less in glm2.  This is also an indicator that glm2 is a better model than glm1.

Since we've found our model of choice, let's determine if the transmission type is a good predictor for miles per gallon. We can do this by looking at the P value. 

If the number of cylinders, `r 'cyl'`, is held constant, P value for `r 'am'` is greater than 0.05, which suggests that `r 'am'` is not a significant interaction term to influence the outcome `r 'mpg'`. this is consistent with our findings from linear regression modeling that `r 'am'` is not a significant variable to predict miles per gallon.


##CONCLUSION
When we tried using a Linear Regression model using transmission type, `r 'am'`, solely as a predictor, there were multiple indications it was a poor model. When we did identify a good model that included `r 'am'`, we find that it wasn't a significant variable to be used as a predictor.
In generalized linear modeling, we identified a good model that included `r 'am'`, we find again that it wasn't a significant varaible to be used as a predictor.
In conclusion, transmission type is not a good variable to determine MPG, nor can the difference in MPG between transmission types be quantified solely based on transmission type.



##ASSUMPTIONS USED
Assumptions and uncertainties are list below:Assumptions

1. For a normal distribution, assume that all data is independent and identically distributed random variables.

2. For T confidence intervals, assume the subjects are relevant samples from a population of subjects

##APPENDIX A
```{r fig1.2, ref.label='fig1'}
```

Figure 1: Residual Plot for fit1

```{r fig2.2, ref.label='fig2'}
```

Figure 2: Residual Plot for fit2

```{r fig3.2, ref.label='fig3'}
```

Figure 3: Residual Plot for glm1

```{r fig4.2, ref.label='fig4'}
```

Figure 4: Residual Plot for glm2
