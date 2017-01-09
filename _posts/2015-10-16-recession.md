---
layout: post
title:  "Forecasting Economic Recessions with LASSO (R Version)"
date:   2015-10-16 
---

I developed a machine learning model that predicts recessions out-of-sample with over 95% accuracy. 

The model uses LASSO estimation and relies on a panel of [transformed economic data]({{ site.url }}/assets/recTransformed.csv). Inputs here are proprietary blends of standard leading economic indicators based on some of my prior consulting work. Variables in the data set fall into nine categories capturing things like the housing market, credit spreads, the yield curve, etc. The outcome variable is a binary recession indicator, coded ex-post by a recession dating committee within the NBER. 

First, load the required libraries and data
{% highlight r %}

library(quantmod)
library(TTR)
library(zoo)
library(lubridate)
library(glmnet)

p <- read.csv("recTransformed.csv", header=T)
p$X <- as.Date(p$X, format = "%m/%d/%y")
p <- zoo(p[,-1], order.by = p$X)
names(p) <- tolower(gsub(".Index", "", names(p)))

{% endhighlight %}

<!--more-->

Next, estimate the LASSO from the input data with `CalcLasso()`

{% highlight r %}
CalcLasso <- function(p, plot = F, return.cvobj = T){

	########
	# Arguments:
	#   p = input data generated from GetInputs()
	#   plot = if T, output plot of lambda vs misclassification error
	#  return.cvobj = if T return base cv.glmnet object; o.w. return res
	# Returns: one of:
	#   cvobj = cv.glmnet object
	#   res = list() containing model lambda, coefficients, and fitted values
	########

	res <- NULL
	x <- model.matrix(usrindex ~ . , data = p)[,-1]
	y <- p$usrindex
	cv.glmod <- cv.glmnet(x, y, alpha = 1, family = "binomial", type.measure = "class")
	fit.glmod <- glmnet(x, y, alpha = 1, family = "binomial")

	if(plot){
		pdf("gvmod.pdf")
		plot(cv.glmod)
		dev.off()
	}

	res$lambda <- cv.glmod$lambda.min
	res$coefs <- coef(cv.glmod, s = "lambda.min")

	# Get Lasso predictions
	pred <- as.data.frame( predict(cv.glmod, newx = x, s = "lambda.min", type = "response") )
	names(pred) <- c("fitted.value")
	pred$dts <- as.Date(rownames(pred), format = "%Y-%m-%d")
	pred <- zoo(pred$fitted.value, pred$dts)
	index(pred) <- index(pred) + months(1) - 1
	res$pred <- pred

	if(! return.cvobj){
		return(res)
	}

	if(return.cvobj){
		return(cv.glmod)
	}

}

{% endhighlight %}

Then we can train the LASSO on data from 1980 to 2005, and fit it out of sample to data from 2005 on:

{% highlight r %}


# Training set from 1980 to 2005
p.train <- p[1:300, ]

# Test set from 2005 to Dec 2015
p.test <- p[301:nrow(p), ]

# Train the model; generate in-sample predictions
cv.glmod <- CalcLasso(p.train, plot = F, return.cvobj = T)
x.in<- model.matrix(usrindex ~ . , data = p.train)[,-1]
pred.in <- predict(cv.glmod, newx = x.in, s = "lambda.min", type = "response")
pred.in <- zoo(pred.in[,1], as.Date(rownames(pred.in), "%Y-%m-%d") )

# Fit the model on the test set; generate out-of-sample predictions
x.out <- model.matrix(usrindex ~ . , data = p.test)[,-1]
pred.out <- predict(cv.glmod, newx = x.out, s = "lambda.min", type = "response")
pred.out <- zoo(pred.out[,1], as.Date(rownames(pred.out), "%Y-%m-%d") )

{% endhighlight %}

Plot the in-sample predicted probability of recession in blue, and the out-of-sample predictions in red.

 {% marginfigure "fig1"  'assets/img/predict200.png' 'Predicted Probability of Recession in the US' %} 

{% highlight r %}
x <- pred.in
plot(x, xlab = "", ylab = "", col = "blue", lwd = 3, type = "n",
    main = "", xlim = c(min(index(pred.in)), max(index(pred.out))))
  rect(as.Date("1980-01-01"), min(x, na.rm=T)-0.1,
    as.Date("1980-07-01"), max(x, na.rm=T) + 0.1, col = "lightgrey")
  rect(as.Date("1981-07-01"), min(x, na.rm=T)-0.1,
    as.Date("1982-11-01"), max(x, na.rm=T) + 0.1, col = "lightgrey")
  rect(as.Date("1990-07-01"), min(x, na.rm=T)-0.1,
    as.Date("1991-03-01"), max(x, na.rm=T) + 0.1, col = "lightgrey")
  rect(as.Date("2001-03-01"), min(x, na.rm=T)-0.1,
    as.Date("2001-11-01"), max(x, na.rm=T) + 0.1, col = "lightgrey")
  rect(as.Date("2007-12-01"), min(x, na.rm=T)-0.1,
    as.Date("2009-06-01"), max(x, na.rm=T) + 0.1, col = "lightgrey")

  lines(pred.in, col = "blue", type ="l", lwd = 3)
  lines(pred.out, col = "red", type ="l", lwd = 3)

{% endhighlight %}

The out-of-sample predictions look excellent, but We can also zoom into them even further. First, restrict the data to out-of-sample predictions, and classify them as (in)correct recession forecasts based on a threshold value of 0.5

{% highlight r %}
usr <- zoo(p$usrindex)
df <- merge(pred.out, usr)
df <- df[complete.cases(df), ]
df[df$pred.out < 0.5 & df$usr == 1 | df$pred.out > 0.5 & df$usr == 0, ]
{% endhighlight %}

Then, plot correct predictions in green and incorrect predictions in red. Note that our model has around a one-quarter lag at forecasting recessions in real-time.

{% marginfigure "fig2" "assets/img/error200.png" "A close-up: where did the LASSO get it wrong?" %}


{% highlight r %}
dtMin <- as.Date("2007-10-01", format = "%Y-%m-%d")
dtMax <- as.Date("2009-12-01", format = "%Y-%m-%d")
seq <- seq(dtMin, dtMax, "month")[seq(1, 27, 3)]
lab <- c("2007Q4", "2008Q1", "2008Q2", "2008Q3", "2008Q4", "2009Q1", "2009Q2", "2009Q3", "2009Q4")
plot(df$pred.out, xlab = "", ylab = "", col = "blue", lwd = 3, type = "n",
      xlim = c(dtMin, dtMax), xaxt = "n")
      axis(1, at = seq, lab = lab )
rect(as.Date("2007-12-01"), min(df$pred.out, na.rm=T)-0.1,
    as.Date("2009-06-01"), max(df$pred.out, na.rm=T) + 0.1, col = "lightgrey")
abline(h  = 0.5, lty = 2)
idx <- which( (df$pred.out < 0.5 & df$usr == 1) | (df$pred.out > 0.5 & df$usr == 0) )
points(df$pre[-idx], col = "darkgreen", lwd = 3, pch = 19)
points(df$pred[idx], col = "red", pch = 19)
{% endhighlight %}

