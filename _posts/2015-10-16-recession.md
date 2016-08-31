---
layout: post
title:  "Forecasting Economic Recessions with LASSO (R Version)"
date:   2015-10-16 
---

I developed a machine learning model that predicts recessions out-of-sample with over 95% accuracy. You can find the white paper [here]({{ site.url }}/assets/Recession Prediction.pdf)

The model uses LASSO estimation and relies on [recession_data.csv]({{ site.url }}/assets/recession_data.csv)

You'll also need the following libraries:
{% highlight r %}
library(quantmod)
 library(TTR)
 library(zoo)
 library(lubridate)
 library(glmnet)
{% endhighlight %}

<!--more-->

Load the data and transform the base variables in a variety of ways. We can wrap this in a `GetInputs()` function:
{% highlight r %}
GetInputs <- function(){

	# Load raw data and convert to zoo
	p <- read.csv("recession_data.csv", header=T)
	p$X <- as.Date(p$X, format = "%m/%d/%y")
	p <- zoo(p[,-1], order.by = p$X)
	names(p) <- tolower(gsub(".Index", "", names(p)))

	# Carry forward quarterly observations
	p$phffanx0 <- c(rep(NA, 2), na.locf(p$phffanx0))
	p$gdp.cyoy <- c(rep(NA, 2), na.locf(p$gdp.cyoy))
	p <- p[year(index(p)) >= 1978 & year(index(p)) < 2016,]

	################################
	
	# Consumer Expectations:
	p$conssent.1 <- SMA(p$conssent, 3)/Lag(SMA(p$conssent, 3), 6) - 1 # 6m change in 3mma
	p$conssent.2 <- runMin(p$conssent/runMax(p$conssent, 12),9) # 9m min of current / annual max
	p$conssent.3 <- runSum(0.5* sign( diff(SMA(p$lei.cexp, 2)) ) + 0.5, 8)

	# Corporate Credit
	p$cred.1 <- SMA(p$moodcavg, 2)/Lag(SMA(p$moodcavg, 2), 6) - 1
	p$cred.2 <- SMA(p$moodcbaa - p$usgg10yr, 4)
	p$cred.3 <- (p$moodcbaa - p$moodcaaa)/(Lag(p$moodcbaa,6) - Lag(p$moodcaaa, 6)) - 1    
	p$cred.4 <- runSum(0.5* sign( diff(SMA(p$moodcavg, 2)) ) + 0.5, 8)

	# Broad Indices
	p$indices.1 <- p$ads.bci
	p$indices.2 <- p$cfnai
	p$indices.3 <- p$coi.totl / Lag(p$coi.totl, 6) - 1
	p$indices.4 <- p$lei.lci
	p$indices.5 <- SMA(p$oustdiff, 3)
	p$indices.6 <- p$phffanx0
	p$indices.7 <- p$s5finl / runMax(p$s5finl, 14)
	p$indices.8 <- runSum(0.5* sign( diff(SMA(p$lei.yoy, 2)) ) + 0.5, 8)

	# Housing
	p$housing.1 <- SMA(p$lei.bp, 3) / Lag(SMA(p$lei.bp, 3), 12) - 1
	p$housing.2 <- p$nhslnfs / Lag(p$nhslnfs, 12) - 1
	p$housing.3 <- SMA(p$nhsltot, 3)
	p$housing.4 <- SMA(p$nmcmfus - p$usgg10yr, 4)

	# Manufacturing
	p$manu.1 <- p$napmnewo / p$napminv
	p$manu.2 <- SMA(p$napmpmi, 2)
	p$manu.3 <- p$crb.rind / runMax(p$crb.rind, 12)
	p$manu.4 <- SMA( p$ecrsuscp / Lag(p$ecrsuscp, 6) - 1, 4)
	p$manu.5 <- SMA( p$ipvptrmh / Lag(p$ipvptrmh, 12) - 1, 3)
	p$manu.6 <- runSum(0.5* sign( diff(SMA(p$lei.nwcn, 2)) ) + 0.5, 8)

	# Macro
	p$macro.1 <- p$uscrwtic / Lag(p$uscrwtic, 12) - 1
	p$macro.2 <- SMA(p$dxy, 4)
	p$macro.3 <- p$gdp.cyoy
	p$macro.4 <- p$m1..yoy - p$cpi.yoy
	#p$macro.5 <- runSum(0.5* sign( diff(SMA(p$lei.m2, 2)) ) + 0.5, 8)  

	# Equities
	p$equities.1 <- p$spx/Lag(p$spx, 6) - 1
	p$equities.2 <- runSum(0.5* sign( diff(SMA(p$lei.stkp, 2)) ) + 0.5, 8)  # before: 6-month average of 1-month changes
	p$equities.3 <- SMA( (p$spx/Lag(p$spx,6) - 1) / (p$usgg10yr/Lag(p$usgg10yr,6) - 1)     ,4)
	p$equities.4 <- p$tran / runMax(p$tran, 12)

	# Employment
	p$emp.1 <- SMA(p$injcjc4, 2) / Lag(SMA(p$injcjc4, 2),6) - 1
	p$emp.2 <- p$injcjc4 / Lag(p$injcjc4, 12) - 1
	p$emp.3 <- p$injcjc4 / runMin(p$injcjc4, 12)
	p$emp.4 <- runSum(0.5* sign( diff(SMA(p$lei.avgw, 2)) ) + 0.5, 8) # before: 6-month average of 1-month changes
	p$emp.5 <- SMA(p$lei.wkij, 2)
	p$emp.6 <- SMA(p$nfp.t, 3) / Lag( SMA(p$nfp.t, 3), 12) - 1
	p$emp.7 <- 50 - p$oust.neg
	p$emp.8 <- p$usestemp / Lag(p$usestemp, 12) - 1

	# Yield Curve
	p$yc.1 <- (p$dljhytw - p$usgg10yr) / runMax(p$dljhytw - p$usgg10yr, 12)
	p$yc.2 <- runSum(0.5* sign( diff(SMA(p$lei.irte, 2)) ) + 0.5, 8)
	p$yc.3 <- p$usgg10yr / runMax(p$usgg10yr, 12)
	p$yc.4 <- (p$usgg10yr - p$usgg5yr) / Lag(p$usgg10yr - p$usgg5yr, 6) - 1
	p$yc.5 <- SMA(p$usgg10yr - p$usgg5yr, 2)
	p$yc.6 <- (p$usgg10yr - p$usgg3m) / Lag(p$usgg10yr - p$usgg3m, 6) - 1
	p$yc.7 <- SMA(p$usgg10yr - p$usgg3m, 2)

	################################
	
	# Clean model inputs - discard missing vars.
	vars <- names(p)[c(1, 46:ncol(p))]
	p <- as.data.frame(p[, vars])
	omit <- c()
	for(i in 1:ncol(p)){
		if( length(which(is.na(p[,i]))) > 50){
			omit <- c(omit, i)
		}
	}
	names(p)[omit]
	p <- p[, -omit]
	p <- p[complete.cases(p),]

	return(p)

}
{% endhighlight %}

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

p <- GetInputs()

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
usr <- zoo(p$usrindex, as.Date(rownames(p), "%Y-%m-%d"))
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

