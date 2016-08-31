---
layout: post
title:  "Forecasting US Recessions with LASSO in Python"
date:   2016-08-25
---
This post gives some Python code to implement LASSO-based forecasts of economic recessions. It relies upon [recession_data.csv]({{ site.url }}/assets/recession_data.csv). 

You'll need the following modules: 

{% highlight python %}
 import os, re, datetime
 import numpy as np
 import pandas as pd
 import matplotlib.pyplot as plt
 from sklearn.linear_model import LogisticRegression 

{% endhighlight %}

We'll need to clean the data first, making variable names one word (without Index on the end), removing non-numeric characters, and making variables lowercase. 

<!--more-->

{% highlight python %}
"""
Clean the data
"""
os.chdir('/Users/jasonweinreb/jweinreb.git/jweinreb.github.io/Recession Prediction')
dateparse = lambda x: pd.datetime.strptime(x, '%m/%d/%y')
df = pd.read_csv("recession_data.csv", parse_dates = [0], index_col = [0], date_parser = dateparse)
df = df.iloc[(df.index.year >= 1978) & (df.index.year < 2016)]
df = df.fillna(method = 'pad')

df.columns = [var.lower() for var in df.columns]
df.columns = [re.sub(r' index', '', col) for col in df.columns]
df.columns = [re.sub(r' ', '', col) for col in df.columns]
df.columns = [re.sub(r'%', '', col) for col in df.columns]
df.columns = [re.sub(r'#', '', col) for col in df.columns]

{% endhighlight %}

Next, we'll code up variables using pandas for help with rolling averages, etc. 

{% highlight python %}

'''
Consumer Sentiment 
'''
df['conssent1'] = df.conssent.rolling(window = 3).mean().pct_change(6) # 6m change in 3mma
df['conssent2'] = (df.conssent/df.conssent.rolling(window = 12).max()).rolling(window = 9).min() # 9m min of current / annual max 
ser = 0.5 * np.sign(df.leicexp.rolling(window = 2).mean().diff(1)) + 0.5 
df['conssent3'] = ser.rolling(window = 8).sum()# 8m count of increase in 2mma

'''
Corporate Credit
'''
df['cred1'] = df.moodcavg.rolling(window = 2).mean().pct_change(6) #6m pc in 2mma
df['cred2'] = (df.moodcbaa - df.usgg10yr).rolling(window = 4).mean() # 4mma
df['cred3'] = (df.moodcaaa - df.moodcbaa).pct_change(6) #6m pc in spread
ser = 0.5*np.sign(df.moodcavg.rolling(window = 2).mean().diff(1)) + 0.5
df['cred4'] = ser.rolling(window = 8).sum() # 8m count of increase in 2mma

'''
Broad Indices
'''
df['indices1'] = df.adsbci
df['indices2'] = df.cfnai
df['indices3'] = df.coitotl.pct_change(6) #6m pc
df['indices4'] = df.leilci
df['indices5'] = df.oustdiff.rolling(window = 3).mean() # 3mma
df['indices6'] = df.phffanx0
df['indices7'] = df.s5finl / df.s5finl.rolling(window = 14).max()
ser = 0.5*np.sign(df.leiyoy.rolling(window = 2).mean().diff(1)) + 0.5
df['indices8'] = ser.rolling(window = 8).sum() # 8m count of increase in 2mma

'''
Housing
'''
df['housing1'] = df.leibp.rolling(window = 3).mean().pct_change(12) # 12m pc of 3mma
df['housing2'] = df.nhslnfs.pct_change(12) #12m pc
df['housing3'] = df.nhsltot.rolling(window = 3).mean() # 3mma

'''
Manufacturing
'''
df['manu1'] = df.napmnewo / df.napminv # current ord/inv ratio
df['manu2'] = df.napmpmi.rolling(window = 2).mean() # 2mma
df['manu3'] = df.crbrind / df.crbrind.rolling(window = 12).max() # current value / 12m max
df['manu4'] = df.ecrsuscp.pct_change(6).rolling(window = 4).mean() #4mma of 6m change
df['manu5'] = df.ipvptrmh.pct_change(12).rolling(window = 3).mean() # 3mma in 12m pc
ser = 0.5*np.sign(df.leinwcn.rolling(window = 2).mean().diff(1)) + 0.5
df['manu6'] = ser.rolling(window = 8).sum() # 8m count of increase in 2mma

'''
Macro
'''
df['macro1'] = df.uscrwtic.pct_change(12) #12m pc
df['macro2'] = df.dxy.rolling(window = 4).mean() #4mma
df['macro3'] = df.gdpcyoy
df['macro4'] = df.m1yoy - df.cpiyoy # real m1 yoy

'''
Equities
'''
df['equities1'] = df.spx.pct_change(6) #6m pc
ser = 0.5*np.sign(df.leistkp.rolling(window = 2).mean().diff(1)) + 0.5
df['equities2'] = ser.rolling(window = 8).sum() #8m count in increase in 2mma
ser = df.spx.pct_change(6) / df.usgg10yr.pct_change(6)
df['equities3'] = ser.rolling(window = 4).mean()
df['equities4'] = df.tran / df.tran.rolling(window = 12).max()
'''
Employment
'''
df['emp1'] = df.injcjc4.rolling(window = 2).mean().pct_change(6)#6m pc in 2mma
df['emp2'] = df.injcjc4.pct_change(12) #12m pc 
df['emp3'] = df.injcjc4 / df.injcjc4.rolling(window = 12).min() # cufrent / 12m min
ser = 0.5*np.sign(df.leiavgw.rolling(window = 2).mean().diff(1)) + 0.5
df['emp4'] = ser.rolling(window = 8).sum() # 8m count of increase in 2mma
df['emp5'] = df.leiwkij.rolling(window = 2).mean() #2mma
df['emp6'] = df.nfpt.rolling(window = 3).mean().pct_change(12) # 12m pc in 3mma
df['emp7'] = 50 - df.oustneg
df['emp8'] = df.usestemp.pct_change(12) #12m pc

'''
Yield Curve
'''
df['yc1'] = (df.dljhytw - df.usgg10yr)/(df.dljhytw - df.usgg10yr).rolling(window = 12).max() # spread/12mm max
ser = 0.5*np.sign(df.leiirte.rolling(window = 2).mean().diff(1))+ 0.5
df['yc2'] = ser.rolling(window = 8).sum() #8m count of increase in 2mma
df['yc3'] = df.usgg10yr/df.usgg10yr.rolling(window = 12).max() # current/12m max
df['yc4'] = (df.usgg10yr - df.usgg5yr).pct_change(6) #5s10s 6m pc
df['yc5'] = (df.usgg10yr - df.usgg5yr).rolling(window = 2).mean() #2mma
df['yc6'] = (df.usgg10yr - df.usgg3m).pct_change(6) # 3s10s 6m pc
df['yc7'] = (df.usgg10yr - df.usgg3m).rolling(window = 2).mean()

{% endhighlight %}

Let's remove variables with more than 50 missing observations, which will severely limit the size of our training data. The code is a bit clunky but gets the job done: 

{% highlight python %}

# Remove variables with more than 50 missing obs.
df = df.ix[:, np.concatenate(([0], np.array(range(45, len(df.columns)))))]
idx = np.zeros(len(df.columns))
for i in range(0, len(df.columns)):
    idx[i] = np.sum(df.ix[:,i].isnull().sum() > 50)
    
idx2 = [i for i,x in enumerate(idx) if x == 0]
df = df.ix[:,idx2]
idx3 = [j for j in range(0,len(df.index)) if df.ix[j,:].isnull().sum() == 0]
df = df.ix[idx3,:]

{% endhighlight %}

Now we're ready to fit the lasso to our training data, all observations before March 2005. Our test data will be all observations from April 2005. 

{% highlight python %}
 
train = df.ix[:'2005-03-01', :]
test = df.ix['2005-04-01': , :]
               
X = train.ix[:, 1:]
y = train['usrindex']
mod = LogisticRegression(penalty = 'l1').fit(X, y)

{% endhighlight %}

Next we can fit our model to the test data and compute our out-of-sample MSE.

{% highlight python %}

X2 = test.ix[:, 1:]
y2 = test.ix[:, 0]
yhat2 = mod.predict(X2)
mse = sum(abs(yhat2 - y2))*(1/len(y2))
print(mse)
# 0.0542635658915

{% endhighlight %}

{% marginfigure "fig3" 'assets/img/recession-py.png' "Predicted Probability of Recession in the US" %}

Finally, we can plot the in-sample and out-of-sample recession forecasts with matplotlib.

{% highlight python %}
plt.figure()
plt.plot(X.index, mod.predict_proba(X)[:, 1], label= 'In-sample prediction (to Mar 2005)', linewidth = 2)
plt.ylabel('Prob(Recession)')
plt.xlim(df.index[1], df.index[len(df.index)-1])
plt.plot(X2.index, mod.predict_proba(X2)[:, 1], color = 'r', 
         label = 'Out-of-sample prediction (from Apr 2005)', linewidth = 2)
plt.title('LASSO Logistic Model of US Recessions')
# Shrink current axis's height by 10% on the bottom
ax = plt.subplot(111)
box = ax.get_position()
ax.set_position([box.x0, box.y0 + box.height * 0.1,
                 box.width, box.height * 0.9])
# Put a legend below current axis
ax.legend(loc='upper center', bbox_to_anchor=(0.5, -0.05),
          fancybox=True, shadow=True, ncol=1)
plt.show()

{% endhighlight %}



