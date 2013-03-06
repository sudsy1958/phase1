###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
setInternet2(TRUE)
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)


#*****************************************************************
# Load historical data
#****************************************************************** 
library('quantmod') 
bt.pca.test()

# load data saved in the bt.pca.test() function
load(file='bt.pca.test.Rdata')

#*****************************************************************
# Principal component analysis (PCA), for interesting discussion
# http://machine-master.blogspot.ca/2012/08/pca-or-polluting-your-clever-analysis.html
#****************************************************************** 
prices = data$prices    
ret = prices / mlag(prices) - 1

p = princomp(na.omit(ret))

loadings = p$loadings[]

x = loadings[,1]
y = loadings[,2]
z = loadings[,3] 
#*****************************************************************
# Principal component analysis (PCA), for interesting discussion
# http://machine-master.blogspot.ca/2012/08/pca-or-polluting-your-clever-analysis.html
#****************************************************************** 
prices = last(data$prices, 1000)
n = len(tickers)        
ret = prices / mlag(prices) - 1

p = princomp(na.omit(ret[1:250,]))

loadings = p$loadings[]

# look at the first 4 principal components  
components = loadings[,1:4]

# normalize all selected components to have total weight = 1
components = components / repRow(colSums(abs(components)), len(tickers))

# note that first component is market, and all components are orthogonal i.e. not correlated to market
market = ret[1:250,] %*% rep(1/n,n)
temp = cbind(market, ret[1:250,] %*% components)
colnames(temp)[1] = 'Market'   

round(cor(temp, use='complete.obs',method='pearson'),2)

# the variance of each component is decreasing
round(100*sd(temp,na.rm=T),2)
#*****************************************************************
# Find stationary components, Augmented Dickey-Fuller test
#******************************************************************     
library(tseries)
equity = bt.apply.matrix(1 + ifna(-ret %*% components,0), cumprod)
equity = make.xts(equity, index(prices))

# test for stationarity ( mean-reversion )
adf.test(as.numeric(equity[,1]))$p.value
adf.test(as.numeric(equity[,2]))$p.value
adf.test(as.numeric(equity[,3]))$p.value
adf.test(as.numeric(equity[,4]))$p.value
#*****************************************************************
# Plot securities and components
#*****************************************************************
layout(1:2)

# add Bollinger Bands
i.comp = 4
bbands1 = BBands(repCol(equity[,i.comp],3), n=200, sd=1)
bbands2 = BBands(repCol(equity[,i.comp],3), n=200, sd=2)
temp = cbind(equity[,i.comp], bbands1[,'up'], bbands1[,'dn'], bbands1[,'mavg'],
             bbands2[,'up'], bbands2[,'dn'])
colnames(temp) = spl('Comp. 4,1SD Up,1SD Down,200 SMA,2SD Up,2SD Down')

plota.matplot(temp, main=paste(i.comp, 'Principal component'))

barplot.with.labels(sort(components[,i.comp]), 'weights')  

