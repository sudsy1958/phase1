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
load.packages('quantmod')   

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

