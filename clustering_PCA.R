###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
#setInternet2(TRUE)
#con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
#source(con)
#close(con)

#*****************************************************************
# Load historical data
# http://www.r-bloggers.com/clustering-with-selected-principal-components/
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
#*****************************************************************
# Create clusters
#******************************************************************         
# create and plot clusters based on the first and second principal components
hc = hclust(dist(cbind(x,y)), method = 'ward')
plot(hc, axes=F,xlab='', ylab='',sub ='', main='Comp 1/2')
rect.hclust(hc, k=3, border='red')
# create and plot clusters based on the first, second, and third principal components
hc = hclust(dist(cbind(x,y,z)), method = 'ward')
plot(hc, axes=F,xlab='', ylab='',sub ='', main='Comp 1/2/3')
rect.hclust(hc, k=3, border='red')
# create and plot clusters based on the correlation among companies
hc = hclust(as.dist(1-cor(na.omit(ret))), method = 'ward')
plot(hc, axes=F,xlab='', ylab='',sub ='', main='Correlation')
rect.hclust(hc, k=3, border='red')
