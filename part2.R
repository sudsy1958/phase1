#*****************************************************************
# Create clusters
#******************************************************************         
# create and plot clusters based on the first and second principal components
hc = hclust(dist(cbind(x,y)), method = 'ward')
plot(hc, axes=F,xlab='', ylab='',sub ='', main='Comp 1/2')
rect.hclust(hc, k=3, border='red')
