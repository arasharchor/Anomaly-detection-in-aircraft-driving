# Remove Outliers

file=file[!(file$ALT>1500),]
file=file[(file$BPYR_1>quantile(file$BPYR_1,.25)-1.5*IQR(file$BPYR_1) & file$BPYR_1<quantile(file$BPYR_1,.75)+1.5*IQR(file$BPYR_1)),]

# Normalization

normal <- function(x){
n = (x-min(x))/(max(x)-min(x))
}

# Feature Engineering

  # Grouping based on airport locations
  # air1 is a variable having dataset
  a <- air1$LATP > (44.88 - err) & air1$LATP < (44.88 + err)
  b <- air1$LONP > (-103.05 - err) & air1$LONP < (-103.05 + err)
  air1 <- air1[a,]
  air1 <- air1[b,]
  dset <- air1[c(-1,-2,-3,-4,-5,-6,-15)]
  
# Clustering

clus <- Kmeans(dset, centers = 11, nstart = 5, iter.max = 100, method = "correlation")

# Plotting

plotcluster(dset, clus$cluster)
