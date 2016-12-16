library('nortest')
library('ellipse')
x = read.csv("hw45-r3b-test-data.csv")
x = as.matrix(x)

Gmeans = function(a,k=1,alpha=0.0001)
{
  set.seed(1)
  par(mfrow = c(1,1))
  #Calculating kmeans for the first time
  first = kmeans(a,k)
  cent = first$centers
  #flag variable is used to exit the outer loop
  flag = 1
  repeat{
    k = nrow(cent)
    flag = 0
    #temp_cent is used to store the temporary centers
    temp_cent = matrix(,0,ncol = dim(cent)[2])
    for (i in 1:k) 
    {
      #temp stores the data points belonging to ith cluster
      temp = subset(a,first$cluster==i)
      
      #Skip the cluster if it is below 8 datapoints
      if(nrow(temp)<8){
        next;
      }
      
      #Scaling and projecting temp to 1D data for normality test
      kme = kmeans(temp,2)
      center = kme$centers[1,] - kme$centers[2,]
      temp_test = temp %*% center
      temp_test = scale(temp_test)
      
      #ad.test is normlity test
      test = ad.test(temp_test)
      if(test$p.value<=alpha)
      {
        #if it fails, then two new centers are added to temp_cent
        clus = kmeans(temp,2)
        temp_cent = rbind(temp_cent,clus$centers)
        flag = 1
      }
      else
      {
        #if it doesnt fail, it adds the center to temp_cent
        temp_cent = rbind(temp_cent,cent[i,])
      }
    }
    #replace centers with temp_cent
    cent = temp_cent
    #apply kmeans on the entire data with these centers
    first = kmeans(a,cent)
    cent = first$centers
    if(flag == 0)
    {
      #Generating plots and ellipses
      cols = ncol(a)
      for(k in 1:(cols-1)){
        for(j in (k+1):cols){
          plot(a[,c(k,j)],col=first$cluster)
          for (i in 1:nrow(cent)) {
            temp = a[which(first$cluster==i),c(k,j)]
            cm = cov(temp)
            center_e = cent[i,c(k,j)]
            e = ellipse(cm, centre = center_e,level = 0.99)
            lines(e, col='blue',type='l')
            points(x = cent[i, k], y = cent[i, j], pch = '+', cex = 2)
          }
        }
      }
      break;
    }
  }
  print("Number of clusters:")
  print(nrow(first$centers))
}
Gmeans(x)

