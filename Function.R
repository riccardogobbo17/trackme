# Function to clean data and remove a noise from them
noise <- function(ds){
  n = length(ds[,1])
  k = 1
  while(k<n){
    if(abs(ds[k,3]-ds[k+1,3])<5){
      ds <- ds[-(k+1),]
      n = length(ds[,1])
    }
    else{
      k = k+1
    }
  }
  return(as.data.frame(ds[,c(1,2)]))
}

#haussdorf distance
haud_dist<-function(lst,n){
  mat<-matrix(0,nrow=n,ncol=n)
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      mat1<-cbind(lst[[i]]$lon,lst[[i]]$lat)
      mat2<-cbind(lst[[j]]$lon,lst[[j]]$lat)
      dist<-distmat(mat1, mat2)
      maxmin_PQ <- max(apply(dist, 1, min))
      maxmin_QP <- max(apply(dist, 2, min))
      mat[i,j]<-(max(maxmin_PQ, maxmin_QP))
    }
  }
  return(mat)
}

## Boxcar kernel density
q_density <- function(haus,epsilon,n){
  vec <- rep(NA, n)
  for(i in 1:n){
    vec[i] = (sum(haus[i,] <= epsilon))/n
  }
  return(as.data.frame(vec))
}

#plot Kernel
plot.kern<-function(dat){
  ggplot(data=dat, aes(x=1:60, y=vec, group=1)) +
    geom_step(direction='hv')+
    geom_point()+labs(title="Behaviour of Kernel by epsilon",
                       x ='Run number', y = 'Boxcar-kernel value')+scale_x_discrete(limits=1:60)
}

#Find top-5
max.find<-function(result){
  sorted <- sort(result$vec, index.return=TRUE, decreasing = TRUE)
  highest <- data.frame(c(sorted$x[1:5]),c(sorted$ix[1:5]))
  colnames(highest) = c("Density", "Runtrack")
  return(highest)
}

#Find bottom-5
min.find<-function(result){
  sorted <- sort(result$vec, index.return=TRUE, decreasing = FALSE)
  lowest <- data.frame(c(sorted$x[1:5]),c(sorted$ix[1:5]))
  colnames(lowest) = c("Density", "Runtrack")
  return(lowest)
}

# plot maps
plot.map<-function(map,df,s,col,a,tit){
  pl <- ggmap(map) + geom_point(data = df, aes(x = lon, y = lat), size = s, colour = col, alpha = a)+labs(title=tit)
  return(pl)
}

# plot paths
plot.path<-function(map,df,s,a,tit){
  pl<-ggmap(map) + geom_path(data = df, aes(x = lon, y = lat, col = id), size = s,
                              lineend = "round", alpha = a)+labs(title=tit)
  return(pl)
}

# dataframe top-5
df.max<-function(run,df,q){
  runmax<-subset(run,id %in% names(df[q[,2]]))
  return(runmax)
}

#dataframe bottom-5
df.min<-function(run,df,q){
  runmin<-subset(run,id %in% names(df[q[,2]]))
  return(runmin)
}