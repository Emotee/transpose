mytranspose <- function(x) {
  if(is.null(x)){
    y<-NULL
    return(y)
  }
  
  z<-as.matrix(x) 
  if(nrow(z)==0){
    y<-0
    return(y)
  }
  y <- matrix(1, nrow=ncol(z), ncol = nrow(z))
  for(i in 1:nrow(z)) {
    for(j in 1:ncol(z)) {
      y[j,i] <- z[i,j]
    }
  }
  return(y)
}