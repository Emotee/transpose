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


myvar1 <-  matrix(1:10, nrow=5, ncol=2)
t1 <-  matrix(c(1,6,2,7,3,8,4,9,5,10), nrow=2,ncol=5)

myvar2 <-  matrix(NA, nrow=0, ncol=0)
t2 <-  0

myvar3 <-  matrix(c(1,2), nrow=1, ncol=2)
t3 <-  matrix(1:2,2,1)

myvar4 <-  matrix(c(1,2), nrow=2, ncol=1)
t4 <-  matrix(1:2,1,2)

myvar5 <- c(1,2,NA,3)
t5 <- matrix(c(1,2,NA,3),1,4)

myvar6 <- c(NA)
t6 <- matrix(NA,1,1)

myvar7 <- c()
t7 <- NULL

d <- c(1,2,3,4)
e <- c("red", "white", "red", NA)
f <- c(TRUE,TRUE,TRUE,FALSE)
mydata3 <- data.frame(d,e,f)

for(i in 1:nrow(myvar1)){
  for(j in 1:ncol(myvar1))
  if(t1[j,i]==mytranspose(myvar1)[j,i]){
   h<-TRUE
  }
  else(
    h<-FALSE
  )
  print(h)
}


if(mytranspose(myvar2)==t2){
  print(TRUE)
}

for(i in 1:nrow(myvar3)){
  for(j in 1:ncol(myvar3))
    if(t3[j,i]==mytranspose(myvar3)[j,i]){
      h<-TRUE
    }
  else(
    h<-FALSE
  )
  print(h)
}

for(i in 1:nrow(myvar4)){
  for(j in 1:ncol(myvar4))
    if(t4[j,i]==mytranspose(myvar4)[j,i]){
      h<-TRUE
    }
  else(
    h<-FALSE
  )
  print(h)
}

for(i in 1:nrow(as.matrix(myvar5))){
  for(j in 1:ncol(as.matrix(myvar5)))
    if(is.na(t5[j,i])&&is.na(mytranspose(myvar5)[j,i])){
      
      h<-TRUE
    }
  else if(t5[j,i]==mytranspose(myvar5)[j,i]){
    
      h<-TRUE
    }
  else(
    h<-FALSE
  )
  print(h)
}

for(i in 1:nrow(as.matrix(myvar6))){
  for(j in 1:ncol(as.matrix(myvar6)))
    if(is.na(t6[j,i])&&is.na(mytranspose(myvar6)[j,i])){
      
      h<-TRUE
    }
  else if(t6[j,i]==mytranspose(myvar6)[j,i]){
    
    h<-TRUE
  }
  else(
    h<-FALSE
  )
  print(h)
}

if(is.null(mytranspose(myvar7))){
  print(TRUE)
}


