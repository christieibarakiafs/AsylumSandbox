add <- function(x){
  bucket = 0
  for(i in x){
    bucket = bucket + i
  }
  return(bucket)
}

prime_check<- function(x){

  if (x <= 1) {
    return(FALSE)
  }
  
  if( x >1){
    for (i in 2:(x-1)){
      if( x %% i == 0){
        return(FALSE)
      }
    }
    return(TRUE)
}
}


square <- function(x){
j = x ^ 0.5
  return(j)}



GCF <- function(x,y){
  if (y ==0 ){
    return(x)
  }
  else {
    return(GCF(y,x%%y))
  }
}


