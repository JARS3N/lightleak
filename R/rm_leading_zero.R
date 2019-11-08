rm_leading_zero<-function(u){
  if(substr(u,1,1)=="0"){
    rm_leading_zero(substr(u,2,nchar(u)))
  }else{
    return(u)
  }
}
