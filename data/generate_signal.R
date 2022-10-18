generate_signal<-function(seed=NULL){
  n_obs<-500
  if(is.null(seed)){
    seed<-sample(1e5,1)
  }
  #browser()
  set.seed(seed)
  x<-sort(runif(n = n_obs,min = -20,max = 20))
  
  sinc<-function(x){ifelse(x==0,1,sin(x)/x)}
  
  
  a<-sample(c(-1,1),1)
  z0<-a*sinc(x)
  
  g<-sample(3,1)
  if(g==1){
    index<-x<0
    if(a==-1){
      b<- -1
    }else{
      b<-1
    }
    z0[index]<-b*abs(z0[index])
  }
  if(g==2){
    if(a==-1){
      b<- -1
    }else{
      b<-1
    }
    index<-x>0
    z0[index]<-b*abs(z0[index])
  }
  g2<-sample(3,1)
  if(g2==1){
    index<-x<0
    z0[index]<-z0[index]+0.002*x[index]^2
  }
  if(g2==2){
    index<-x>0
    z0[index]<-(z0[index])+0.002*x[index]^2
  }
  d<-sample(2,1)
  f<-sample(x = c(-1,-0.75,0.75,1),size = 1)
  u<-0
  if(d==1){
    step_point<-runif(n = 1,min = -15,max = -5)
    u<-ifelse(x<=step_point,f,0)
  }
  if(d==2){
    step_point<-runif(n = 1,min = 5,max = 15)
    u<-ifelse(x>=step_point,f,0)
  }
  y<-z0+u+rnorm(n = n_obs,sd=0.05)
  
  res_list<-list(data=data.frame(x=x,y=y),seed=seed)
  return(res_list)
}