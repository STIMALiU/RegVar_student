sim_anova_func<-function(ver,n_obs=200,seed1=235,seed2=4655){
  n_obs<-200
  set.seed(seed1)
  A<-sample(c("a1","a2","a3"),size = n_obs,replace = TRUE,prob = c(0.4,0.35,0.25))
  B<-sample(c("b1","b2"),size = n_obs,replace = TRUE,prob = c(0.6,0.4))
  A<-as.factor(A)
  B<-as.factor(B)
  X<-model.matrix(~A+B+A*B)
  if(ver==1){
    reg_beta<-c(25,-10,7,4,-8,12)
  }else if(ver==2){
    reg_beta<-c(25,-10,7,-5,0,0)
  }else if(ver==3){
    reg_beta<-c(25,-10,7,0,0,10)
  }else if(ver==4){
    reg_beta<-c(25,0,0,7,5,-10)
  }else if(ver==5){
    reg_beta<-c(25,2,-1,1.5,0.5,-0.5)
  }else{
    stop("ver= must be om of: 1 2 3 4 5")
  }
  set.seed(seed2)
  y<-X%*%reg_beta+rnorm(n = nrow(X),sd=4)
  return(data.frame(y=y,A=A,B=B))
}