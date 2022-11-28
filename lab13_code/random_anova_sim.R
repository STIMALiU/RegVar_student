random_anova_sim<-function(model,seed1=NULL,seed2=NULL,mu,n,sigma2,sigma2_A,sigma2_B=0,sigma2_AB=0,a,b){
  
  check<-sigma2<0|sigma2_A<0|sigma2_B<0|sigma2_AB<0
  if(check) stop("sigma2, sigma2_A, sigma2_B and sigma2_AB must be >=0")
  
  set.seed(seed1)
  if(model==1){
    # en faktor
    if(sigma2_A>0){
      set.seed(seed1)
      A<-rnorm(n = a,mean = 0,sd = sqrt(sigma2_A))   
    }else{
      A<-rep(0,a)
    }
    
    H_grid<-paste0("a",1:a)
    index<-rep(1:length(H_grid),each=n)
    H_data<-sort(A)
    set.seed(seed2)
    y<-mu+rep(H_data,each=n)+rnorm(n = n*a,mean = 0,sd = sqrt(sigma2))
    X<-H_grid[index]
    df<-data.frame(y=y,A=X)
    rownames(df)<-NULL
    df$A<-as.factor(df$A)
  }else if(model==2){
    # två faktorer
    
    set.seed(seed1)
    if(sigma2_A>0){
      A<-rnorm(n = a,mean = 0,sd = sqrt(sigma2_A))
    }else{
      A<-rep(0,a)
    }
    if(sigma2_B>0){
      B<-rnorm(n = b,mean = 0,sd = sqrt(sigma2_B))
    }else{
      B<-rep(0,b)
    }
    if(sigma2_AB>0){
      # två faktorer med interaktion
      AB<-rnorm(n = a*b,mean = 0,sd = sqrt(sigma2_AB))
    }else{
      AB<-rep(0,a*b)
    }
    H_grid<-expand.grid(A=paste0("a",1:a),B=paste0("b",1:b))
    index<-rep(1:nrow(H_grid),each=n)
    H_data<-expand.grid(A=sort(A),B=sort(B))
    rand_part<-rowSums(H_data)+AB

    set.seed(seed2)
    y<-mu+rep(rand_part,each=n)+rnorm(n = n*a*b,mean = 0,sd = sqrt(sigma2))
    X<-H_grid[index,]
    df<-data.frame(y=y,X)
    rownames(df)<-NULL
    df$A<-as.factor(df$A)
    df$B<-as.factor(df$B)
  }else{
    stop("model must be either 1 or 2")
  }
  return(df)
}