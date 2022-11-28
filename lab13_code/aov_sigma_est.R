
# model_obj objekt av klassen "aov"
# model = 1 ger envägs-ANOVA
# model = 2 ger tvåvägs-ANOVA
# interaction finns det interaktion i modellen (givet model=2)?
# a och b: antal faktornivåer
# n antal obs per cell
aov_sigma_est<-function(model_obj,model=1,interaction=FALSE,a,b,n){
  if(class(model_obj)[1]!="aov") stop("model_obj must be of class aov!")
  
  anova_tab<-anova(model_obj)
  
  if(model==1){
    MSE<-anova_tab$`Mean Sq`[2]
    MSA<-anova_tab$`Mean Sq`[1]
    sig_est<-MSE
    sig_A_est<-(MSA-MSE)/n
    res<-data.frame(parameter=c("sigma2","sigma2_A"),est=c(sig_est,sig_A_est))
  }else if(model==2){
    if(interaction){
      MSE<-anova_tab$`Mean Sq`[4]
      MSAB<-anova_tab$`Mean Sq`[3]
      MSB<-anova_tab$`Mean Sq`[2]
      MSA<-anova_tab$`Mean Sq`[1]
      
      
      sig_est<-MSE
      
      sig_AB_est<-(MSAB-MSE)/n
      
      sig_A_est<-(MSA-MSE-n*sig_AB_est)/(b*n)
      sig_B_est<-(MSB-MSE-n*sig_AB_est)/(a*n)
      
      res<-data.frame(parameter=c("sigma2","sigma2_A","sigma2_B","sigma2_AB"),est=c(sig_est,sig_A_est,sig_B_est,sig_AB_est))
      
    }else{
      
      MSE<-anova_tab$`Mean Sq`[3]
      MSB<-anova_tab$`Mean Sq`[2]
      MSA<-anova_tab$`Mean Sq`[1]
      
      
      sig_est<-MSE
      
      
      sig_A_est<-(MSA-MSE)/(b*n)
      sig_B_est<-(MSB-MSE)/(a*n)
      
      res<-data.frame(parameter=c("sigma2","sigma2_A","sigma2_B"),est=c(sig_est,sig_A_est,sig_B_est))
      
      
      
    }
  }else{
    stop("model must be either 1 or 2")
  }
  return(res)
}