################################################################################
################################################################################
# Enkel linjär logistisk regression
################################################################################
################################################################################

rm(list=ls())


################################################################################
# Logistiska funktionen
################################################################################

logistic_func<-function(z){
  p<-exp(z)/(1+exp(z))
  return(p)
}
# hur ser den ut?
curve(expr = logistic_func,from = -10,to = 10)
abline(h = c(0,0.5,1),v=0,lty="dashed",col="gray")


# värden >= 0.5 klassificerar vi som klass 1, annars klass 0



################################################################################
# Simulera data
################################################################################

n_obs<-200
x<-seq(-5,5,length=n_obs)
beta0_ver1<- 0
beta1_ver1<-1
z1<-beta0_ver1 + beta1_ver1*x
prob_vect1<-logistic_func(z = z1)
hist(prob_vect1)
mean(prob_vect1>=0.5)



plot(x,prob_vect1,t="l")
abline(h = c(0,0.5,1),v=0,lty="dashed",col="gray")

# prob_vect1 anger varje obs sannolikhet anta klass 1


# slumpa fram data
?rbinom()
# Bernoulli är ett specialfall av binomial, när n=1 (antal försök) i binomial
set.seed(434)
y1<-rbinom(n = n_obs,size = 1,prob = prob_vect1)

plot(x,y1)

df1<-data.frame(x=x,y=y1,class=ifelse(y1==1,"1","0"))

library(tidyverse)

df1 %>%
  ggplot(aes(x, y)) +
  geom_point(alpha = .3,aes(col=class)) 

# lägga till skattad logistisk funktion 
p1<-df1 %>%
  ggplot(aes(x, y)) +
  geom_point(alpha = .3,aes(col=class)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),se=FALSE)+
  ggtitle(paste("sanna beta0 =",beta0_ver1," sanna beta1 = ",beta1_ver1))
# den blå linjen ger den anpassade sannolikheten för y=1
# om sannolikheten är >= 0.5 så klassificerar vi som y=1, annars som y=0
p1



# nu testar vi att ändra parametervärden

# om vi har en mer brant lutning på kurvan så ökar vi beta1
beta0_ver2<- 0
beta1_ver2<-5
z2<-beta0_ver2 + beta1_ver2*x
prob_vect2<-logistic_func(z = z2)
set.seed(753)
y2<-rbinom(n = n_obs,size = 1,prob = prob_vect2)
df2<-data.frame(x=x,y=y2,class=ifelse(y2==1,"1","0"))

p2<-df2 %>%
  ggplot(aes(x, y)) +
  geom_point(alpha = .3,aes(col=class)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),se=FALSE)+
  ggtitle(paste("sanna beta0 =",beta0_ver2," sanna beta1 = ",beta1_ver2))
p2

# om vi har en mindre brant lutning på kurvan så minskar vi beta1
beta0_ver3<- 0
beta1_ver3<-0.3
z3<-beta0_ver3 + beta1_ver3*x
prob_vect3<-logistic_func(z = z3)
set.seed(543)
y3<-rbinom(n = n_obs,size = 1,prob = prob_vect3)
df3<-data.frame(x=x,y=y3,class=ifelse(y3==1,"1","0"))

p3<-df3 %>%
  ggplot(aes(x, y)) +
  geom_point(alpha = .3,aes(col=class)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),se=FALSE)+
  ggtitle(paste("sanna beta0 =",beta0_ver3," sanna beta1 = ",beta1_ver3))
p3


library(cowplot)
plot_grid(p1,p2,p3,nrow = 3)


# gör nu 3 nya plottar där ni har beta1=1 och testar beta0: -4 , 0 , 4
# vad händer?



# Vad händer om vi har negativa beta1?
beta0_ver4<- 0
beta1_ver4<--1
z4<-beta0_ver4 + beta1_ver4*x
prob_vect3<-logistic_func(z = z4)
set.seed(543)
y4<-rbinom(n = n_obs,size = 1,prob = prob_vect3)
df4<-data.frame(x=x,y=y4,class=ifelse(y4==1,"1","0"))

p4<-df4 %>%
  ggplot(aes(x, y)) +
  geom_point(alpha = .3,aes(col=class)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),se=FALSE)+
  ggtitle(paste("sanna beta0 =",beta0_ver3," sanna beta1 = ",beta1_ver3))
p4
# testa att ändra beta1 till -5 och -0.3




################################################################################
# Skatta modellen
################################################################################

rm(list=ls())

logistic_func<-function(z){
  p<-exp(z)/(1+exp(z))
  return(p)
}

?glm

# simulera lite data
n_obs<-100
set.seed(334)
y<-rep(c(0,1),each=n_obs/2)
x<-rnorm(n = n_obs,mean = rep(c(-1,1),each=n_obs/2),sd = 0.7)
# sotera data efter storleken på x
y<-y[order(x)]
x<-x[order(x)]

df1<-data.frame(x=x,y=y,class=ifelse(y==1,"1","0"))

library(tidyverse)

df1 %>%
  ggplot(aes(x, y)) +
  geom_point(alpha = .7,aes(col=class)) 

?glm
# glm kan anpassa olika generaliserade linjära modeller 
# family = "binomial" ger logistisk regression
A1<-glm(formula = y~x,family = "binomial")

A1
coef(A1)
A1_sum<-summary(A1)
A1_sum
A1_sum$coefficients


A1_sum$family
A1_sum$aic

# anpassade värden
# type = "response" ger skattade sannolikheter
y_hat_prob<-predict(object = A1,type = "response")

plot(x,y_hat_prob,t="l")

# type = "link" ger skattade linjära prediktorn 
# (innan vi använder den logistiska funktionen)
z_hat<-predict(object = A1,type = "link")
plot(x,z_hat)
# vi kan räkna för hand:
z_hat2<-coef(A1)[1]+coef(A1)[2]*x
# z_hat och z_hat2 är samma

y_hat_prob2<-logistic_func(z_hat)
# y_hat_prob och y_hat_prob2 är samma

# predikterar en obs som 1 om sannolikheten är 0.5 eller högre
y_hat<-ifelse(y_hat_prob>=0.5,1,0)


# linjär logistisk regression skapar linjära beslutsgränser
# om vi har en kontinuerlig förklarande variabel så kommer vi ha en punkt d
# som delar upp x i två delar
# -oändligheten < x< d : här predikterar vi till klass 0 (i exemplet)
# d <= x < +oändligheten : här predikterar vi till klass 1 (i exemplet)
# gränsen d går vid det x som uppfyller p(y=1)=0.5
# så vi kan lösa ekvationen 0.5 = 1 / (1+exp(beta0 + beta1*x)) för x
# så får vi
# x = -beta0/beta1
d<- -coef(A1)[1]/coef(A1)[2]

# plotta data, anpassade värden och beslutsgränsen
df1$y_prob<-y_hat_prob

df1 %>%
  ggplot(aes(x, y)) +
  geom_point(alpha = .7,aes(col=class))+theme_bw()+
  geom_line(aes(y=y_prob))+geom_vline(xintercept=d,linetype="dashed",col="blue")
# beslutsgränsen är den blå streckade linjen
# alla punkter till vänster om den linjen klassificeras som 0
# alla punkter till höger om den linjen klassificeras som 1


A1_sum
residuals(A1,type = "deviance")
A1_sum$deviance
sum(residuals.glm(A1,type = "deviance")^2)

A1_sum$cov.unscaled # skattning av kovariansmatrisen för beta_hat 
A1_sum$coefficients[,1:2] # SE för beta_hat
sqrt(diag(A1_sum$cov.unscaled))
# kontrollräkna:
A1_sum$coefficients[,2]-sqrt(diag(A1_sum$cov.unscaled))

confint(A1,level = 0.9)
