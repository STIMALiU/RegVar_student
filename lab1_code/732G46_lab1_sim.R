#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 732G46 Lab 1: Simulering av regressionsdata
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

library(ggplot2)
library(cowplot)

#-------------------------------------------------------------------------------
# Börja simulera
# olika värden på beta0 och beta1
#-------------------------------------------------------------------------------

n<-100 # antal obs
# skapa en vektor med x-värden
# här är bara en sekvens med värden
x<-seq(from=1,to=20,length=n)
# andra alternativ är att simulera x från
# uniformfördelning
#set.seed(323)
#x<-runif(n = n,min = 1,max = 20)
# normalfördelning 
#set.seed(765)
#x<-rnorm(n = n,mean = 10.5,sd = 7)

beta_vect<-c(10,2) # första värdet är beta0 och andra är beta1
names(beta_vect)<-c("beta0","beta1")
beta_vect[0]

# en modell utan slump i y (deterministisk)
y<-beta_vect[1]+ beta_vect[2]*x

qplot(x = x,y = y)+theme_bw()

# med normalfördelade fel:
sigma_val<-4
set.seed(244)
y<-beta_vect[1]+ beta_vect[2]*x+rnorm(n = n,mean = 0,sd = sigma_val)
qplot(x = x,y = y)+theme_bw()
# skattade parameterar
coef(lm(formula = y~x))
# jämför med beta_vect
beta_vect
cor(x,y)


set.seed(356)
y<-40 -3.5*x+rnorm(n = length(y),mean = 0,sd = sigma_val)
qplot(x = x,y = y)+theme_bw()
# vilken typ av samband har vi här?
cor(y,x)


set.seed(356)
y<-20 +0.2*x+rnorm(n = length(y),mean = 0,sd = sigma_val)
qplot(x = x,y = y)+theme_bw()
# vilken typ av samband har vi här?
cor(y,x)


set.seed(3262)
y<- 20 +0*x+rnorm(n = length(y),mean = 0,sd = sigma_val)
qplot(x = x,y = y)+theme_bw()
# vilken typ av samband har vi här?
cor(y,x)



#-------------------------------------------------------------------------------
# Olika värden på sigma
#-------------------------------------------------------------------------------
rm(y)
sigma_val1<-1
set.seed(3262)
y1<- 20 +2*x+rnorm(n = n,mean = 0,sd = sigma_val1)
qplot(x = x,y = y1)+theme_bw()
# vilken typ av samband har vi här? Syns det?
# vi ser ett mycket starkt positivt linjärt samband
cor(y1,x)
qplot(x = x,y = y1)+theme_bw()+geom_smooth(method="lm",se=FALSE)



sigma_val2<-5
set.seed(3262+1)
y2<- 20 +2*x+rnorm(n = n,mean = 0,sd = sigma_val2)
qplot(x = x,y = y2)+theme_bw()
# vilken typ av samband har vi här? Syns det?
# vad händer när vi ökar sigma?
cor(y2,x)


sigma_val3<-10
set.seed(3262+2)
y3<- 20 +2*x+rnorm(n = n,mean = 0,sd = sigma_val3)
qplot(x = x,y = y3)+theme_bw()
# vilken typ av samband har vi här? Syns det?
# vad händer när vi ökar sigma?
cor(y3,x)

sigma_val4<-20
set.seed(3262+3)
y4<- 20 +2*x+rnorm(n = n,mean = 0,sd = sigma_val4)
qplot(x = x,y = y4)+theme_bw()
# vilken typ av samband har vi här? Syns det?
# vad händer när vi ökar sigma?
cor(y4,x)


sigma_val5<-50
set.seed(3262+4)
y5<- 20 +2*x+rnorm(n = n,mean = 0,sd = sigma_val5)
qplot(x = x,y = y5)+theme_bw()
# vilken typ av samband har vi här? Syns det?
# vad händer när vi ökar sigma?
cor(y5,x)

sigma_val6<-100
set.seed(3262+5)
y6<- 20 +2*x+rnorm(n = n,mean = 0,sd = sigma_val6)
qplot(x = x,y = y6)+theme_bw()
# vilken typ av samband har vi här? Syns det?
# vad händer när vi ökar sigma?
cor(y6,x)
qplot(x = x,y = y6)+theme_bw()+geom_smooth(method="lm",se=FALSE)
# här är det svårt att se, men det finns en positiv trend i data

sigma_val7<-200
set.seed(3262+6)
y7<- 20 +2*x+rnorm(n = n,mean = 0,sd = sigma_val7)
qplot(x = x,y = y7)+theme_bw()
# vilken typ av samband har vi här? Syns det?
# vad händer när vi ökar sigma?
cor(y7,x)
qplot(x = x,y = y7)+theme_bw()+geom_smooth(method="lm",se=FALSE)




# sätt ihop plottar:
p1<-qplot(x = x,y = y1)+theme_bw()+ggtitle(paste("Sigma =",sigma_val1))
p2<-qplot(x = x,y = y2)+theme_bw()+ggtitle(paste("Sigma =",sigma_val2))
p3<-qplot(x = x,y = y3)+theme_bw()+ggtitle(paste("Sigma =",sigma_val3))
p4<-qplot(x = x,y = y4)+theme_bw()+ggtitle(paste("Sigma =",sigma_val4))
p5<-qplot(x = x,y = y5)+theme_bw()+ggtitle(paste("Sigma =",sigma_val5))
p6<-qplot(x = x,y = y6)+theme_bw()+ggtitle(paste("Sigma =",sigma_val6))
p7<-qplot(x = x,y = y7)+theme_bw()+ggtitle(paste("Sigma =",sigma_val7))
plot_grid(p1,p2,p3,p4,p5,p6,p7)
# vad händer med sambandet när sigma blir större?








#-------------------------------------------------------------------------------
# Olika seed
#-------------------------------------------------------------------------------
sigma_val<-10
y1<- 20 +2*x+rnorm(n = length(x),mean = 0,sd = sigma_val)
p1<-qplot(x = x,y = y1)+theme_bw()
# vilken typ av samband har vi här? Syns det?
# vad händer när vi ökar sigma?
cor(y1,x)


y2<- 20 +2*x+rnorm(n = length(x),mean = 0,sd = sigma_val)
p2<-qplot(x = x,y = y2)+theme_bw()
# vilken typ av samband har vi här? Syns det?
# vad händer när vi ökar sigma?
cor(y2,x)


y3<- 20 +2*x+rnorm(n = length(x),mean = 0,sd = sigma_val)
p3<-qplot(x = x,y = y3)+theme_bw()
# vilken typ av samband har vi här? Syns det?
# vad händer när vi ökar sigma?
cor(y3,x)


plot_grid(p1,p2,p3,nrow = 3)


#-------------------------------------------------------------------------------
# Vad händer med medelfelet?
#-------------------------------------------------------------------------------

set.seed(568)
x<-runif(n = 50,min = 0,max = 20)


# ändra sigma
sigma_val<-1
set.seed(76)
y1<- 20 +0.5*x+rnorm(n = length(x),mean = 0,sd = sigma_val)
qplot(x = x,y = y1)+theme_bw()

lm1<-lm(y1~x)
summary(lm1)

sigma_val<-10
set.seed(88)
y2<- 20 +0.5*x+rnorm(n = length(x),mean = 0,sd = sigma_val)
qplot(x = x,y = y2)+theme_bw()
lm2<-lm(y2~x)
summary(lm2)

# vad händer med medelfelet, t-värdet och p-värdet när sigma_val ökar?
# testa att öka sigma_val ännu mer för y2.
summary(lm1)$coef
summary(lm2)$coef


# nu testar vi olika antal observationer men lika mycket brus:
set.seed(568)
x1<-runif(n = 20,min = 0,max = 20)
x2<-runif(n = 200,min = 0,max = 20)

sigma_val<-8
set.seed(76)
y1<- 20 +0.2*x1+rnorm(n = length(x1),mean = 0,sd = sigma_val)
set.seed(76)
y2<- 20 +0.2*x2+rnorm(n = length(x2),mean = 0,sd = sigma_val)
qplot(x = x1,y = y1)+theme_bw()
qplot(x = x2,y = y2)+theme_bw()
lm1<-lm(y1~x1)
lm2<-lm(y2~x2)
summary(lm1)
summary(lm2)
# vad händer med medelfelet, t-värdet och p-värdet när antalet obs ökar?
summary(lm1)$coef
summary(lm2)$coef

# undersök formlerna i boken/slides för att förstå hur sigma och antal obs
# påverkar medelfelet (och då även olika intervallskattningar)




