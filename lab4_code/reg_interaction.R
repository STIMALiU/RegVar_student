#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Interaktion i linjär regression
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Interaktion mellan en kategorisk och en numerisk variabel
#-------------------------------------------------------------------------------


# simulera lite data

# varje obs är en anställd på ett företag
n_obs<-300
set.seed(7334)
years_employed<-sample(x = 10,size = n_obs,replace = TRUE)
workplace<-sample(x = c("factory","office"),size = n_obs,replace = TRUE,prob = c(0.65,0.35))
hist(years_employed)
table(workplace)

# om bara tiden som anställd påverkade lönen:
salary<- 20 + 2*years_employed + rnorm(n = n_obs)  # lön i tusentals kronor
plot(years_employed,salary)
beta<-coef(lm(salary~years_employed))
beta
plot(years_employed,salary)
abline(a = beta[1],b = beta[2],col="blue",lwd=3)


# om tiden som anställd och vilken arbetsplats påverkade lönen 

workplace2<-ifelse(test = workplace=="office",yes = 1,no = 0)
salary<- 20 + 2*years_employed +7*workplace2 + rnorm(n = n_obs)
plot(years_employed,salary)
plot(years_employed,salary,col=workplace2+1)  # röd = office

# notera att det är vi vill ha en regressionslinje för varje grupp
# detta kan vi få gneom att ha med workplace som en kategorisk variabel i 
# regressionen

lm2<-lm(salary~years_employed+workplace2)
lm2
summary(lm2)
beta2<-coef(lm2)
# fallet workplace2==1
y_hat1<-beta2[1]+beta2[2]*sort(years_employed[workplace2==1])+beta2[3]
# fallet workplace2==0
y_hat2<-beta2[1]+beta2[2]*sort(years_employed[workplace2==0])
plot(years_employed,salary,col=workplace2+1)  # röd = office
lines(sort(years_employed[workplace2==1]),y_hat1,col="red",lwd=3)
lines(sort(years_employed[workplace2==0]),y_hat2,col="black",lwd=3)
# vi får två linjer, en för varje grupp. Linjerna har samma lutning men har 
# olika intercept 


# om tiden som anställd och vilken arbetsplats påverkade lönen,
# och det finns en interaktion mellan arbetsplats och tiden som anställd

workplace2<-ifelse(test = workplace=="office",yes = 1,no = 0)
salary<- 20 + 1.5*years_employed +10*workplace2 + 1.5*workplace2*years_employed + rnorm(n = n_obs)
plot(years_employed,salary)
plot(years_employed,salary,col=workplace2+1)  # röd = office



lm3<-lm(salary~years_employed+workplace2+workplace2*years_employed)
lm3
summary(lm3)
beta3<-coef(lm3)
# fallet workplace2==1
y_hat3<-beta3[1]+(beta3[2]+beta3[4])*sort(years_employed[workplace2==1])+beta3[3]
# fallet workplace2==0
y_hat4<-beta3[1]+beta3[2]*sort(years_employed[workplace2==0])
plot(years_employed,salary,col=workplace2+1)  # röd = office
lines(sort(years_employed[workplace2==1]),y_hat3,col="red",lwd=3)
lines(sort(years_employed[workplace2==0]),y_hat4,col="black",lwd=3)
# Vi får två linjer, en för varje grupp. 
# Linjerna har olika lutning och olika intercept 

source("https://raw.githubusercontent.com/STIMALiU/RegVar_student/main/general_code/lm_diagnostics.R")
lm_diagnostics(lm_obj = lm3)
# ser bra ut

# kollar residualer på felaktiga modeller för detta data

# utan interaktion
lm_diagnostics(lm_obj = lm(salary~years_employed+workplace2))
# ser inte bra ut!

# utan interaktion och utan years_employed
lm_diagnostics(lm_obj = lm(salary~years_employed))
# ser inte alls bra ut!
plot(years_employed,salary,col=workplace2+1)  # röd = office
beta_wrong<-coef(lm(salary~years_employed))
abline(a = beta_wrong[1],b = beta_wrong[2])
# inte alls en bra anpassning!

#-------------------------------------------------------------------------------
# Interaktion mellan två numeriska variabler
#-------------------------------------------------------------------------------
# 
n_obs<-200
x_min<-0
set.seed(64)
x1<-runif(n = n_obs,min = x_min,max = 5)
x2<-rnorm(n = n_obs,mean=0,sd = 3)

# 
x1_x2<-x1*x2
set.seed(465)
y<-10 +1.5*x1-1.5*x2-3*x1_x2+rnorm(n = n_obs,sd = 1)
library(GGally)
df<-data.frame(x1=x1,x2=x2,x1_x2=x1_x2,y=y)
ggpairs(data = df)
plot(df)

# felaktig modell utan interaktion
summary(lm(y~x1+x2))
# sanna beta-värden: 10, 1.5, -1.5, -3 (=interaktionen)
coef(lm(y~x1+x2))
# värden är inte nära de sanna (förutom intercept), då modellen för felspecificerad
source("https://raw.githubusercontent.com/STIMALiU/RegVar_student/main/general_code/lm_diagnostics.R")
lm_diagnostics(lm_obj = lm(y~x1+x2))
# ser inte bra ur här

summary(lm(y~x1+x2+x1_x2))
coef(lm(y~x1+x2+x1_x2))
# nu är värdena nära de sanna när vi valt korrekt modell
lm_diagnostics(lm_obj = lm(y~x1+x2+x1_x2))
# ser mycket bättre ut










