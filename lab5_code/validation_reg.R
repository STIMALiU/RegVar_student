#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Intern och extern validering av linjära modeller
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


# vid extern validering så är det vanligt att vi delar upp data i en 
# träningsmängd ( = träningsdata) och en valideringsmängd ( = valideringsdata)
# Detta gör vi slumpmässigt, det förutsätter att vi har oberoende observationer
# i vårt dataset. Har vi inte det så behöver vi använda speciella metoder för
# att skapa valideringdata, tex i tidseriedata.


# Ofta väljer vi att träningsdata ska vara minst 50 % av all vår data
# Valideringsdata brukar ofta vara på 50 % till 10 % av all vår data.
# Det finns ingen "rätt andel" valideringsdata som passar alla situationer
# Har vi många obs så brukar vi ofta låta valideringsdata utgöra en mindre
# andel.


# Detta för enkelt genom att skapa en slumpmässig indexvektor 

# vi utgår från iris-data
?iris
head(iris)
cor(iris[,-5])
plot(iris[,-5])
nrow(iris)

# vi väljer att träningsdata ska vara 2/3 av alla obs
nrow(iris)*(2/3)
set.seed(355)  # sätt en seed för replikerbarhet.
train_index<-sample(x = nrow(iris),size = 100,replace = FALSE)
head(train_index)

data_train<-iris[train_index,]
data_valid<-iris[-train_index,]
nrow(data_train)
nrow(data_valid)

head(data_train)
# vi kan manuellt välja ett antal modeller 
# y = Petal.Width

model1<-lm(Petal.Width~Petal.Length,data=data_train)
model2<-lm(Petal.Width~Petal.Length+Sepal.Length,data=data_train)
model3<-lm(Petal.Width~Petal.Length+Sepal.Length+Sepal.Width,data=data_train)
model4<-lm(Petal.Width~(.)^3,data=data_train[,-5])
?formula

# Vi kan beräkna olika mått
AIC(model1)
AIC(model2)
AIC(model3)
AIC(model4)

BIC(model1)
BIC(model2)
BIC(model3)
BIC(model4)

AIC_vect<-c(AIC(model1),AIC(model2),AIC(model3),AIC(model4))
BIC_vect<-c(BIC(model1),BIC(model2),BIC(model3),BIC(model4))

plot(1:4,AIC_vect)  # model4 är bäst här
plot(1:4,BIC_vect)  # model3 är bäst här

?AIC
?extractAIC

# Prediktioner: MSPR

# Predikationer på träningsdata:

# ta reda på hur vad som händer i beräkningen nedan
MSPR_train1<-mean((residuals(model1))^2)


MSPR_train2<-mean((residuals(model2))^2)
MSPR_train3<-mean((residuals(model3))^2)
MSPR_train4<-mean((residuals(model4))^2)

# prediktioner för valideringsdata
y_hat_valid1<-predict(object = model1,newdata = data_valid)
MSPR_valid1<-mean((data_valid$Petal.Width-y_hat_valid1)^2)

y_hat_valid2<-predict(object = model2,newdata = data_valid)
MSPR_valid2<-mean((data_valid$Petal.Width-y_hat_valid2)^2)

y_hat_valid3<-predict(object = model3,newdata = data_valid)
MSPR_valid3<-mean((data_valid$Petal.Width-y_hat_valid3)^2)

y_hat_valid4<-predict(object = model4,newdata = data_valid)
MSPR_valid4<-mean((data_valid$Petal.Width-y_hat_valid4)^2)

# jämför värden
df_compare<-data.frame(model=1:4,MSPR_train=c(MSPR_train1,MSPR_train2,MSPR_train3,MSPR_train4),
           MSPR_valid=c(MSPR_valid1,MSPR_valid2,MSPR_valid3,MSPR_valid4))

df_compare
# model4 har lägst MSPR på träningsdata
# model4 har lägst MSPR på valideringsdata, detta gör denna modell troligen 
# kommer att generalisera bäst till ny liknande data
# Notera att det är liten skillnad mellan model3 och model4 på valideringsdata
# de har nästan samma fel, men model3 är betydligt enklare än model4


#-------------------------------------------------------------------------------
# PRESS
#-------------------------------------------------------------------------------

# Gå igenom koden här: https://www.statology.org/press-statistic/
# Se även PRESS() i pakten qpcR och MPV, 
?MPV::PRESS

# vad mäter PRESS?

#-------------------------------------------------------------------------------
# Hitta rätt polynom
#-------------------------------------------------------------------------------

# simulera data
n<-200
sd<-5
set.seed(864)
x<-sort(runif(n = n,min = -5,5))
x<-scale(x)
y_true<- 3 +6*x+2*x^2 -4*x^3
y<- y_true+rnorm(n = n,sd = sd)
plot(x,y)
lines(x,y_true)

# olika mått
max_grad<-10
AIC_vect<-rep(0,max_grad)
BIC_vect<-rep(0,max_grad)
PRESS_vect<-rep(0,max_grad)
for(i in 1:max_grad){
  model_temp<-lm(y~poly(x,degree = i,raw = TRUE))
  AIC_vect[i]<-AIC(model_temp)
  BIC_vect[i]<-BIC(model_temp)
  PRESS_vect[i]<-MPV::PRESS(model_temp)
}

# hur ska man tolka dessa figurer?
par(mfrow=c(3,1))
plot(1:max_grad,AIC_vect,t="l")
plot(1:max_grad,BIC_vect,t="l")
plot(1:max_grad,PRESS_vect,t="l")
par(mfrow=c(1,1))

# vilket grad av polynom ska vi välja?
which.min(AIC_vect) # grad 5
which.min(BIC_vect) # grad 3
which.min(PRESS_vect) # grad 5


# ändra i koden ovan:
# testa att ändra sd till 10
# testa att ändra sd till 20

# vad händer om vi har mycket brus?

# testa att ändra n till 400 och sd=5










