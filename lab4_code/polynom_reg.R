
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Polynomregression
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


# vi kan modellera icke-linjära samband mellan x och y genom att göra 
# lämpliga transformationer av x, ett sådant exempel är polynomtermer

# simulera data:

set.seed(2323)
n_obs<-100
x<-runif(n = n_obs,min = -5,max = 15)
y<-4-1*x+0.2*x^2 +rnorm(n = n_obs) 

A<-lm(y~x)

source("https://raw.githubusercontent.com/STIMALiU/RegVar_student/main/general_code/lm_diagnostics.R")
lm_diagnostics(lm_obj = A) # ser inte bra ut...


plot(x,y)
cor(x,y)

# vi kan anpassa polynom-modeller på olika sätt

df1<-data.frame(y=y,x=x,x2=x^2)


lm(y~.,data=df1)

# vi vill ofta centrera eller standardisera variabler som vi 
# skapar polynomtermer av

?scale
df2<-df1
df2[,-1]<-scale(df1[,-1],center = TRUE,scale = FALSE)
#X2<-scale(X,center = TRUE,scale = FALSE) # vad är skillnaden?
colMeans(df1)
colMeans(df2)
head(df2)

lm(y~.,data=df2)

# alt:

lm(y ~ x + I(x^2))


# alt:
?poly
X1<-poly(x = x,degree = 2,raw = TRUE)
head(X1)
colMeans(X1)
cor(X1[,1],X1[,2])

X2<-poly(x = scale(x,scale = FALSE),degree = 2,raw = TRUE)
head(X2)
cor(X2[,1],X2[,2])

plot(X2[,1],X2[,2])

summary(lm(y~.,data=data.frame(y=y,X2)))

B<-lm(y~.,data=data.frame(y=y,X2))
lm_diagnostics(lm_obj = B)  # ser bättre ut

# Gå igenom koden här:
# https://datascienceplus.com/fitting-polynomial-regression-r/