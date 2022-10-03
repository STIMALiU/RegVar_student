################################################################################
################################################################################
# Multipel linjär logistisk regression
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
# Plotta den logistiska funktionen
################################################################################

n_obs_per_dim<-50
x1<-seq(-5,5,length=n_obs_per_dim)
x2<-seq(-5,5,length=n_obs_per_dim)
# vi tar alla kombinationer av värden i x1 och x2
# detta gör att vi kan göra plottar senare över den logistiska funktionen i 3D
X<-expand.grid(x1,x2) 

dim(X)
# testa att ändra värden på beta_vect1 
beta_vect1<-c(0,1,1) # beta0, beta1 och beta2

z1<-beta_vect1[1] + beta_vect1[2]*X[,1]+beta_vect1[3]*X[,2]
prob_vect1<-logistic_func(z = z1)
hist(prob_vect1)
mean(prob_vect>=0.5)
prob<-prob_vect1
df1<-data.frame(x1=X[,1],x2=X[,2],prob=prob_vect1)
head(df1)
dim(df1)



library(plotly)
# vi kan göra ett ytdiagram för att titta på den logistiska funktionen i två
# dimensioner. Notera att ni kan vrida, zoom mm på plotten med musen 
plot_ly(z = ~xtabs(prob ~ x1 + x2, data = df1)) %>% add_surface()%>% 
  layout(scene = list(xaxis = list(title = 'x1'), yaxis = list(title = 'x2'),
          zaxis = list(title = 'prob')))
# notera x = x1, y = x2 och z = p(y=1) i plotten

# annan varianter:
ggplot(data=df1,aes(x=x1,y=x2))+geom_contour_filled(aes(z=prob))
ggplot(data=df1,aes(x=x1,y=x2))+geom_raster(aes(fill=prob))
ggplot(data=df1,aes(x=x1,y=x2))+geom_point(aes(col=prob))


################################################################################
# Simulera data med flera x
################################################################################

n_obs<-600
x1<-runif(n = n_obs,min = -5,max = 5)
x2<-runif(n = n_obs,min = -5,max = 5)

# test olika värden på beta:
beta_vect2<-c(0,10,10)
#beta_vect2<-c(0,2,2)
#beta_vect2<-c(0,0.8,0.8)
#beta_vect2<-c(0,0.2,0.2)
#beta_vect2<-c(0,-2,2)
#beta_vect2<-c(3,-2,2)
#beta_vect2<-c(1,0.7,0.7)

z2<-beta_vect2[1] + beta_vect2[2]*x1+beta_vect2[3]*x2
y_prob2<-logistic_func(z2)
y<-rbinom(n = n_obs,size = 1,prob = y_prob2)
df2<-data.frame(x1=x1,x2=x2,y=y,class=as.factor(ifelse(y==1,"1","0")))


ggplot(data = df2,aes(x=x1,y=x2))+geom_point(aes(col=class))+theme_bw()+scale_color_manual(values = c("red","blue"))
fig <- plot_ly(data = df2, x = ~x1, y = ~x2, z = ~y, color = ~class, colors = c('#BF382A', '#0C4B8E'),alpha=0.7)%>% 
  add_markers() %>% layout(scene = list(xaxis = list(title = 'x1'),
                                        yaxis = list(title = 'x2'),
                                        zaxis = list(title = 'y')))
# notera att plottar från plotly ligger under "Viewer"
print(fig)
################################################################################
# Skatta modellen
################################################################################

n_obs<-100
set.seed(4768)
x1<-runif(n = n_obs,min = -5,max = 5)
x2<-runif(n = n_obs,min = -5,max = 5)
beta_vect3<-c(3,2,-2)
z3<-beta_vect3[1] + beta_vect3[2]*x1+beta_vect3[3]*x2
y_prob3<-logistic_func(z3)
y3<-rbinom(n = n_obs,size = 1,prob = y_prob3)
df3<-data.frame(x1=x1,x2=x2,y=y3,class=as.factor(ifelse(y3==1,"1","0")))
head(df3)

?formula
?glm
A1<-glm(formula = y~x1+x2,data=df3,family = "binomial")
A1
A1_sum<-summary(A1)
A1_sum

A1_sum$coefficients

A1_sum$family
A1_sum$aic


# anpassade värden
# type = "response" ger skattade sannolikheter
y_hat_prob<-predict(object = A1,type = "response")
# predikterar en obs som 1 om sannolikheten är 0.5 eller högre
y_hat<-ifelse(y_hat_prob>=0.5,1,0)

df3$y_hat_prob<-y_hat_prob
df3$y_hat<-as.factor(y_hat)


# plotta skattade sannolikheter för alla datapunkter:
ggplot(data = df3,aes(x=x1,y=x2))+geom_point(aes(col=y_hat_prob))+theme_bw()

# plotta predikterade värden (0/1)
ggplot(data = df3,aes(x=x1,y=x2))+geom_point(aes(col=y_hat))+theme_bw()+scale_color_manual(values = c("red","blue"))

# kombinera 
ggplot(data = df3,aes(x=x1,y=x2))+geom_point(aes(col=y_hat_prob,shape=y_hat),size=3)+theme_bw()



# type = "link" ger skattade linjära prediktorn 
# (innan vi använder den logistiska funktionen)
z_hat<-predict(object = A1,type = "link")

# vi kan räkna för hand:
z_hat2<-coef(A1)[1]+coef(A1)[2]*x1+coef(A1)[3]*x2
# z_hat och z_hat2 är samma
z_hat-z_hat2


# linjär logistisk regression skapar linjära beslutsgränser
# om vi har två kontinuerlig förklarande variabler så kommer variabelrummet
# som baseras på x1 och x2 delas av en en rät linje 
# där alla punkter under linjen predikteras till en klass och alla punkter
# ovanför linjen predikteras till den andra klassen
# linjen kommer att gå längs den kombination av x1 och x2 värden där vi har
# p(y=1) = 0.5
# linjen ges av 
# x2 = -(beta1/beta2)*x1 -beta0/beta2
# så lutningen ges av -(beta1/beta2) och interceptet ges av -beta0/beta2
# där beta0, beta1 och beta2 är våra skattade parametervärden



# i tre dimensioner så kommer beslutsgränsen att vara ett plan
# i fyra och högre dimensioner så kommer beslutsgränsen att vara ett hyperplan
# (vilket vi inte kan visualisera på något enkelt sätt)

ggplot(data = df3,aes(x=x1,y=x2))+geom_point(aes(col=y_hat_prob,shape=y_hat),size=3)+
  geom_abline(intercept=-coef(A1)[1]/coef(A1)[3], slope=-coef(A1)[2]/coef(A1)[3], linetype="dashed", color="red", size=1,5)+
  theme_bw() 
# i detta fall ser vi att alla punkter över linjen klassifiseras som 0
# och alla punkter under linjen klassificeras som 1
# linjen motsvarar p(y=1)=0.5, vilket är där modellen är som mest osäker på sin 
# prediktion. 


A1_sum
residuals(A1,type = "deviance")
A1_sum$deviance
sum(residuals.glm(A1,type = "deviance")^2)

A1_sum$cov.unscaled # skattning av kovariansmatrisen för beta_hat 
A1_sum$coefficients[,1:2] # SE för beta_hat
sqrt(diag(A1_sum$cov.unscaled))
# kontrollräkna:
A1_sum$coefficients[,2]-sqrt(diag(A1_sum$cov.unscaled))

confint(A1,level = 0.99)

# prediktera ny data
no_valid<-30
set.seed(876)
# ny data:
df_valid<-data.frame(x1=runif(n = no_valid,min = -5,max = 5),x2=runif(n = no_valid,min = -5,max = 5))
qplot(df_valid$x1,df_valid$x2)
dim(df_valid)
# prediktion
y_hat_prob_valid<-predict.glm(A1,newdata = df_valid,type = "response")
df_valid$y_hat<-as.factor(ifelse(y_hat_prob_valid>=0.5,"1","0"))
df_valid$data<-"valid"

df3_temp<-df3[,c(1,2,6)]
df3_temp$data="train"

df3_all<-rbind(df3_temp,df_valid)
dim(df3_all)


ggplot(data = df3_all,aes(x=x1,y=x2))+geom_point(aes(col=y_hat,shape=data),size=3)+
  geom_abline(intercept=-coef(A1)[1]/coef(A1)[3], slope=-coef(A1)[2]/coef(A1)[3], linetype="dashed", color="green", size=1)+
  theme_bw()+scale_color_manual(values = c("red","blue"))
# röd: predikteras till klass 0
# blå: predikteras till klass 1
# Cirkel: träningsdata
# Triangel: valideringsdata
# notera att alla 


################################################################################
# Irisdata
################################################################################

?iris
iris<-iris[1:100,]
head(iris)
summary(iris)
my_data<-iris
str(iris)
my_data$Species
# vi sätter Species=1 om setosa och 0 om versicolor
my_data$Species<-droplevels(my_data$Species)
str(my_data$Species)
# vi sätter y=1 om setosa och y=0 om versicolor
my_data$y<-ifelse(my_data$Species=="setosa",1,0)

# hur många i varje klass?
table(my_data$y)

library(GGally)
plot(my_data[,1:4],col=my_data$y+2)
ggpairs(data = my_data,aes(col=Species))
# grupperna (= vår respons) ser rätt uppdeldae ut -> logistisk regression passar nog bra

head(my_data)
A2<-glm(y~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width ,data=my_data)

A2_sum<-summary(A2)
A2_sum
confint(A2)

