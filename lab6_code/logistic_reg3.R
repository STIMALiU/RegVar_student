################################################################################
################################################################################
# Utvärdering logistisk regression
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
# Extern validering
################################################################################

n_obs_half<-150
set.seed(537)
y<-c(rep(0,n_obs_half),rep(1,n_obs_half))
x<-c(rnorm(n_obs_half),rnorm(n_obs_half,mean = 2)) 
# ökar vi värdet på mean här så blir det ett enklare problem


# library
library(ggplot2)
library(dplyr)

# Build dataset with different distributions
df1 <- data.frame(x=x,y=as.factor(y))

# Represent it
p <- df1 %>%
  ggplot( aes(x=x, fill=y)) +
  geom_histogram( color="#e9ecef", alpha=0.5, position = 'identity') +
  scale_fill_manual(values=c("red", "blue")) +
  labs(fill="")
p
# lila färg betyder att histogrammen överlappar
# ju mer separerade klasserna är desto lättare klassificeringsproblem 


################################################################################
# dela upp i träning och validering

set.seed(909)
train_index<-sample(x = n_obs_half*2,size = n_obs_half,replace = FALSE)
length(train_index)

x_train<-x[train_index]
y_train<-y[train_index]
x_valid<-x[-train_index]
y_vaild<-y[-train_index]

# kolla klasser i båda data:
table(y_train)
table(y_vaild)


################################################################################
# Skatta modell
A1<-glm(formula = y~x,family = "binomial")
A1
A1_sum<-summary(A1)
A1_sum
library(stringr)
source("https://raw.githubusercontent.com/STIMALiU/RegVar_student/main/general_code/class_evaluation.R")

# träningsdata:
class_evaluation(new_data = data.frame(x=x_train),model = A1,true_y = y_train)

# validering
class_evaluation(new_data = data.frame(x=x_valid),model = A1,true_y = y_vaild)




################################################################################
# 2D-data
################################################################################
path<-"https://raw.githubusercontent.com/STIMALiU/RegVar_student/main/data/2d_dataset.csv"
D<-read.csv(file =path)
head(D)
D$class<-as.factor(D$class)
dim(D)
table(D$class)

ggplot(data = D,aes(x=x1,y=x2))+geom_point(aes(col=class),size=3)

# dela upp träning (50 %) och validering (50 %)
# anpassa en modell på träning med x1 och x2 som variabler, och class som y.


# ta fram utvärderingsmått på träning och validering 

# ta fram en scatter plot där ni färglägger om punkteerna är rätt klassificerade eller inte
# lägg in den linjära beslutsgränsen
# gör detta för träning och validering
# funkar modellen bra?


# försök att förbättra modellen med polynomtermer, interaktioner eller 
# andra transformationer 


