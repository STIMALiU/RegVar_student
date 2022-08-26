
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 732G46 Lab 1
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

rm(list=ls())

#-------------------------------------------------------------------------------
# 1 Trees data
#-------------------------------------------------------------------------------


# 1)
data("trees")
?trees


# 2)
Girth<-trees$Girth
Volume<-trees$Volume

dim(trees)
hist(Girth)
hist(Volume)

summary(Girth)
summary(Volume)
sd(Girth)
sd(Volume)

any(is.na(Girth))
any(is.na(Volume))


# 3) 
library(ggplot2)
qplot(x = trees$Girth,y = trees$Volume)+theme_bw()+xlab("Diameter")+ylab("Volym")



# 4) tolka plotten
# vilket samband ser vi?

# 5)
cor(Volume,Girth)

# 6) 
qplot(x = trees$Girth,y = trees$Volume)+theme_bw()+xlab("Diameter")+
  ylab("Volym")+geom_smooth(method=lm,se=FALSE)



# 8) 

reg_object<-lm(formula=Volume~Girth,data=trees)
print(reg_object)

# 9)
beta_trees<-coef(reg_object)
beta_trees
# anpassade värden:
y_hat<-beta_trees[1]+beta_trees[2]*trees$Girth
# residualer
res_volume<- trees$Volume-y_hat
# skatta sigma:
n<-nrow(trees)
SSE<-sum(res_volume^2)
sigma_hat<-sqrt(SSE/(n-2))
sigma_hat

# 11)
summary(reg_object)

# 13
hist(res_volume,20)
boxplot(res_volume)
summary(res_volume)
mean(res_volume)
round(mean(res_volume),4)


# 14
source("/home/joswi05/Dropbox/Josef/732G46_HT2021/732G46_RV/2022/code/lm_diagnostics.R")

lm_diagnostics(lm_obj = reg_object,binwidth = 1)


# 17 
?plot.lm
plot(reg_object) # tryck enter i konsolen för att byta plot

plot(reg_object,which = c(1))
plot(reg_object,which = c(2))
plot(reg_object,which = c(3))
plot(reg_object,which = c(4))
plot(reg_object,which = c(5))
plot(reg_object,which = c(6))



#-------------------------------------------------------------------------------
# 2.1 Använda lm()
#-------------------------------------------------------------------------------


reg_object<-lm(formula=Volume~Girth,data=trees)

?lm # kolla under Value
str(reg_object)

reg_object$coefficients
reg_object$fitted.values

coef(reg_object)

fitted(reg_object)

residuals(reg_object)

reg_object_sum<-summary(reg_object)
print(reg_object_sum)
str(reg_object_sum)
?summary.lm

# här kan vi få ut regressionskoefficienter, men också medelfel och t-värden
reg_object_sum$coefficients
reg_object_sum$coefficients[,1]
reg_object_sum$coefficients[,2]

reg_object_sum$coefficients[,3:4]


anova(reg_object)

# anpassade värden för observerade x-värden
predict(reg_object)

summary(Girth)

# anpassade värden för x= 15.2
predict(reg_object,newdata = data.frame(Girth=15.5))

# anpassade värden för x= 10 till 15
predict(reg_object,newdata = data.frame(Girth=10:15))

# intervall för y_hat:
predict(reg_object,newdata = data.frame(Girth=15.5),interval = "conf")
predict(reg_object,newdata = data.frame(Girth=15.5),interval = "pred")
predict(reg_object,newdata = data.frame(Girth=15.5),interval = "conf",level = 0.99)

predict(reg_object,newdata = data.frame(Girth=10:15),interval = "pred",level = 0.9)

predict(reg_object,interval = "pred",level = 0.9)


# konfidensintervall för beta:
confint(reg_object)
confint(reg_object,level = 0.99)

confint(reg_object,level = 0.99,parm = "Girth")

# konfidensintervall för y_hat med ggplot2:
qplot(x = trees$Girth,y = trees$Volume)+theme_bw()+xlab("Diameter")+
  ylab("Volym")+geom_smooth(method=lm,se=TRUE,level=0.9)

# se även: https://rpubs.com/Bio-Geek/71339


