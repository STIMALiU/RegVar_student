

# generera lite data:
set.seed(339)
y<-c(rnorm(10,2,1),rnorm(10,2.5,1),rnorm(10,5,1))
color<-c(rep(1,10),rep(2,10),rep(3,10))

A<-data.frame(y=y,color=color)
A$color<-as.factor(A$color)
head(A)

aggregate(x = A$y,by=list(A$color),FUN=mean)
aggregate(x = A$y,by=list(A$color),FUN=sd)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# plot 1: medelvärde, gruppvisa KI för varje faktor
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

library(Rmisc)
library(plotrix)
?CI()
# kör CI (utan parentes) i terminalen för att se koden
ci_obj<-aggregate(x = A$y,by=list(A$color),FUN=CI,ci=0.9)
str(ci_obj$x)

# se här: https://stackoverflow.com/questions/14069629/how-can-i-plot-data-with-confidence-intervals
plotCI(x =1:3, y = ci_obj$x[,2], ui=ci_obj$x[,1], li=ci_obj$x[,3],ylab="value",xlab="factor",col=c("red","blue","purple"),lwd=2)
abline(h=mean(A$y),lwd=2,lty=3) 

# alt: plotmeans() från gplots-paketet

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# plot 2: main effects
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


plot(x=1:3,y=ci_obj$x[,2],t="o")



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# One-way ANOVA
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

anova_obj<-aov(formula = y~color,data=A)
summary(anova_obj)

#------------------------------------------------------------------------------
# Comparisons and contrasts
#------------------------------------------------------------------------------
# kolla här
# https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html


library(emmeans)

B<- emmeans(anova_obj, specs = "color")
B
?pairs.emmGrid
D<-pairs(B)
D
class(D)
str(D)
# from the link above:
# "In its out-of-the-box configuration, pairs() sets two defaults 
# for summary(): adjust = "tukey" (multiplicity adjustment), 
# and infer = c(FALSE, TRUE) (test statistics, not confidence intervals). 
# You may override these, of course, by calling summary() on the result 
# with different values for these."
?summary.emmGrid
summary(D,adjust = "tukey")
summary(D,adjust = "scheffe")
summary(D,adjust = "bonferroni")
summary(D,adjust = "bonferroni",infer = c(TRUE, FALSE)) # get tests
summary(D,adjust = "bonferroni",infer = c(TRUE, TRUE)) # get both
summary(D,adjust = "bonferroni",infer = c(FALSE, TRUE)) # get only tests

# läs under P-value adjustments
?summary.emmGrid


# another function:
?TukeyHSD
g<-TukeyHSD(x = anova_obj,which = "color",conf.level = 0.99)
print(g)
plot(g)




