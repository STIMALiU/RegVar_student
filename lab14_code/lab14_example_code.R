
# baseras på: https://www.statology.org/three-way-anova-in-r/


df <- data.frame(program=rep(c(1, 2), each=20),
                 gender=rep(c('M', 'F'), each=10, times=2),
                 division=rep(c(1, 2), each=5, times=4),
                 height=c(7, 7, 8, 8, 7, 6, 6, 5, 6, 5,
                          5, 5, 4, 5, 4, 3, 3, 4, 3, 3,
                          6, 6, 5, 4, 5, 4, 5, 4, 4, 3,
                          2, 2, 1, 4, 4, 2, 1, 1, 2, 1)) 

df$program<-as.factor(df$program)
df$gender<-as.factor(df$gender)
df$division<-as.factor(df$division)

library(dplyr)

# vi har tre faktorer: (alla har två nivåer)
# 1: program
# 2: gender
# 3: division


# beräkna cellmedelvärden
df %>%
  group_by(program, gender, division) %>%
  summarize(mean_height = mean(height))



# beräkna mu_i..
df %>%
  group_by(program) %>%
  summarize(mean_height = mean(height))
# så vi har:
# mu_1.. = 5.2
# mu_2.. = 3.3

# beräkna mu_.j.
df %>%
  group_by(gender) %>%
  summarize(mean_height = mean(height))
# så vi har:
# mu_.1. = 2.95
# mu_.2. = 5.55


# beräkna mu_..k
df %>%
  group_by(division) %>%
  summarize(mean_height = mean(height))
# så vi har:
# mu_..1 = 4.95
# mu_..2 = 3.55


# alt:

model1<-aov(formula = height~(.)^3,data = df)
# ger samma som ovan:
model2<-aov(formula = height~program*gender*division,data = df)
anova(model1)==anova(model2)

anova(model1)
model.tables(model1,type = "effects")

model.tables(model1,type = "mean") # här får vi fram olika typer av medelvärden

# jämför med
df %>%
  group_by(division) %>%
  summarize(mean_height = mean(height))
tab1<-model.tables(model1,type = "mean")
tab1$tables$division
