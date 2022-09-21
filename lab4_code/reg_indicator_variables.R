

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Indikatorvariabler i regressionsmodeller
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


# om vi ha en kategorisk variabel som vi vill använda som förklarande variabel
# så måste vi koda den som en eller flera binära variabler (0/1) innan den
# används i en regressionmodell 

# detta kan göras på olika sätt i R.
# Det är viktigt att vi har koll på vad 1 och 0 betyder i vår kodning av 
# variablerna, eftersom det påverkar vår tolkning av våra parameterskattningar



#-------------------------------------------------------------------------------
# exempel med kategorisk variabel med två nivåer 
#-------------------------------------------------------------------------------
set.seed(34)
x1<-sample(x = c("A","B"),size = 10,replace = TRUE,prob = c(0.3,0.7))
table(x1)

# koda om
ifelse(test = x1=="B",yes = 1,no = 0)
ifelse(test = x1=="A",yes = 1,no = 0)

# alt
x2<-rep(0,10)
x2[x1=="A"]<-1
table(x2)


y<-10+3*x2+rnorm(n = 10)
lm(y~x2)
fitted(lm(y~x2))
plot(x2,y)



#-------------------------------------------------------------------------------
# exempel med kategorisk variabel med tre nivåer 
#-------------------------------------------------------------------------------

# har vi fler än två nivåer så behöver vi antal nivåer - 1 indikatorvariabler
# för att reporesentera variabeln, som om vi har tre nivåer behöver vi
# två nya binära variabler


set.seed(365)
x1<-sample(x = c("A","B","C"),size = 50,replace = TRUE,prob = c(0.2,0.3,0.5))
table(x1)

# koda om
x1_A<-ifelse(test = x1=="A",yes = 1,no = 0)
x1_B<-ifelse(test = x1=="B",yes = 1,no = 0)

# 
y<-30+5*x1_A -3*x1_B +rnorm(n = 50)
plot(as.factor(x1),y)

lm(y~x1_A+x1_B)




# alt: konvertera  x1 till en factor:
x1<-as.factor(x1)
lm(y~x1)
# nu sker omkodningen i bakgrunden i lm()


# ändra referenskategorin
x2<-relevel(x1,ref = "C")

lm(y~x2)


#-------------------------------------------------------------------------------
# model.matrix()
#-------------------------------------------------------------------------------

X<-data.frame(x=x1)
A<-model.matrix(object = ~.,data = X)
head(A,10)



