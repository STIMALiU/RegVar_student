library(rms) 
?ols
model1 <- ols(formula = y~.,data = my_data) 

?fastbw
# "fastbw deletes factors, not columns of the design matrix. 
# Factors requiring multiple d.f. will be retained or dropped as a group."

# with p-values, sls = significance level for staying in a model
model2 <- fastbw(fit=model1, rule="p", sls=0.1,type = "individual") 

# with aic
model3 <- fastbw(fit=model1, rule="aic",type = "individual") 

print(model1)
print(model2) 
print(model3)
str(model2)
str(model3)
