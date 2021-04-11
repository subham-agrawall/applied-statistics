## Question1
# Loading the data
adv = read.csv("ques1.csv", header=TRUE)
colnames(adv)=c("AdvertisingCosts","Sales")
adv
# (a)Scatter plot
attach(adv)
plot(Sales~AdvertisingCosts)
# (b)Equation of regression line
# Fit the regression model using the fnction lm():
adv.lm<-lm(Sales~AdvertisingCosts,data= adv)
# Use the function summary() to get results
summary(adv.lm)
# Hence, b0=343.706, b1=3.221
# (c)Estimation for $35
343.706 + 35*3.221
# (d)residuals Vs advertising costs
#create the table of fitted values and residuals
advNew=data.frame(adv,fitted.value=fitted(adv.lm),residual=resid(adv.lm))
advNew
plot(residual~AdvertisingCosts, data=advNew)
# From the plot, we can say that advertising costs do not explain all the variability in Sales.
# There is no pattern which means higher-degree polynomial fit is not required.

## Question-2
# (a)Equation of regression line
# Loading the data
data = read.csv("ques2.csv")
data
attach(data)
plot(shearResistance~normalStress)
# Fit the regression model using the fnction lm():
data.lm<-lm(shearResistance~normalStress,data= data)
# Use the function summary() to get results
summary(data.lm)
# Hence, b0=42.5818, b1=-0.6861
# (b) Estimation for normal stress 24.5
42.5818-0.6861*24.5
