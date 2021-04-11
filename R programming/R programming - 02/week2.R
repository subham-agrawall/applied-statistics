#install.packages("knitr")
#library(knitr)
#install.packages("rmarkdown")
#library(rmarkdown)

a<-11
b<-19

if(a<b){
  print("I like Mathematics.")
  print("I like Statistics.")
}

x<-seq(1,30,by=0.5) 
if(length(x>30)){
  print("bleh")
}else{
  print("bleh bleh")
}
  
switch(1, "hello", "hola")
switch("gre", "area"=1, "ball"=3, "gre"="pass")

v<-c("India", "is", "great!")
s=12
while(s<20){
  print(v[3])
  s=s+2
}

repeat{
  print("India")
  s=s+2
  if(s>50){
    break
  }
}

for(i in 1:6){
  x=i*i
  print(x)
}

add_fun=function(x,y){
  addition=x+y
  print(addition)
}

add_fun(2,3)

factorial_fun=function(x){
  fac=1
  for(i in 1:x){
    fac <- i*fac
  }
  print(fac)
}
factorial_fun(3)

fibonacci = function(x){
  f = c(0,1)
  for(i in 2:x){
    a = f[i-2]+f[i-1]
    f=c(f,a)
  }
  print(f)
}
fibonacci(5)

##Pie chart
x = c(21, 62, 10, 53)
label= c("Delhi", "Chennai", "Hyderabad", "Bombay")
pie(x, labels=label)
png(file = "city_title_colours.jpg")

pie(x, labels, main="City pie chart", col = rainbow(length(x)))
dev.off()

piepercent <- round(100*x/sum(x), 1)
png(file = "city_percentage_legends.jpg")
pie(x, labels= piepercent, main="City pie chart", col = rainbow(length(x)), legend("topright", ))

install.packages("plotrix")
library(plotrix)
x = c(21, 62, 10, 53)
lab= c("Delhi", "Chennai", "Hyderabad", "Bombay")
pie3D(x,labels =lab, explode = 0.1, main = "Pie Chart of Cities")
##

##BAR CHART
b = c(17,10,15,23,29)
month = c("mar", "apr", "may", "jun", "jul")
barplot(b, names.arg = month)
##

##SCATTER PLOT
gridx = seq(0,1,length=50)
fx = sin(2*pi*gridx)
plot(x=gridx, y=fx, "l", main="Scatter Plot for sin(2*pi*x)", col="Red")
plot(fx ~ gridx)
##

##BOX PLOT
library(MASS)
head(Pima.tr)
BP <- Pima.tr$bp
skin <- Pima.tr$skin
boxplot(BP~Pima.tr$type)
##

##Quantile
quantile(skin, probs=c(0,0.25,0.5,0.75,1))
##

##HISTOGRAM
x = c(34,23,6,7,31,12,9,27,31,5,8,1,34,27,29,39,38,40,0)
hist(x, border="red", col="blue", breaks=15)
##

##MEAN
mean(x)
y = c(1,2,3,4,5,6,7,8,9)
mean(y, trim=0.2)
#median
#mode function

pnorm(85, mean=70, sd=15.2, lower.tail = FALSE)
pnorm(45, mean=70, sd=15.2, lower.tail = T)
