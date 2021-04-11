#ae13b063
#Subham Agrawal
#assignment-2

#library(knitr)
#library(rmarkdown)
library(prob)

#Question-1
S= tosscoin(5, makespace = TRUE)
S

#adding "number of heads" column to S 
headcount <- vector(mode="numeric", length=0) #numeric null vector
for (j in 1:nrow(S)) {
  a=0
  for(i in 1:5){
    if(S[j,i]=="H"){
      a = a + 1
    }
  }
  headcount = c(headcount, a)
}
S$noofheads = headcount
S
event = subset(S, noofheads >= 3) #more heads than tails
Prob(event)

#Question-2
#install.packages("gss")
library(gss)
data(ozone)
summary(ozone)

hist(ozone$ibtp,main = "Histogram of Inversion Base Temperature",border = "red",col = "blue")
hist(ozone$hmdt,main = "Histogram of Humidity",border = "red",col = "blue")

boxplot(ozone$vsty ~ ozone$wdsp, main="BoxPlot")

plot(x=ozone$day,y=ozone$hmdt, main = "Scatter plot for Humidity")
plot(x=ozone$day,y=ozone$vsty, main = "Scatter plot for Visibility", "l")
plot(x=ozone$day,y=ozone$wdsp, main = "Scatter plot for Wind Speed", col="Red")

#humidity and sandburg airbase temperature
hist(ozone$sbtp, main="Histogram for Sandburg airbase temperature", border="red")
plot(x=ozone$sbtp,y=ozone$hmdt, main = "Scatter plot for Humidity and Sandburg Airbase Temp")

#Question-3
conc <- c( 5, 18, 15, 7, 23, 220, 130, 85, 103, 25, 80, 7, 24, 6, 13, 65, 37, 25,24, 65, 82, 95, 77, 15, 70, 110, 44, 28, 33, 81, 29, 14, 45, 92, 17, 53)
hist(conc, main="Histogram for average particulate concentration", border="red", col="blue")
#From the below graph, we can see that histogram is approximately normal
hist(conc, main="Histogram for average particulate concentration", border="red", col="blue", breaks = 10)

#Question-4
#Label 1-5:defective, 6-15:partially defective, 16-40:Acceptable transistors
S = urnsamples(1:40, size=1)
p = rep(1, times=nrow(S))
S = probspace(S, probs = p) #equally likely
#EventA : does not fail immediately
A = subset(S, out>5)
#EventB : acceptable
B = subset(S, out>15)
Prob(B, given = A) #Answer

#Question-5
x <-seq(-5,5,.01) 
densities <-dnorm(x, 0,1) #density calculation
plot(x, densities, col="darkgreen",xlab="", ylab="Density", type="l", main="PDF of Standard Normal") #Plot

#Question-6
set.seed(100)
#Part1(Uniform Law)
vectors <- matrix(ncol = 12, nrow = 1000)
for(i in 1:1000){
  vec = runif(12,0,1)
  vectors[i,]=vec
}
hist(vectors[3,],main = "Histogram of generated values",border = "red",col = "blue") #hist of generated values
sampleMean = apply(vectors,1,mean) 
hist(sampleMean,main = "Histogram of Sample Mean",border = "red",col = "blue")

#Part-2(Exponential Law)
vectors2 <- matrix(ncol = 12, nrow = 1000)
for(i in 1:1000){
  vec = rexp(12,10)
  vectors2[i,]=vec
}
hist(vectors2[4,],main = "Histogram of generated values",border = "red",col = "blue")
hist(vectors2[10,],main = "Histogram of generated values",border = "red",col = "blue")
sampleMean2 = apply(vectors2,1,mean)
hist(sampleMean2,main = "Histogram of Sample Mean",border = "red",col = "blue")

##Exercise Problems from lab session-3
#Exercise-1
S = rolldie(2, makespace = TRUE)
S
A = subset(S, X1 == X2) #outcomes match
A
B = subset(S, X1 + X2 >= 8) #sum of outcomes atleast 8
B
Prob(A, given = B)
Prob(B, given = A)

#Exercise-2
x1 = c("A",2:10,"J","Q","K")
x1
x1 = rep(x1, 4) #as we have 4 suits
S = urnsamples(x1, size=2, replace = FALSE, ordered = TRUE)
p = rep(1, times=nrow(S))
S = probspace(S, probs = p) #equally likely
A = subset(S, X1=="A")
B = subset(S, X2=="A")
S2 = subset(S, X1=="A"& X2=="A") #both aces
Prob(S2)

#Exercise-3
#Label balls 1-7: Red, balls 8-10: Green
S = urnsamples(1:10, size=3, replace=FALSE, ordered=TRUE)
p = rep(1, times=nrow(S))
S = probspace(S, probs = p) #equally likely

A= subset(S, X1<8 & X2<8 & X3<8) #all 3 balls are red
Prob(A)

B= subset(S, X1<8 & X2<8 & X3>7| X1<8 & X2>7 & X3<8 | X1>7 & X2<8 & X3<8) #2 balls are red
Prob(B)

#Exercise-4
S = tosscoin(10, makespace = TRUE)
#We will find probability of no head and then subtract it from 1
nohead = S
for(i in 1:10){
  nohead = subset(nohead, nohead[,i]=="T")
}
ans = 1-Prob(nohead)
ans
