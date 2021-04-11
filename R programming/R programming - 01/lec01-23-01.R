x1 = 1:6
y1 = 2:7
x1+y1
x1-y1
x1*y1
5*x1
x1/y1

#MAtrices
m1=matrix(1:8, ncol = 2)
m1
m2 = matrix(c(1,2,3,4,5,6,7,8), nrow=2)
m2
m3 = matrix(1:8, nrow=2, byrow = TRUE)
m3
m1[1,2]
m1[2,]
m1[,2]
m=matrix(1:4, ncol=2)
n =matrix(3:6, ncol=2, byrow=T)
n
t(m)
diag(5)
det(m)
dim(m)
nrow(m)
a=c(5,3)
cbind(m,a)
rbind(m,a)
m%*%n #Product of two matrix
cbind(1:3,4:6)
cbind(a,m)

#Factor
sex <- factor(c("M", "F", "M", "M"))
sex
myvector = seq(2,10,by=3)
mymatrix=matrix(1:8, ncol=2)
myfactor= sex
mylist <-list(myvector, mymatrix, myfactor)
length(mylist)
names(mylist)
mylist
mylist[1]
mylist[2]
mode(mylist)
names(mylist)<-c("vec", "mat", "sex")
names(mylist)
mylist$vec
mylist[1]

#data frame
vec1 <- 1:5
vec2 <- c("a","b","c","c","b")
df <- data.frame(name.var1=vec1, name.var2=vec2)
#(as.data.frame)(data.matrix)