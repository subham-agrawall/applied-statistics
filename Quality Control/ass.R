library(readxl)
df1=read_excel("Assignment.xlsx", sheet=1)
df2=read_excel("Assignment.xlsx", sheet=2)
df3=read_excel("Assignment.xlsx", sheet=3)

library(qcc)

### Question1
# Control limits and control charts
obj1 <-qcc(df1,type ="xbar")
summary(obj1)
obj2 <-qcc(df1,type ="R")
summary(obj2)
obj3 <-qcc(df1,type ="S")
summary(obj3)

# No, the process is not under control.
# Out of control subgroups are 11 and 26.
# Elimination of control points:
df1_new=df1[-c(11,26),]

# Revised control limits and control charts
obj4 <-qcc(df1_new,type ="xbar")
summary(obj4)
obj5 <-qcc(df1_new,type ="R")
summary(obj5)
obj6 <-qcc(df1_new,type ="S")
summary(obj6)

# Estimation of mu and sigma
xbar=rowMeans(df1_new)
n=3
mu=mean(xbar)
mu
sigma=var(xbar)*n
sigma

# Before removing out of control subgroups
xbar=rowMeans(df1)
n=3
mu=mean(xbar)
mu
sigma=var(xbar)*n
sigma

# Process capability analysis
lsl=64-0.02
usl=64+0.02
process.capability(obj4,spec.limits=c(lsl,usl))
# From graph, we note that Cp=0.646 and Cp_k=0.627 after removing out of control subgroups.
# Hence the process is bound to produce rejections even when the mean is set on target.

# Before removing out of control subgroups
process.capability(obj1,spec.limits=c(lsl,usl))
# Cp=0.0205 and Cp_k=-0.0254

### Question2
# (a) x-bar control chart is good for such type of data. 
# (b) Control limits and control charts
xbar=mean(df2$Q2)
xbar
movingRange=vector("list", length=19)
for(i in 2:20){
  movingRange[i-1]=abs(df2$Q2[i]-df2$Q2[i-1])
}
movingRange=as.numeric(movingRange)
mrbar=mean(movingRange)
mrbar
d2=1.128

# Control chart for individuals
ucl=xbar + (3*mrbar)/d2
lcl=xbar - (3*mrbar)/d2
ucl
lcl
plot(df2$Q2,main="xbar control plot",xlab="samples",ylab="mistakes",type='l', ylim=c(-6,15))
abline(h=c(xbar,lcl,ucl), lty=2)

# Control chart for moving ranges
ucl=3.267*mrbar
lcl=0
ucl
lcl
plot(movingRange,main="moving ranges control plot",xlab="samples",ylab="moving ranges",type='l', ylim=c(0,13))
abline(h=c(mrbar,lcl,ucl), lty=2)
# hence, the entire process is under control. There are no out of control points.

### Question3
# Control charts for machine1
machine1<-qcc.groups(df3[,1],df3[,4])
obj1 <-qcc(machine1,type ="xbar")
summary(obj1)
obj2 <-qcc(machine1,type ="R")
summary(obj2)
# Subgroup 8 is out of the control on the xbar chart. Hence, process variation is in control but mean is not in control for subgroup-8.

# Control charts for machine2
machine2<-qcc.groups(df3[,2],df3[,4])
obj1 <-qcc(machine2,type ="xbar")
summary(obj1)
obj2 <-qcc(machine2,type ="R")
summary(obj2)
# All subgroups of machine2 are in control. Hence, entire process is in control.

# Control charts for machine3
machine3<-qcc.groups(df3[,3],df3[,4])
obj1 <-qcc(machine3,type ="xbar")
summary(obj1)
obj2 <-qcc(machine3,type ="R")
summary(obj2)
# Subgroups 2 and 14 are out of the control on the xbar chart. Hence, process variation is in control but mean is not in control for subgroups-2 and 14.