install.packages("prob")
library(prob)

#SAMPLE SPACE
s<- data.frame(toss1 = c("H", "T"))
s

tosscoin(1)
tosscoin(3)

rolldie(2)
rolldie(3, nsides=4)

cards()

#Ordered with replacement
urnsamples(1:3, size=2, replace=TRUE, ordered=TRUE)

#ordered without replacement
urnsamples(1:3, size=2, replace=FALSE, ordered=TRUE)

#unordered without replacement
urnsamples(1:3, size=2, replace=FALSE, ordered=FALSE)

urnsamples(1:3, size=2, replace=TRUE, ordered=FALSE)

#EVENTS
s = tosscoin(2, makespace = TRUE)
s
s[1:3,]
s[c(2,4),]

s= cards()
subset(s, suit=="Heart")
subset(s, rank %in% 7:9)

subset(rolldie(3), X1 + X2 + X3 > 16)

#FUNCTIONS FOR SUBSET

x = 1:10
y= 8:12
y %in% x
#elementwise

isin(x,y)
# false implies x is not contained in y

x = 1:10
y = c(3,3,7)
all(y %in% x)
isin(x,y)

#SET OPERATIONS

s=cards()
a = subset(s, suit=="Heart")
b = subset(s, rank %in% 7:9)

union(a,b)
intersect(a,b)
setdiff(a,b)
#we can get complement using setdiff i.e. setdiff(S,A)

#PROBABILITY SPACE
outcomes = rolldie(1)
outcomes
p = rep(1/6, times=6)
#p = rep(1/8, times=6) also gives same result coz probspace makes sum of prob 1 and each prob is non-negative.
probspace(outcomes, probs=p)

probspace(1:6, probs=p)

probspace(1:6)

rolldie(1, makespace = TRUE)
#no makespace option in urn

probspace(tosscoin(1), probs = c(0.7,0.3))

s= cards(makespace = TRUE)
A= subset(s, suit=="Heart")
B= subset(s, rank %in% 7:9)
Prob(A)
Prob(B)
Prob(s)

#choose k from n
#number of points in sample space #length can also be used
nsamp(n=3, k=2, replace=TRUE, ordered=TRUE)
nsamp(n=3, k=2, replace=FALSE, ordered=TRUE)

#Exercise1
S = rolldie(2, makespace = TRUE)
S
B = subset(S, X1 + X2 == 8)
B
A = subset(S, X1 == X2)
A

Prob(A)
Prob(B)

C = intersect(A,B)
C

#prob of happening A given B
Prob(C)/Prob(B)
Prob(A, given = B) #Better

Prob(B, given=A)

#Exercise2
S=cards(makespace = TRUE)
S
nsamp(n=52, k=2, replace = FALSE, ordered = TRUE)
nsamp(n=4, k=2, replace = FALSE, ordered = TRUE)
12/2652

choose(52, 2)
factorial(3)

mn = c("A",2:10,"J","Q","K")
mn = rep(mn, 4)
mn
urnsamples(mn, size=2, replace = FALSE, ordered = FALSE)


#Exercise3
S = urnsamples(1:10, size=3, replace=FALSE, ordered=TRUE)
p = rep(1, times=nrow(S))
S = probspace(S, probs = p)
S
#assume 1-7 red and 8-10 green
A= subset(S, X1<8 & X2<8 & X3<8)
A
Prob(A)

B= subset(S, X1<8 & X2<8 & X3>7| X1<8 & X2>7 & X3<8 | X1>7 & X2<8 & X3<8)
Prob(B)

C= subset(S, X1<8 & X2<8 & X3>7)
Prob(C)
# 3*probC gives probB

#Exercise4
S = tosscoin(10, makespace = TRUE)
S

##RANDOM VARIABLE
#sample space to real line
#addrv
s = rolldie(3, nsides=4, makespace=TRUE)
s = addrv(S, U=X1-X2+X3)
head(s)
Prob(s, U>6)
#getting wrong CHECK

s = addrv(s, FUN = max, invars = c("X1", "X2","X3"), name="V")
marginal(s, vars = "V")
