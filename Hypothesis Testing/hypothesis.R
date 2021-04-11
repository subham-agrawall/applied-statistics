## Question-1
n=100     # sample size
xbar=71.8 # sample mean
sigma=8.9 # population standard deviation
mu0=70    # hypothesized value
z=(xbar-mu0)/(sigma/sqrt(n))
z         # test statistic

# Null Hypothesis : mu=mu0
# Alternate hypothesis : mu>mu0
alpha =0.05
z.alpha=qnorm(1-alpha)
z.alpha    # critical value

# Alternate method
pval= pnorm(z, lower.tail=FALSE)
pval       # upper tail p-value

# The test statistic(2.0224) is greter than the critical value(1.645)
# Reject null hypothesis
# Hence, at .05 significance level, it indicates that mean life span today is greater than 70 years.


## Question-2
n=50
d0=12
x1bar=86.7
sigma1=6.28
x2bar=77.8
sigma2=5.61
z= ((x1bar-x2bar)-d0)/sqrt((sigma1^2/n)+(sigma2^2/n))
z

# Null hypothesis : muA-muB>=d0 (where d0=12)
alpha=0.05
z_alpha=qnorm(1-alpha)
-z_alpha    # Critical value

# Alternate method
pvalue=pnorm(z)
pvalue       #lower tail p-value

# The test statistic(-2.6) is less than the critical value(-1.645)
# Reject null hypothesis
# Hence, at .05 significance level, we reject the manufacturer's claim that the average tensile strength of thread A exceeds the average tensile strength of thread B by at least 12 kilograms.

## Question-3
n=200
p=0.6
q=1-p
P=110/200
z= (P-p)/sqrt((p*q)/n)
z

# Null hypothesis : p>=0.6
alpha=0.05
z_alpha=qnorm(1-alpha)
-z_alpha    # Critical value

# Alternate method
pvalue=pnorm(z)
pvalue       #lower tail p-value

# The test statistic(-1.44) is geater than the critical value(-1.645)
# Fail to reject null hypothesis
# Hence, at .05 significance level, we believe that 60% of residents favor an annexation suit.

## Question-4
P=16/48
p=0.25
q=1-p
n=48
z= (P-p)/sqrt((p*q)/n)
z

# Null hypothesis : p=0.25
# Alternate hypothesis: p>0.25
alpha =0.05
z.alpha=qnorm(1-alpha)
z.alpha    # critical value

# Alternate method
pval= pnorm(z, lower.tail=FALSE)
pval       # upper tail p-value

# The test statistic(1.33) is less than the critical value(1.645)
# Fail to reject null hypothesis
# Hence, at .05 significance level, we don't have a reason to believe that the proportion of rats developing tumors has increased.
