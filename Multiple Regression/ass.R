library(UsingR)
data(homeprice)
plot(homeprice$sale, homeprice$list)

summary(lm(sale~ full+half+bedrooms+rooms+neighborhood-1, data=homeprice))
lm(sale~ half, data=homeprice)

library(lattice)
splom(homeprice)

homeprice$diff= homeprice$sale-homeprice$list
plot(homeprice$neighborhood, homeprice$diff)
summary(lm(diff~neighborhood, data=homeprice))
tapply(homeprice$diff, homeprice$neighborhood, mean)

newdata=data.frame(homeprice, fitted.value=fitted(sale.lm), residual= resid(sale.lm))
