
```{r}
require(vegan)
#the species data are natural log(x) transformed
mod <- prc(response = log(spdta), treatment = dose, time = week)
mod
coef(mod)
coef(summary(mod))
const1 <- -4/sqrt(nrow(spdta))
const2 <- sqrt((nrow(spdta)-1)*mod$tot.chi)/sqrt(6))
const3 <- 
```

> Written with [StackEdit](http://benweet.github.io/stackedit/).