
```{r}
require(vegan)
#the species data are natural log(x) transformed
mod <- prc(response = log(spdta), treatment = dose, time = week)
mod
coef(mod)
coef(summary(mod))
const00 <- -4/sqrt(nrow(spdta))
const01 <- -4/sqrt(ncol(spdta))
const10 <- -4/sqrt(nrow(spdta))
const11 <- -4/sqrt(ncol(spdta))
const20 <- sqrt((nrow(spdta)-1)*mod$tot.chi)/sqrt(nrow(spdta)))
const21 <- sqrt((nrow(spdta)-1)*mod$tot.chi)/sqrt(ncol(spdta)))
const30 <- sqrt(nrow(spdta))/2
const31 <- sqrt(ncol(spdta))/2
const40 <- (sqrt(nrow(spdta))/((nrow(spdta)-1)*mod$tot.chi)^(1/4))
const41 <- (sqrt(ncol(spdta))/((nrow(spdta)-1)*mod$tot.chi)^(1/4))
```

> Written with [StackEdit](http://benweet.github.io/stackedit/).