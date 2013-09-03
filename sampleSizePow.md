Sample Size and Power Calculation for ...
=====================================================

### Executive Summary

We need to talk over the phone about some details of the 2005 study so that I can make more realistic assumaptions.

#### Assumptions we make in sample size and power calculations
1. Search Efficiency: 95% and scavenger removal rate: 50%
2. The dead bird rate is $p=25/530= 0.047$ **(question here, how should we calculate the death rate? The total number of birds should be 530*number of scans?)**
3. We then observe $25\times0.5\times 0.95\approx 12$ dead birds in total.

```{r}
library(Hmisc)

```


#### Numbers from previous study
1. Number of treated fields: Ntrt=33 (treated with Mocap 10 G)
2. Number of control fields: Nctr=5
3. Number of reference fields: Nref=6 (treated with other nematicides)
4. Revisited sites in treated fields: Nrev=7
5. Total number of observed birds per scan: Btot= (0.158+0.163+0.102+0.143+0.581)*462$\approx$530
6. Dead birds: Bdead=2/0.74/0.11 $\approx$ 25

#### Problems in the previous study:
1. Instead of the number of birds observed on all fields, we should record number of observed birds in each field. Especially, we need to differentiate between control and treated fields.
2. Similarly, for the dead birds, we should also record where are they found.




#### Methods
You have a one-sided, exact alternative hypothesis $p_{1} > p_{0}$ where $p_{1} = 0.001$ and $p_{0} = 0$.

 - The first step is to identify a threshold $c$ for the number of successes such that the probability to get at least $c$ successes in a sample of size $n$ is very low under the null hypothesis (conventionally $\alpha = 0.05$). In your case, $c=1$, regardless of your particular choice for $n \geqslant 1$ and $\alpha > 0$.
 - The second step is to find out the probability to get at least $c$ successes in a sample of size $n$ under the alternative hypothesis - this is your power. Here, you need a fixed $n$ such that the Binomial distribution $\mathcal{B}(n, p_{1})$ is fully specified.

The second step in R with $n = 500$:

    > n  <- 500                 # sample size
    > p1 <- 0.001               # success probability under alternative hypothesis
    > cc <- 1                   # threshold
    > sum(dbinom(cc:n, n, p1))  # power: probability for cc or more successes given p1
    [1] 0.3936211

To get an idea how the power changes with sample size, you can draw a power function:
![enter image description here][1]

    nn   <- 10:2000                 # sample sizes
    pow  <- 1-pbinom(cc-1, nn, p1)  # corresponding power
    tStr <- expression(paste("Power for ", X>0, " given ", p[1]==0.001))
    plot(nn, pow, type="l", xaxs="i", xlab="sample size", ylab="power",
         lwd=2, col="blue", main=tStr, cex.lab=1.4, cex.main=1.4)

If you want to know what sample size you need to achieve at least a pre-specified power, you can use the power values calculated above. Say you want a power of at least $0.5$.

    > powMin <- 0.5
    > idx    <- which.min(abs(pow-powMin))  # index for value closest to 0.5
    > nn[idx]     # sample size for that index
    [1] 693
    
    > pow[idx]    # power for that sample size
    [1] 0.5000998

So you need a sample size of at least $693$ to achive a power of $0.5$.


  [1]: http://i.stack.imgur.com/LADf2.jpg

#### More
1. In general, to compare two or more proportions, we can perform a chi-square test with continuity correction.
2. If, exact test, fisher's 


```{r}
power.prop.test(p1=.0001, p2=.0047, sig.level=.05, power=.8,alternative="one.sided")

library(pwr)
pwr.2p2n.test(h=0.1172204,n1=800,n2=NULL,power=0.80,sig.level=0.05,alternative="greater")
ES.h(0.0001,0.0047)
## The effect size is 2*asin(sqrt(p1))-2*asin(sqrt(p2))

library(Hmisc)
ballocation(p1=.0001, p2=.0047, alpha=.05)
bpower(p1=.0001, p2=.0047,n1=800,n2=1028, alpha=0.05)
bsamsize(p1=.0001, p2=.0047,fraction=1/2.25,alpha=0.05,power=0.8)

bpower.sim(p1=.001, p2=.047,n1=8000,n2=10208, alpha=0.05)

```


More generally, we can use 

    prop.test(x=c(22, 15, 12, 7), n=c(3053, 2807, 3070, 2806))

to compare the four proportions. Or , a $\chi^2$-Test:

    m <- matrix(c(22, 15, 12, 7, 3053-22, 2807-15, 3070-12, 2806-7), 2, 4, byrow=T)
    chisq.test(m)

can produce exactly the same result.

* https://stat.ethz.ch/pipermail/r-help/2010-June/243518.html