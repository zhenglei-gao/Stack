
# How to calculate the degree of freedom for Williams' Test. 

Williams' test is a multiple comparison test. My question is, when compare the Williams' test statistic with the tabulated critical values, which degree of freedom should I look for. 

In the SAS example, a substance has been tested at seven levels($k=6$) in a randomized block design of eight blocks and the mean square is with (7 - 1)(8 - 1) = 42 degrees of freedom. From my understanding of degree of freedom, shouldn't it be $7\times8-7$?

More formally,

The null hypothesis is:

$$
H_0: \mu_0=\mu_1=...=\mu_k
$$

The alternative is:

$$
H_1: \mu_0\leq\mu_1\leq...\leq\mu_k
$$

with at least one strict inequality. 

The MLE of the treatment means $\mu_i$ are obtained by the sample means $\bar{x}_i$ and the number of observations in the samples $n_i$, by the formular

$$
\mu_i^*=\max_{l\leq u\leq i}\min_{i\leq v \leq k}\sum_{j=u}^{v}n_j \bar{x}_j/\sum_{j=u}^{v}n_j
$$

The test statistic is:

$$
T_i=\frac{\mu_i^*-\bar{x}_0}{\sqrt{s^2/n_i+s^2/n_0}}$$

where, $s$ is an unbiased estimated of $\sigma$, the within group standard deviation that is independent from $\bar X_i$. The null hypothesis is rejected and the fact that the $i$-th dose level is the minimum effective dose is concluded if 

$$T_j> t_{j,\alpha}, \mbox{for all } j \geq i$$

where,  $t_{j,\alpha}$ is the upper $\alpha$th percentile of the distribution $T_j$, with $\nu$ degree of freedom. 

My question is how to calculate $\nu$? And why?

I thought $\nu$ should be $\sum n_i -(k+1)$, but my simulation results agree more with the SAS way of using $n_0\times k$, where all $n_i$'s are equal. Another point, for all $j$'s , I should use the same degree of freedom since I calculated $s$ based on all samples not after removing the already effective high dose level samples. Am I right?



> Written with [StackEdit](http://benweet.github.io/stackedit/).