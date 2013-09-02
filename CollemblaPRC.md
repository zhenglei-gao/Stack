
Analysis on Collembolan
===================================================

## Summary

First, it is very hard for me to decide which kind of test to use and which type of transformation is more appropriate. There is no right or wrong answer. You can always work out some rationale to support your choices. Thus, instead of arguing whether to use transformed data, we provide both results and state that we get the same conclusion regardless of whether we log transform the data.

For the simple test, the ToxRat way of doing it is like following:

1. Shapiro-Wilk's test for normal assumption. If not significantly deviate from normal, t-test.
2. Levene's test on variance homogeneity, if not heteroscedastic, followed by a parametric test (t-test) or a non-parametric test (U-test). 

For small sample size of 4 like our case, it is not useful to perform a normality test. I personally would prefer the results from nonparametric test. As the choice of tests, UCLA has [a nice table](http://www.ats.ucla.edu/STAT/mult_pkg/whatstat/default.htm) for choosing an appropriate statistical analysis. For each date and each relevant species, I give the p-values from student-t test and Mann-Whitney test for control vs. tox ref. For test items with 2 different application rates, I give the table of p-values from one-way ANOVA and Kruskal Wallis tests. Also the NOEC was calculated from William's test. There are NaN values in the tables where the observed abundance values are all 0's in all groups.

The results from multivariate principle response curve analysis are summarized in "Multivariate Principle Response Curve Analysis.docx". 




## 0. Data Pre-processing and Source Functions

* Read in data and pre-process

```{r data-preprocess,eval=TRUE}
#setwd("E:/PRC/")

## Compare the old data with Steffi's new data! And use the new one!

soilcore.testitem <- read.csv("soilcore.testitem.csv")
soilcore.testitem.all <- read.csv("soilcore.testitem.all.csv")
sum(soilcore.testitem.all!=soilcore.testitem)
soilcore.testitem$Date <- as.Date(soilcore.testitem$Date,format="%d/%m/%Y")
soilcore.testitem$DAT <- soilcore.testitem$Date-as.Date("2012-06-28")
soilcore.toxic <- read.csv("soilcore.toxic.csv")
soilcore.toxic.all <- read.csv("soilcore.toxic.all.csv")
## for(i in 7:55) print(paste(i,prod(soilcore.toxic.all[,i]==soilcore.toxic[,i])))
## No Need to Change
soilcore.toxic$Date <- as.Date(soilcore.toxic$Date,format="%d/%m/%Y")
soilcore.toxic$DAT <- soilcore.toxic$Date-as.Date("2012-06-28")
## soilcore <- read.csv("soilcore.csv")
### pitfalls, all
#pitfall.testitem <-   read.csv("pitfall.testitem.csv")
##pitfall.testitem.all <-   read.csv("pitfall.testitem.all.csv")
## Need to change!
pitfall.testitem <-   read.csv("pitfall.testitem.all.csv")
pitfall.testitem$Date <- factor(pitfall.testitem$Date,levels=c("20-23.06.2012","06-09.07.2012","31.07-03.08.2012","18-21.09.2012"))
library(plyr)
pitfall.testitem$Date1 <- revalue(pitfall.testitem$Date,c("20-23.06.2012"="2012-06-23","06-09.07.2012"="2012-07-09","31.07-03.08.2012"="2012-08-03","18-21.09.2012"="2012-09-21"))
pitfall.testitem$DAT <- as.Date(pitfall.testitem$Date1)-as.Date("2012-06-28")
########################
## pitfall.toxic <-   read.csv("pitfall.toxic.csv")
## pitfall.toxic.all <-   read.csv("pitfall.toxic.all.csv")
## Need to change!
pitfall.toxic <-   read.csv("pitfall.toxic.all.csv")
pitfall.toxic$Date <- factor(pitfall.toxic$Date,levels=c("20-23.06.2012","06-09.07.2012","31.07-03.08.2012","18-21.09.2012"))
pitfall.toxic$Date1 <- revalue(pitfall.toxic$Date,c("20-23.06.2012"="2012-06-23","06-09.07.2012"="2012-07-09","31.07-03.08.2012"="2012-08-03","18-21.09.2012"="2012-09-21"))
pitfall.toxic$DAT <- as.Date(pitfall.toxic$Date1)-as.Date("2012-06-28")
########################
### With only the most abundant species:
# pitfall.toxic.ma <-   read.csv("pitfall.toxic.ma.csv")
## need to change:
pitfall.toxic.ma <-   read.csv("pitfall.toxic.ma.new.csv")
pitfall.toxic.ma$Date <- factor(pitfall.toxic.ma$Date,levels=c("20-23.06.2012","06-09.07.2012","31.07-03.08.2012","18-21.09.2012"))
pitfall.toxic.ma$Date1 <- revalue(pitfall.toxic.ma$Date,c("20-23.06.2012"="2012-06-23","06-09.07.2012"="2012-07-09","31.07-03.08.2012"="2012-08-03","18-21.09.2012"="2012-09-21"))
pitfall.toxic.ma$DAT <- as.Date(pitfall.toxic.ma$Date1)-as.Date("2012-06-28")
###################
#pitfall.testitem.ma <-   read.csv("pitfall.testitem.ma.csv")
## need to change!
pitfall.testitem.ma <-   read.csv("pitfall.testitem.ma.new.csv")
pitfall.testitem.ma$Date <- factor(pitfall.testitem.ma$Date,levels=c("20-23.06.2012","06-09.07.2012","31.07-03.08.2012","18-21.09.2012"))
pitfall.testitem.ma$Date1 <- revalue(pitfall.testitem.ma$Date,c("20-23.06.2012"="2012-06-23","06-09.07.2012"="2012-07-09","31.07-03.08.2012"="2012-08-03","18-21.09.2012"="2012-09-21"))
pitfall.testitem.ma$DAT <- as.Date(pitfall.testitem.ma$Date1)-as.Date("2012-06-28")

#################
##soilcore.testitem.ma <- read.csv("soilcores.testitem.ma.csv")
## Need to Change
soilcore.testitem.ma <- read.csv("soilcores.testitem.ma.new.csv")
soilcore.testitem.ma$Date <-revalue(soilcore.testitem.ma$Date,c("T1"="2012-06-22","T2"="2012-07-09","T3"="2012-08-03","T4"="2012-09-18")) 
soilcore.testitem.ma$DAT <- as.Date(soilcore.testitem.ma$Date)-as.Date("2012-06-28")
##soilcore.toxic.ma <- read.csv("soilcores.toxic.ma.csv")
soilcore.toxic.ma <- read.csv("soilcores.toxic.ma.new.csv")
soilcore.toxic.ma$Date <-revalue(soilcore.toxic.ma$Date,c("T1"="2012-06-22","T2"="2012-07-09","T3"="2012-08-03","T4"="2012-09-18"))
soilcore.toxic.ma$DAT <- as.Date(soilcore.toxic.ma$Date)-as.Date("2012-06-28")
################### Family Data Pitfalls
pitfall.toxic.family <-   read.csv("pitfall.toxic.family.csv")
pitfall.toxic.family$Date <- factor(pitfall.toxic.family$Date,levels=c("20-23.06.2012","06-09.07.2012","31.07-03.08.2012","18-21.09.2012"))
pitfall.toxic.family$Date1 <- revalue(pitfall.toxic.family$Date,c("20-23.06.2012"="2012-06-23","06-09.07.2012"="2012-07-09","31.07-03.08.2012"="2012-08-03","18-21.09.2012"="2012-09-21"))
pitfall.toxic.family$DAT <- as.Date(pitfall.toxic.family$Date1)-as.Date("2012-06-28")
###################
pitfall.testitem.family <-   read.csv("pitfall.testitem.family.csv")
pitfall.testitem.family$Date <- factor(pitfall.testitem.family$Date,levels=c("20-23.06.2012","06-09.07.2012","31.07-03.08.2012","18-21.09.2012"))
pitfall.testitem.family$Date1 <- revalue(pitfall.testitem.family$Date,c("20-23.06.2012"="2012-06-23","06-09.07.2012"="2012-07-09","31.07-03.08.2012"="2012-08-03","18-21.09.2012"="2012-09-21"))
pitfall.testitem.family$DAT <- as.Date(pitfall.testitem.family$Date1)-as.Date("2012-06-28")

#################Family Data Soil Cores
soilcore.testitem.family <- read.csv("soilcore.testitem.family.csv")
soilcore.testitem.family.new <- read.csv("soilcore.testitem.family.new.csv")
soilcore.testitem.family$Date <-revalue(soilcore.testitem.family$Date,c("T1"="2012-06-22","T2"="2012-07-09","T3"="2012-08-03","T4"="2012-09-18")) 
soilcore.testitem.family$DAT <- as.Date(soilcore.testitem.family$Date)-as.Date("2012-06-28")
soilcore.toxic.family <- read.csv("soilcore.toxic.family.new.csv")
soilcore.toxic.family$Date <-revalue(soilcore.toxic.family$Date,c("T1"="2012-06-22","T2"="2012-07-09","T3"="2012-08-03","T4"="2012-09-18"))
soilcore.toxic.family$DAT <- as.Date(soilcore.toxic.family$Date)-as.Date("2012-06-28")

################ Order Data
soilcore.testitem.order <- read.csv("soilcore.testitem.order.csv")
soilcore.testitem.order$Date <-revalue(soilcore.testitem.order$Date,c("T1"="2012-06-22","T2"="2012-07-09","T3"="2012-08-03","T4"="2012-09-18")) 
soilcore.testitem.order$DAT <- as.Date(soilcore.testitem.order$Date)-as.Date("2012-06-28")
soilcore.toxic.order <- read.csv("soilcore.toxic.order.csv")
soilcore.toxic.order$Date <-revalue(soilcore.toxic.order$Date,c("T1"="2012-06-22","T2"="2012-07-09","T3"="2012-08-03","T4"="2012-09-18"))
soilcore.toxic.order$DAT <- as.Date(soilcore.toxic.order$Date)-as.Date("2012-06-28")


###################
pitfall.toxic.order <-   read.csv("pitfall.toxic.order.csv")
pitfall.toxic.order$Date <- factor(pitfall.toxic.order$Date,levels=c("20-23.06.2012","06-09.07.2012","31.07-03.08.2012","18-21.09.2012"))
pitfall.toxic.order$Date1 <- revalue(pitfall.toxic.order$Date,c("20-23.06.2012"="2012-06-23","06-09.07.2012"="2012-07-09","31.07-03.08.2012"="2012-08-03","18-21.09.2012"="2012-09-21"))
pitfall.toxic.order$DAT <- as.Date(pitfall.toxic.order$Date1)-as.Date("2012-06-28")
###################
pitfall.testitem.order <-   read.csv("pitfall.testitem.order.csv")
pitfall.testitem.order$Date <- factor(pitfall.testitem.order$Date,levels=c("20-23.06.2012","06-09.07.2012","31.07-03.08.2012","18-21.09.2012"))
pitfall.testitem.order$Date1 <- revalue(pitfall.testitem.order$Date,c("20-23.06.2012"="2012-06-23","06-09.07.2012"="2012-07-09","31.07-03.08.2012"="2012-08-03","18-21.09.2012"="2012-09-21"))
pitfall.testitem.order$DAT <- as.Date(pitfall.testitem.order$Date1)-as.Date("2012-06-28")

## New Data:
## Only Species:
##-----------------
soilcore.testitem.species <-  read.csv("soilcore.testitem.species.csv")
soilcore.testitem.species$Date <-revalue(soilcore.testitem.species$Date,c("T1"="2012-06-22","T2"="2012-07-09","T3"="2012-08-03","T4"="2012-09-18")) 
soilcore.testitem.species$DAT <- as.Date(soilcore.testitem.species$Date)-as.Date("2012-06-28")
##--------------
soilcore.toxic.species <-  read.csv("soilcore.toxic.species.csv")
soilcore.toxic.species$Date <-revalue(soilcore.toxic.species$Date,c("T1"="2012-06-22","T2"="2012-07-09","T3"="2012-08-03","T4"="2012-09-18"))
soilcore.toxic.species$DAT <- as.Date(soilcore.toxic.species$Date)-as.Date("2012-06-28")
##------------------------
pitfall.testitem.species <-   read.csv("pitfall.testitem.species.csv")
pitfall.testitem.species$Date <- factor(pitfall.testitem.species$Date,levels=c("20-23.06.2012","06-09.07.2012","31.07-03.08.2012","18-21.09.2012"))
pitfall.testitem.species$Date1 <- revalue(pitfall.testitem.species$Date,c("20-23.06.2012"="2012-06-23","06-09.07.2012"="2012-07-09","31.07-03.08.2012"="2012-08-03","18-21.09.2012"="2012-09-21"))
pitfall.testitem.species$DAT <- as.Date(pitfall.testitem.species$Date1)-as.Date("2012-06-28")
##-----------------------
pitfall.toxic.species <-   read.csv("pitfall.toxic.species.csv")
pitfall.toxic.species$Date <- factor(pitfall.toxic.species$Date,levels=c("20-23.06.2012","06-09.07.2012","31.07-03.08.2012","18-21.09.2012"))
pitfall.toxic.species$Date1 <- revalue(pitfall.toxic.species$Date,c("20-23.06.2012"="2012-06-23","06-09.07.2012"="2012-07-09","31.07-03.08.2012"="2012-08-03","18-21.09.2012"="2012-09-21"))
pitfall.toxic.species$DAT <- as.Date(pitfall.toxic.species$Date1)-as.Date("2012-06-28")
```

## 1. Simple Exploratory Data Analysis

* Compareplot to get a rough idea

```{r compareplot,eval=FALSE}
require(taRifx)
Icompareplot <- function(dataset,rmcol,measure.vars,string){
   longdata <- melt(dataset[,rmcol],measure.vars=measure.vars)
   windows()
   compareplot(~value|Treatment.1*Date*variable,data.frame=longdata,main=string,show.outliers=TRUE)
  dev.print(device=png,file=paste(string,"-Treatment.png",sep=""),units="in", width=11, height=8.5, res=300)
  compareplot(~value|Date*Treatment.1*variable,data.frame=longdata,main=string,show.outliers=TRUE)
  dev.print(device=png,file=paste(string,"-Date.png",sep=""),units="in", width=11, height=8.5, res=300)
 compareplot(~value|variable*Date*Treatment.1,data.frame=longdata,main=string,show.outliers=TRUE)
 dev.print(device=png,file=paste(string,"-Species.png",sep=""),units="in", width=11, height=8.5, res=300)
   return(NULL)
}
dataset <- soilcore.toxic.order
rmcol <- -c(2,5,9)
measure.vars <- names(dataset)[6:8]
Icompareplot(dataset,rmcol,measure.vars,string="Soil Cores-Tox Ref-Order")
dataset <- pitfall.testitem.order
rmcol <- -c(2,5,9,10)
measure.vars <- names(dataset)[6:8]
Icompareplot(dataset,rmcol,measure.vars,string="Pitfalls-Test Item-Order")

dataset <- soilcore.toxic.family
rmcol=-c(2,5,17);
measure.vars=names(dataset)[6:12]
Icompareplot(dataset,rmcol,measure.vars,string="Soil Cores-Tox Ref-Family-1")
measure.vars=names(dataset)[13:16]
Icompareplot(dataset,rmcol,measure.vars,string="Soil Cores-Tox Ref-Family-2")

dataset <- pitfall.toxic.family
names(dataset)[2] <- "Treatment.1"
rmcol=-c(4,15,16);
measure.vars=names(dataset)[5:11]
Icompareplot(dataset,rmcol,measure.vars,string="Pitfalls-Tox Ref-Family-1")
measure.vars=names(dataset)[12:14]
Icompareplot(dataset,rmcol,measure.vars,string="Pitfalls-Tox Ref-Family-2")

dataset <- soilcore.testitem.ma
rmcol=-c(2,5,19)
measure.vars=names(dataset)[6:12]
Icompareplot(dataset,rmcol,measure.vars,string="Soil Cores-Test Item-MA-1")
measure.vars=names(dataset)[13:18]
Icompareplot(dataset,rmcol,measure.vars,string="Soil Cores-Test Item-MA-2")

dataset <- pitfall.toxic.ma
rmcol=-c(2,5,17,18)
measure.vars=names(dataset)[6:12]
Icompareplot(dataset,rmcol,measure.vars,string="Pitfalls-Tox Ref-MA-1")
measure.vars=names(dataset)[13:16]
Icompareplot(dataset,rmcol,measure.vars,string="Pitfalls-Tox Ref-MA-2")
```

* Boxplot requested from Frank

```{r boxplot,eval=FALSE}
bplot(dataset=soilcore.testitem.order,responsecol=6:8,transformAbun=function(x){x})
bplot(dataset=soilcore.toxic.order,responsecol=6:8,transformAbun=function(x){x})

bplot(dataset=pitfall.testitem.order,responsecol=6:8,transformAbun=function(x){x})
bplot(dataset=pitfall.toxic.order,responsecol=6:8,transformAbun=function(x){x})

bplot(dataset=soilcore.toxic.family,responsecol=6:16,transformAbun=function(x){x})
bplot(dataset=soilcore.testitem.family,responsecol=6:16,transformAbun=function(x){x})

pitfall.toxic.family$Treatment.1 <- pitfall.toxic.family$Treatment
bplot(dataset=pitfall.toxic.family,responsecol=5:14,transformAbun=function(x){x})
pitfall.testitem.family$Treatment.1 <- pitfall.testitem.family$Treatment
bplot(dataset=pitfall.testitem.family,responsecol=5:14,transformAbun=function(x){x})

#########

bplot(dataset=soilcore.testitem.ma,responsecol=6:18,transformAbun=function(x){x})
bplot(dataset=soilcore.toxic.ma,responsecol=6:18,transformAbun=function(x){x})

bplot(dataset=pitfall.testitem.ma,responsecol=6:16,transformAbun=function(x){x})
bplot(dataset=pitfall.toxic.ma,responsecol=6:16,transformAbun=function(x){x})
```

## 2. Simple Tests Summary

```{r testsource}
findNOEC <- function(out_willi){
 
  ndose <- nrow(out_willi[[1]])
  res <- lapply(out_willi,function(x){
    pval <- x$pval
    i <- ndose
    for(i in ndose:1){
      if(is.na(pval[i])){
        break
      }
      if(pval[i]>0.05){
        break
        }else{
          
          }
      
      }
    return(x$Dose[i])
    })
  return(unlist(res))
}


findNOEC_D <- function(out_dunn){
 
  ndose <- nrow(out_dunn[[1]])
  res <- lapply(out_dunn,function(x){
    pval <- x$pval
    i <- ndose
    for(i in ndose:1){
      if(pval[i]>0.05){
        break
        }else{
          
          }
      
      }
    return(x$comp[i])
    })
  return(unlist(res))
}

simpletests <- function(dataset=soilcore.toxic.order,responsecol=6:8,treatment=c("testitem","tox")){
  if(is.null(dataset$Treatment.1)) dataset$Treatment.1 <- dataset$Treatment
  nm <- deparse(substitute(dataset))
  dates <- dataset$Date
  spnms <- names(dataset)[responsecol]
  
  treatment <- match.arg(treatment)
  res <- NULL
  resmat <- matrix(NA,nrow=length(responsecol),ncol=nlevels(dates))
  rownames(resmat) <- spnms
  colnames(resmat) <- levels(dates)
  if(treatment=="testitem"){
    res[["KStest"]] <- resmat
    res[["aovtest"]] <- resmat
    res[["Wtest"]] <- as.data.frame(resmat)
    ## there are two treatment group vs. control group
    for(j in 1:nlevels(dates)){
      dat <- levels(dates)[j]
      y <- dataset[dates==dat,responsecol]
      x <- dataset[dates==dat,"Treatment.1"]
      out_willi <- calcWiiliams(data=data.frame(dose=x,y),method="simulation",trend="downward")
      res[["Wtest"]][,j] <- findNOEC(out_willi)
      }
    outfile <- paste(nm,"Williams_test.csv",sep="_")
    write.csv(res[["Wtest"]],file=outfile)
    for(i in seq(responsecol)){   
        spnm <- spnms[i]
        for(j in 1:nlevels(dates)){
          dat <- levels(dates)[j]
          ## 1. anova
          y <- dataset[dates==dat,responsecol[i]]
          x <- dataset[dates==dat,"Treatment.1"]
          res[["aovtest"]][i,j] <- summary(aov(y~x))[[1]][["Pr(>F)"]][1]
          ## 2. KS test
          res[["KStest"]][i,j] <- kruskal.test(y,x)$p.value
          ## 3. William's T-test
          
         ## res[["Wtest"]][i,j] <- Williams()
          ## 4. Dunnetts
          }
        outfile <- paste(nm,"aov_test.csv",sep="_")
        write.csv(res[["aovtest"]],file=outfile)
        outfile <- paste(nm,"Kruska-Wallis_test.csv",sep="_")
        write.csv(res[["KStest"]],file=outfile)
        
        }
    
    }else{
      ## There are only control and tox group
      res[["ttest"]] <- resmat
      res[["Utest"]] <- resmat
      for(i in seq(responsecol)){   
        spnm <- spnms[i]
        for(j in 1:nlevels(dates)){
          dat <- levels(dates)[j]
          ## 1. simple t-test
          y <- dataset[dates==dat,responsecol[i]]
          x <- dataset[dates==dat,"Treatment.1"]
          res[["ttest"]][i,j] <- t.test(y[x=="control"],y[x=="Tox"],alternative="less")$p.value
          ## 2. wilcoxon/Mann-Whitney test
           res[["Utest"]][i,j] <- wilcox.test(y[x=="control"],y[x=="Tox"],alternative="less")$p.value
          }
        }
      outfile <- paste(nm,"t_test.csv",sep="_")
      write.csv(res[["ttest"]],file=outfile)
      outfile <- paste(nm,"Mann-Whitney_test.csv",sep="_")
      write.csv(res[["Utest"]],file=outfile)
      }
  
  return(res)
  }

```

```{r simpletestsexc,eval=FALSE}
simpletests(dataset=soilcore.testitem.order,responsecol=6:8,treatment="testitem")
simpletests(dataset=soilcore.toxic.order,responsecol=6:8,treatment="tox")

simpletests(dataset=pitfall.testitem.order,responsecol=6:8,treatment="testitem")
simpletests(dataset=pitfall.toxic.order,responsecol=6:8,treatment="tox")

simpletests(dataset=soilcore.toxic.family,responsecol=6:16,treatment="tox")
simpletests(dataset=soilcore.testitem.family,responsecol=6:16,treatment="testitem")

pitfall.toxic.family$Treatment.1 <- pitfall.toxic.family$Treatment
simpletests(dataset=pitfall.toxic.family,responsecol=5:14,treatment="tox")
pitfall.testitem.family$Treatment.1 <- pitfall.testitem.family$Treatment
simpletests(dataset=pitfall.testitem.family,responsecol=5:14,treatment="testitem")

######### MA

simpletests(dataset=soilcore.testitem.ma,responsecol=6:18,treatment="testitem")
simpletests(dataset=soilcore.toxic.ma,responsecol=6:18,treatment="tox")

simpletests(dataset=pitfall.testitem.ma,responsecol=6:16,treatment="testitem")
simpletests(dataset=pitfall.toxic.ma,responsecol=6:16,treatment="tox")
```


## 3. PRC
```{r PRCsourcefun}
##
MYprc <- function(dataset,responsecol,transformAbun,wotoxic=TRUE,calc2ndprc=TRUE,NOECc=c("permutation","Williams"),...){
  ## the function to do all procedures for each datasets.
  Treatment <- dataset$Treatment.1
  if(wotoxic==TRUE) {
    Dose <- revalue(Treatment,c("control"="0","Rate 1"="5","Rate 2"="10"))
    }else Dose <- revalue(Treatment,c("control"=0,"Tox"="1"))
  Response <- (dataset[,responsecol])
#   if(transformAbun=="none"){
#     print("No transformation applied to abundence data")
#   }else{
#     if(transformAbun=="log") Response <- log(Response+1) else Response <- sqrt(Response)
#   }
  Response <- transformAbun(Response)
  
  time <- as.factor(dataset$DAT)
  dataset_prc <- prc(response = Response, treatment = Treatment , time = time)
  sum_prc <- summary(dataset_prc)
  # Species scores:
  ## sum_prc$sp
  #plot.prc(dataset_prc, select = abs(sum_prc$sp) > 0.5,xlab="Date",at=as.numeric(unique(dataset$DAT)),labels=unique(dataset$Date),main="Soil Score w.o. Toxic Reference",legpos="bottomleft")
  ##
  res <- data.frame(percentTime=NA,percentTrt=NA,percent1prc=NA,percent2prc=NA,periodSig=NA,NOECc=NA,p1prc=NA,p2prc=NA,pMod=NA)
  res[1] <- dataset_prc$pCCA$tot.chi/dataset_prc$tot.chi
  res[2] <- dataset_prc$CCA$tot.chi/dataset_prc$tot.chi
  tmp <- dataset_prc$CCA$eig/sum(dataset_prc$CCA$eig)
  res[3] <- tmp[1]
  res[4] <- tmp[2]
  tmp <- anova(dataset_prc, strata = time, first=TRUE, perm.max = 9999)
  res["p1prc"] <- tmp["Pr(>F)"][1,1]
  if(calc2ndprc==TRUE){
    # cat("can we get the second PRC ??\n")
    ## print(anova(dataset_prc, strata = time,by="axis",perm.max = 9999))
    tmp <- anova(dataset_prc, strata = time,perm.max = 9999)
    res["pMod"] <- tmp["Pr(>F)"][1,1]
    }
  ## NOEC
  # the period of significant influence of the treatment:
  ## First we need to transform the treatment dose:
  ln_treatment <- log(2/5 * as.numeric((Dose)) + 1)
  ## 
#   out <- NULL
#   for(i in levels(time)) {
#     take_spec <- Response[time == i, ]
#     browser()
#     take_treatment <- ln_treatment[time == i]
#     tmp <-rda(take_spec ~ take_treatment)
#     out[[i]] <- anova(tmp, by = "terms", step = 1000)
#     }
#   period <- sapply(out, function(x) x[1, 5]) 
#   
#   cat("# grabs the p-values per date\n")
#   print(period)
  if(NOECc="Williams") NOEC <- NOECcommunity(Treatment,time,Response) else NOEC <- NULL
  return(list(tableres=res,prcres=dataset_prc,Response=Response,time=time,ln_treatment=ln_treatment,NOEC=NOEC))
  
  }

summaryPRC <- function(dataset,trend="downward",calc2ndprc=TRUE,transform=TRUE,NOECc=c("permutation","Williams"),...){
  nm <- deparse(substitute(dataset))
  if(nm=="soilcore.testitem") responsecol <- 6:55
  if(nm=="soilcore.testitem.family") responsecol <- 6:14
  if(nm=="soilcore.testitem.ma") responsecol <- 6:13
  if(nm=="soilcore.testitem.order") responsecol <- 6:8
  if(nm=="pitfall.testitem") responsecol <- 6:47
  if(nm=="pitfall.testitem.family") responsecol <- 5:14
  if(nm=="pitfall.testitem.ma") responsecol <- 6:14
  if(nm=="pitfall.testitem.order") responsecol <- 6:8
  if(nm=="soilcore.toxic") responsecol <- 6:55
  if(nm=="soilcore.toxic.family") responsecol <- 6:14
  if(nm=="soilcore.toxic.ma") responsecol <- 6:13
  if(nm=="soilcore.toxic.order") responsecol <- 6:8
  if(nm=="pitfall.toxic") responsecol <- 6:47
  if(nm=="pitfall.toxic.family") responsecol <- 5:14
  if(nm=="pitfall.toxic.ma") responsecol <- 6:14
  if(nm=="pitfall.toxic.order") responsecol <- 6:8
  
  if(nm=="pitfall.toxic.species") responsecol <- 6:29
  if(nm=="pitfall.testitem.species") responsecol <- 6:29
  if(nm=="soilcore.toxic.species") responsecol <- 6:34
  if(nm=="soilcore.testitem.species") responsecol <- 6:34
 
  if(grepl("testitem",nm)==TRUE){
    wotoxic <- TRUE
    }else{
      wotoxic <- FALSE
      }
  if(grepl("soilcore",nm)==TRUE) A0 <- 2/510 else A0<- 2/1
  if(transform==TRUE){
    tfAbun <- function(x,A=A0){
      log(A*x+1)
      }
    }else{
      tfAbun <- function(x){
      x
      }
    }
  ###########
  res <- MYprc (dataset=dataset,responsecol=responsecol,transformAbun=tfAbun,wotoxic=wotoxic,trend=trend,calc2ndprc=calc2ndprc,NOECc=NOECc)
  sum_prc <- summary(res$prcres)
  windows()
  plotprc(res$prcres,xlab="DAT",select=abs(sum_prc$sp)>0.5,labels=unique(dataset$Date),ylab="Canonical Coefficient(Cdt)",legpos="bottomleft")
  dev.print(pdf,paste(nm,"_treatment.pdf",sep=""))
  plotprc(res$prcres,xlab="DAT",labels=unique(dataset$Date),legpos="bottomleft")
  dev.print(pdf,paste(nm,"_treatment_0.pdf",sep=""))
  ## res$NOEC
  sampledates <- levels(dataset$Date)
  if(NOECc=="Williams"){
  W <- as.matrix(do.call(rbind,lapply(res$NOEC$willi,function(x) x$pval))[,-1])
  rownames(W)<- sampledates
  colnames(W) <- paste("Williams",levels(dataset$Treatment.1)[-1])
  D <- do.call(rbind,lapply(res$NOEC$dunnett,function(x) x$pval))
  rownames(D)<- sampledates
  colnames(D) <- paste("Dunnetts",levels(dataset$Treatment.1)[-1])
  NOEC <- cbind(W,D)
   
  NOECdates <- findNOEC(res$NOEC$willi)
  names(NOECdates) <- levels(dataset$Date)
  tableres <- res$tableres
  tableres["NOECc"]<-sort((NOECdates))[1]
  ########################################
  NOECdatesD <- findNOEC_D(res$NOEC$dunnett)
  tableres$NOECc_D <- sort((NOECdatesD))[1]
  }
  #################
  Response <- res$Response
  time <- res$time
  ln_treatment=res$ln_treatment
  out <- NULL
  outNOEC <- NULL
  for(i in levels(time)) {
    take_spec <- Response[time == i, ]
    take_treatment <- ln_treatment[time == i]
    tmp <-rda(take_spec ~ take_treatment)
    out[[i]] <- anova(tmp, by = "terms", step = 1000)
    if(wotoxic==TRUE){
    outNOEC[[i]] <- data.frame(comp=c("Rate 1","Rate 2"),pval=rep(NA,2))
    take_spec <- Response[time==i& dataset$Treatment.1!="Rate 2", ]
    take_treatment <- ln_treatment[time == i& dataset$Treatment.1!="Rate 2"]
    tmp <-rda(take_spec ~ take_treatment)
    outNOEC[[i]][1,2]<- anova(tmp, by = "terms", step = 1000)[1, 5]
    take_spec <- Response[time==i& dataset$Treatment.1!="Rate 1", ]
    take_treatment <- ln_treatment[time == i& dataset$Treatment.1!="Rate 1"]
    tmp <-rda(take_spec ~ take_treatment)
    outNOEC[[i]][2,2] <- anova(tmp, by = "terms", step = 1000)[1, 5]
    }else{
      if(wotoxic==FALSE){
        ## NO NEED to Recalculate
        outNOEC[[i]] <- data.frame(comp="Tox Ref",pval=out[[i]][1,5])
      }
    }
    }
  RDA_NOEC <- findNOEC_D(outNOEC)
  RDA_NOEC <- sort((RDA_NOEC))[1]
  period0 <- sapply(out, function(x) x[1, 5]) 
  if(NOECc=="Williams") write.csv(cbind(NOEC,period0),paste(nm,"Will_Dunn_SigPeriod.csv"))
   if(NOECc=="permutation") write.csv(cbind(RDA_NOEC,period0),paste(nm,"RDA_NOEC_SigPeriod.csv"))
  period <- sampledates[period0<0.05]
  tableres["NOEC_RDA"] <- RDA_NOEC
  tableres["periodSig"] <- paste(period,collapse=",")
  tableres$dataset <- nm
  return(tableres)
  }

```

```{r PRCexc,eval=FALSE}
lownumber(soilcore.testitem[,6:55])
lownumber(soilcore.toxic[,6:55])
lownumber(pitfall.testitem[,6:48])
lownumber(pitfall.toxic[,6:48])
##
lownumber(soilcore.testitem.ma[,6:18])
lownumber(soilcore.toxic.ma[,6:18])
lownumber(pitfall.testitem.ma[,6:16])
lownumber(pitfall.toxic.ma[,6:16])
##
lownumber(soilcore.testitem.family[,6:16])
lownumber(soilcore.toxic.family[,6:16])
lownumber(pitfall.testitem.family[,5:14])
lownumber(pitfall.toxic.family[,5:14])
##
lownumber(soilcore.testitem.order[,6:8])
lownumber(soilcore.toxic.order[,6:8])
lownumber(pitfall.testitem.order[,6:8])
lownumber(pitfall.toxic.order[,6:8])
##
summaryPRC(dataset=soilcore.toxic)
summaryPRC(dataset=soilcore.toxic.ma)
summaryPRC(dataset=soilcore.toxic.family)
summaryPRC(dataset=soilcore.toxic.order)
summaryPRC(dataset=soilcore.toxic.species)

summaryPRC(dataset=soilcore.testitem)
summaryPRC(dataset=soilcore.testitem.ma)
summaryPRC(dataset=soilcore.testitem.family)
summaryPRC(dataset=soilcore.testitem.order)
summaryPRC(dataset=soilcore.testitem.species)

summaryPRC(dataset=pitfall.toxic)
summaryPRC(dataset=pitfall.toxic.ma)
pitfall.toxic.family$Treatment.1 <- pitfall.toxic.family$Treatment
summaryPRC(dataset=pitfall.toxic.family)
summaryPRC(dataset=pitfall.toxic.order)
summaryPRC(dataset=pitfall.toxic.species)

summaryPRC(dataset=pitfall.testitem)
summaryPRC(dataset=pitfall.testitem.ma)
pitfall.testitem.family$Treatment.1 <- pitfall.testitem.family$Treatment
summaryPRC(dataset=pitfall.testitem.family)
summaryPRC(dataset=pitfall.testitem.order)
summaryPRC(dataset=pitfall.testitem.species)


myPRCres <- function(transfrom=TRUE,filename="PRC_transAbun.csv"){
  res <- NULL
  res[[1]] <-summaryPRC(dataset=soilcore.testitem,transform=transform)
  res[[2]] <-summaryPRC(dataset=soilcore.toxic,transform=transform)
  res[[3]] <-summaryPRC(dataset=pitfall.testitem,transform=transform)
  res[[4]] <-summaryPRC(dataset=pitfall.toxic,transform=transform)
  res[[5]] <-summaryPRC(dataset=soilcore.testitem.ma,transform=transform)
  res[[6]] <- summaryPRC(dataset=soilcore.toxic.ma,transform=transform)
  res[[7]] <-summaryPRC(dataset=pitfall.testitem.ma,transform=transform)
  res[[8]] <-summaryPRC(dataset=pitfall.toxic.ma,transform=transform)
  res[[9]] <-summaryPRC(dataset=soilcore.testitem.family,transform=transform)
  res[[10]] <-summaryPRC(dataset=soilcore.toxic.family,transform=transform)
  pitfall.testitem.family$Treatment.1 <- pitfall.testitem.family$Treatment
  res[[11]] <-summaryPRC(dataset=pitfall.testitem.family,transform=transform)
   pitfall.toxic.family$Treatment.1 <- pitfall.toxic.family$Treatment
  res[[12]] <-summaryPRC(dataset=pitfall.toxic.family,transform=transform)
  res[[13]] <-summaryPRC(dataset=soilcore.testitem.order,transform=transform)
  res[[14]] <-summaryPRC(dataset=soilcore.toxic.order,transform=transform)
  res[[15]] <-summaryPRC(dataset=pitfall.testitem.order,transform=transform)
  res[[16]] <-summaryPRC(dataset=pitfall.toxic.order,transform=transform)
  res[[17]] <-summaryPRC(dataset=soilcore.testitem.species,transform=transform)
  #### PROBLEMATIC
  res[[18]] <-summaryPRC(dataset=soilcore.toxic.species,transform=transform)
  res[[19]] <-summaryPRC(dataset=pitfall.testitem.species,transform=transform)
  res[[20]] <-summaryPRC(dataset=pitfall.toxic.species,transform=transform)
  write.csv(ldply(res),file=filename)
  
  
  
  return(NULL)  
}
myPRCres(transfrom=TRUE,filename="PRC_transAbun.csv")
myPRCres(transfrom=FALSE,filename="PRC_transAbun.csv")
```



### Transformation of Species Abundance Matrix

The question of optimal transformation of species abundances in ordination has not yet been fully addressed. Some similarity/dissimilarity indices carry in-built transformations (Faith et al. 1987). A logrithmic transformation is used to reduce the effects of the dominant species.  

Here we plot the general patterns in species abundance data.

### General Patterns in Species Abundance and Distributions

~~References~~
* [Transformations for community composition data](http://biol09.biol.umontreal.ca/PLcourses/Section_7.7_Transformations.pdf)
* [Artifactions in the Log-Transformation of Species
Abundance Distributions](http://sev.lternet.edu/~jnekola/nekola%20pdf/fg-43-259-268)
* [Explaining General Patterns in Species Abundance and Distributions](http://www.nature.com/scitable/knowledge/library/explaining-general-patterns-in-species-abundance-and-23162842)
* [vegan-diversity](http://cran.r-project.org/web/packages/vegan/vignettes/diversity-vegan.pdf)
* [taxonomic resolution](http://www.borenv.net/BER/pdfs/ber13/ber13-359.pdf)


```{r transformornot}
extractPlot <- function(dataset,rmcol=-c(2,5,19),measure.vars=names(dataset)[6:18]){
  ## extract the data from each plot for diversity assessment.
  ## for each plot, there are 4 sampling dates.
  ## in this case, there are 3 treatments * 4 replicates plots.
  Treatment <- dataset$Treatment.1 
  dates <- dataset$Date
  ndate <- length(levels(dates))
  ndose <- length(levels(Treatment))

  }
svtest <- function(dataset){
  Treatment <- dataset$Treatment.1 
  dates <- dataset$Date
  for(i in 1:ndate){## for each date
    ## for each species, we make a test between the treatments.
    
  }
} 

```


## Appendix

* My plot function for PRC results

```{r replace-plot.prc,echo=FALSE,eval=TRUE}
`plotprc` <-
    function (x, species = TRUE, select, scaling = 3, axis = 1, type = "l",
              xlab, ylab, ylim, lty = 1:5, col = 1:6, pch, legpos, cex = 0.8,at=NULL,labels,reverse=FALSE,
              ...)
{
    ## save level names before getting the summary
    levs <- x$terminfo$xlev[[2]]
    x <- summary(x, scaling = scaling, axis = axis)
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    b <- t(coef(x))
    xax <- rownames(b)
    if (missing(xlab))
        xlab <- x$names[1]
    if (missing(ylab))
        ylab <- "Effect"
    if (!missing(select))
        x$sp <- vegan:::.checkSelect(select, x$sp)
    if (missing(ylim))
         if (species){
           if(reverse) ylim <- range(-b, -x$sp, na.rm = TRUE) else ylim <- range(b, x$sp, na.rm = TRUE)
         }else{
          if(reverse) ylim <- range(-b, na.rm = TRUE) else  ylim <- range(b, na.rm = TRUE)
         } 
    if (species) {
        op <- par("mai")
        mrg <- max(strwidth(names(x$sp), cex = cex, units = "in")) +
            strwidth("mmm", cex = cex, units = "in")
        par(mai = c(op[1:3], max(op[4], mrg)))
    }
    if (missing(pch))
        pch <- as.character(1:nrow(b))
   # matplot(xax, b, type = type, xlab = xlab, ylab = ylab, ylim = ylim,cex = cex, lty = lty, col = col, pch = pch, xaxt="n",...)
  #axis(1,at=at,labels=labels)
    ##browser()
    if(reverse) matplot(xax, -b, type = type, xlab = xlab, ylab = ylab, ylim = ylim,cex = cex, lty = lty+1, col = col+1, pch = pch,lwd=2.5,...) else matplot(xax, b, type = type, xlab = xlab, ylab = ylab, ylim = ylim,cex = cex, lty = lty+1, col = col+1, pch = pch,lwd=2.5,...)
    abline(h = 0, col = "black",lwd=1.5)
    #text(x=xax,y=0,labels=labels)
    axis(side=3,at=xax,labels=labels)
    if (species) {
      if(reverse){
        linestack(-x$sp, at = par("usr")[2], add = TRUE, hoff = 1,
                  cex = cex, ...)
        rug(-x$sp, side = 4)
      }else{
        linestack(x$sp, at = par("usr")[2], add = TRUE, hoff = 1,
                  cex = cex, ...)
        rug(x$sp, side = 4)
        }
    }
    if (missing(legpos)) {
        holes <- abs(par("usr")[3:4] - range(b, na.rm = TRUE))
        if (holes[1] > holes[2])
            legpos <- "bottomleft"
        else legpos <- "topleft"
    }
    if (!is.na(legpos)) {
        nl <- length(levs)
        pp <- type %in% c("b", "p")
        pl <- type %in% c("b", "l")
        if (length(lty) == 1)
            lty <- rep(lty, nl-1)
        legend(legpos, legend = levs, col = c(1, col+1),
               lty = if (pl) c(1, lty+1),
               pch = if (pp) pch, cex = cex, title = x$names[2])
    }
    invisible()
}

```

* My functions to do the PRC and summarize the results.


```{r archived, eval=FALSE}
suppressWarnings(suppressMessages(library(vegan)))
library(EcotoxTests)
lownumber <- function(x){
  tmp <- unlist(x)
  return(min(tmp[tmp>0]))
}

IBACONprc <- function(dataset,responsecol=c(6,55),transformAbun=tfAbun,wotoxic=TRUE,...){
  ## the function to do all procedures for each datasets.

  Treatment <- dataset$Treatment.1
  if(wotoxic==TRUE) {
    Dose <- revalue(Treatment,c("control"="0","Rate 1"="5","Rate 2"="10"))
    }else Dose <- revalue(Treatment,c("control"=0,"Tox"="1"))
  Response <- (dataset[,responsecol[1]:responsecol[2]])
#   if(transformAbun=="none"){
#     print("No transformation applied to abundence data")
#   }else{
#     if(transformAbun=="log") Response <- log(Response+1) else Response <- sqrt(Response)
#   }
  Response <- transformAbun(Response)
  
  time <- as.factor(dataset$DAT)
  dataset_prc <- prc(response = Response, treatment = Treatment , time = time)
  sum_prc <- summary(dataset_prc)
  # Species scores:
  ## sum_prc$sp
  #plot.prc(dataset_prc, select = abs(sum_prc$sp) > 0.5,xlab="Date",at=as.numeric(unique(dataset$DAT)),labels=unique(dataset$Date),main="Soil Score w.o. Toxic Reference",legpos="bottomleft")
  ##
  cat("The variation in treatment regime explained by the PRC component\n")
  print(dataset_prc$CCA$eig/sum(dataset_prc$CCA$eig))
  ##
  cat("treatment percentage",dataset_prc$CCA$tot.chi/dataset_prc$tot.chi,"\n")## treatment percentage
  cat("time percentage",dataset_prc$pCCA$tot.chi/dataset_prc$tot.chi,"\n")## time percentatge
  print(anova(dataset_prc, strata = time, first=TRUE, perm.max = 9999))
  ## cat("can we get the second PRC ??\n")
  ## print(anova(dataset_prc, strata = time,by=axis,perm.max = 9999))
  ## NOEC
  # the period of significant influence of the treatment:
  ## First we need to transform the treatment dose:
  ln_treatment <- log(20 * as.numeric((Dose)) + 1)
  ## 
#   out <- NULL
#   for(i in levels(time)) {
#     take_spec <- Response[time == i, ]
#     browser()
#     take_treatment <- ln_treatment[time == i]
#     tmp <-rda(take_spec ~ take_treatment)
#     out[[i]] <- anova(tmp, by = "terms", step = 1000)
#     }
#   period <- sapply(out, function(x) x[1, 5]) 
#   
#   cat("# grabs the p-values per date\n")
#   print(period)
  NOEC <- NOECcommunity(Treatment,time,Response)
  return(list(prcres=dataset_prc,Response=Response,time=time,ln_treatment=ln_treatment,NOEC=NOEC))
  
  }
### NOEC calculation::
NOECcommunity <- function(Treatment,time,Response,trend="upward"){
  df <- data.frame(Treatment = Treatment,
                   time = time,Response=Response)
  # package for multiple comparisons
  require(multcomp)
  # create empty object
  out_willi <- NULL
  out_dunnett <- NULL
  pcascores <- NULL
  # loop through time, compute PCA, extract scores and do Williams-Test
  for(i in levels(time)) {
    pca <- rda(Response[time == i, ]) # Compute PCA
    pca_scores <- scores(pca, display="sites", choices = 1) # scores of first principle component
    pcascores[[i]]<-pca_scores
    treatment <- Treatment[time == i]
    out_dunnett[[i]] <- summary(glht(aov(pca_scores ~ treatment, data = df[time == i, ]),alternative = "t",linfct = mcp(treatment = "Dunnett")))
    }
  # extract p-values
  result_dunnett <- lapply(out_dunnett, function(x) data.frame(comp = levels(df$Treatment)[-1],pval = x$test$pvalues,sig = x$test$pvalues < 0.05))
  # shows the results of Williams-Test on PCA-scores for week 1:
  # result_dunnett[['1']]
  
  out_willi <- calcWiiliams(data=data.frame(dose=Treatment[time==i],pcascores),method="simulation",trend=trend)
  return(list(willi=out_willi,dunnett=result_dunnett))
  }
tfAbun <- function(x,A=1){
  log(A*x+1)
}

```

* My functions to make the boxplots.

```{r boxplots}
## http://stackoverflow.com/questions/12784376/multiple-box-plots-within-one-group
require(lattice)
BASF_bplot_0 <- function(dataset,responsecol=6:55,transformAbun=function(x){x}){
  
  nm <- deparse(substitute(dataset))
  fname <- paste(nm,"boxplots.pdf",sep="_")
  pdf(file=fname)
  ## for each species. ## Treatment as the factor
  ## Date as the facets
  #########################
  for(i in responsecol){
    print(i)
    sp <- colnames(dataset)[i]
    #ggplot(dataset,aes(Date,dataset[,i],dodge=Treatment.1))+geom_boxplot(position = position_dodge(width = 10))+theme_bw()
    #qplot(Date,dataset[,i],fill=Treatment.1,data=dataset,col=Treatment.1,geom="boxplot")
    #qplot(dataset[,i],Treatment.1,data=dataset,geom="boxplot")+ geom_point()+facet_grid(.~factor(Date))
    print(bwplot(dataset[,i]~Treatment.1|Date,data=dataset,layout=c(4,1),ylab=paste(sp,"Abundance"),main=nm,panel=function(...){
      panel.bwplot(...,pch="|")
      #panel.points(...,pch=rep(1:3,each=4),jitter=TRUE,cex=2,col="red")
    }))
  }
  dev.off()
  fname <- paste(nm,"allspecies_boxplots.pdf",sep="_")
  pdf(file=fname)
  print(bwplot(apply(dataset[,responsecol],1,sum)~Treatment.1|Date,data=dataset,layout=c(4,1),ylab=paste("All Species","Abundance"),main=nm,panel=function(...){
      panel.bwplot(...,pch="|")
      panel.points(...,pch=rep(1:3,each=4),jitter=TRUE,cex=2,col="red")
    }))
  dev.off()
  
  fname <- paste(nm,"allspecies_2_boxplots.pdf",sep="_")
  pdf(file=fname)
  longdata <- melt(dataset,id.vars=setdiff(colnames(dataset),colnames(dataset)[responsecol]))
  print(bwplot(transformAbun(value)~Treatment.1|Date,data=longdata,layout=c(4,1),ylab=paste("Unpooled All Species","Transformed Abundance"),main=nm,panel=function(...){
      panel.bwplot(...,coef=0)
      panel.points(...,col="red")
    }))
  dev.off()
}

##
bplot <- function(dataset,responsecol=6:55,transformAbun=function(x){x}){

  nm <- deparse(substitute(dataset))
  fname <- paste(nm,"boxplots.pdf",sep="_")
  pdf(file=fname)
  ## for each species. ## Treatment as the factor
  ## Date as the facets
  #########################
  for(i in responsecol){
    print(i)
    sp <- colnames(dataset)[i]
    #ggplot(dataset,aes(Date,dataset[,i],dodge=Treatment.1))+geom_boxplot(position = position_dodge(width = 10))+theme_bw()
    #qplot(Date,dataset[,i],fill=Treatment.1,data=dataset,col=Treatment.1,geom="boxplot")
    #qplot(dataset[,i],Treatment.1,data=dataset,geom="boxplot")+ geom_point()+facet_grid(.~factor(Date))
    print(bwplot(dataset[,i]~Treatment.1|Date,data=dataset,layout=c(4,1),ylab=paste(sp,"Abundance"),main=nm,panel=function(...){
      panel.bwplot(...,pch="|")
      #panel.points(...,pch=rep(1:3,each=4),jitter=TRUE,cex=2,col="red")
    }))
  }
  dev.off()
  }
```

* My functions to do the t-tests and nonparametric wilcoxon tests.




## 1. Simple Tests and Statistics

```{r oldboxplotsRes, eval=FALSE}
BASF_bplot(soilcore.testitem,responsecol=6:55,transformAbun=function(x){
  A <- 2/510
  log(A*x+1)
})
BASF_bplot(soilcore.toxic,responsecol=6:55,transformAbun=function(x){
  A <- 2/510
  log(A*x+1)
})
BASF_bplot(pitfall.toxic,responsecol=6:48,transformAbun=function(x){
  A <- 2/1
  log(A*x+1)
})
BASF_bplot(pitfall.testitem,responsecol=6:48,transformAbun=function(x){
  A <- 2/1
  log(A*x+1)
})
```

## 2. Principle Response Curve Analysis

## References

* [R labs for Vegetation Ecologists](http://ecology.msu.montana.edu/labdsv/R/labs/)
* [Applied Multivariate Statistics for Ecological Data](http://www.umass.edu/landeco/teaching/multivariate/schedule/multivariate_schedule.html)
*[General MVA](http://ubio.bioinfo.cnio.es/Cursos/CEU_MDA07_practicals/Further%20reading/)

> Written with [StackEdit](http://benweet.github.io/stackedit/).