
library(tidyverse)

## Code for Exercise B1

## Define functions at the start - so you will have it when you need it

########### the bootplot.f function ############

## This function "bootplot.f" takes a vector of Bootstrap samples as the main argument ('stat_boot'), and produces a plot showing the histogram, with smooth density estimate overlay,and also provides a option (detail) for the number of *bins* used in the histogram. You will need to run through the function code once to save it as an object before you can use the function. 

bootplot.f<- function(stat.boot, bins=50){
  
  df <- tibble(stat = stat.boot)
  CI <- round(quantile(stat.boot, c(0.025, 0.975)),2)
  
  p <- df %>% ggplot(aes(x=stat, y=..density..)) +  
    geom_histogram(bins=bins, colour="magenta", fill="magenta", alpha=0.2) + 
    geom_density(fill="magenta", colour="magenta", alpha=0.2) +
    geom_vline(xintercept = CI, colour = "magenta", linetype=3) +
    theme_bw()
  
  p
}

######## end of bootplot.f function #######
######### continue with main steps ########

## step i.
set.seed(57892)
n <- 30
mu.true <- 3
sig.true <- 2
x <- rnorm(n=n, mean=mu.true, sd=sig.true) # simulated values


dt_data <- tibble(x=x)
p1_simdata <- dt_data %>% ggplot(aes(x=x,y=..density..)) + geom_histogram(colour="steelblue", fill="steelblue", alpha=0.2) + geom_density(colour="steelblue", fill="steelblue", alpha=0.2) + xlim(c(-4,10)) +
  theme_bw()

p1_simdata

## step ii.

xbar.x <- mean(x) # point estimate
SE.x <- sd(x)/sqrt(n) # estimated SE
ttest.out <- t.test(x) %>% tidy() #obtain the standard t-test output
CLT.CI <- c(ttest.out$conf.low, ttest.out$conf.high) 

## step iii.

B <- 5000
xbar_boot <- rep(NA,B)
for(i in 1:B){
  temp <- sample(x, size=n, replace=TRUE)
  xbar_boot[i] <- mean(temp)
}

## step iv.

boot.CI <- quantile(xbar_boot, c(0.025, 0.975))
boot.CI

## step v.

# get the basic plot
p_xbarboot <- bootplot.f(xbar_boot, bins=100) 

## then add layers
## set up a range to view the entire population density
len <- (max(xbar_boot) - min(xbar_boot))/3
xxmax <- (max(xbar_boot) + sqrt(n)*len)
xxmin <- (min(xbar_boot) - sqrt(n)*len)

## define tibble to add layer for the population density
xx <- seq(xxmin, xxmax, length.out=1000)
popn <- dnorm(xx, mean=mu.true, sd=sig.true)
dt <- tibble(xx=xx,popn=popn)

## layer the plot
p_xbarboot <- p_xbarboot + 
  geom_vline(xintercept=xbar.x, colour="red") +
  annotate("text",label=round(boot.CI[1],2), x=(boot.CI[1]-0.5),
           y=0.5,colour="magenta") + 
  annotate("text",label=round(boot.CI[2],2), x=(boot.CI[2]+0.5),
           y=0.5,colour="magenta") +
  geom_vline(xintercept=CLT.CI, colour="darkslategrey", linetype=2) + 
  annotate("text",label=round(CLT.CI[1],2), x=(boot.CI[1]-0.5),
           y=0.75,colour="darkslategrey") +
  annotate("text",label=round(CLT.CI[2],2), x=(boot.CI[2]+0.5),
           y=0.75,colour="darkslategrey") +
  geom_line(data=dt, aes(x=xx, y=popn), colour="blue") +
  geom_vline(xintercept=mu.true, colour="blue") 

## add titles and change axis labels, save plot as object p1
p1 <- p_xbarboot + xlab(expression(bar(x))) 
p1 <- p1 + ggtitle(expression(paste("Bootstrap-based approximate sampling distribution of ", bar(X))),"N(3,4), n=30 and B=5000") 

## step vi. 
p1 


