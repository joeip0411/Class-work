
library(tidyverse)
options(digits=4)

## Code for Question 1

############## the beta_binomial function ##############

beta_binomial <- function(n,x,alpha=1, beta=1){
  
  atil <- alpha + x
  btil <- beta + n - x
  
  cf <- n/(alpha + beta + n)
  
  out <- list(alpha_tilde = atil, beta_tilde = btil, 
              credibility_factor = cf)
  return(out)
}

#### Code to visualise components of Bayes theorem ####

# A colourblind-friendly palette with black: 
cbbPal <- c(black="#000000", orange="#E69F00", ltblue="#56B4E9", "#009E73",green="#009E73", yellow="#F0E442", blue="#0072B2", red="#D55E00", pink="#CC79A7") 

cbbP <- cbbPal[c("orange","blue","pink")] #choose colours for p1

thetaxx <- seq(0.001,0.999,length.out=100)
priorxx <- dbeta(thetaxx,alpha,beta)
postxx <- dbeta(thetaxx, Q1out$alpha_tilde, Q1out$beta_tilde)
likexx <- dbinom(xobs,size=n,prob=thetaxx)
nlikexx <- 100*likexx/sum(likexx)

df <- tibble(theta=thetaxx, `prior pdf`=priorxx, `normalised likelihood`=nlikexx, `posterior pdf`=postxx)

df_longer <- df%>% pivot_longer(-theta, names_to = "distribution", values_to="density" )

p1 <- df_longer %>% ggplot(aes(x=theta, y=density, colour=distribution, fill=distribution)) + geom_line() + scale_fill_manual(values=cbbP) + theme_bw()

#########################################################

