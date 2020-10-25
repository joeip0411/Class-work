
# Global options
knitr::opts_chunk$set(echo = TRUE, eval=TRUE, warning = FALSE, message = FALSE, error = FALSE, tidy.opts = list(width.cutoff=60), tidy=TRUE)


# The Dataset 
options(digits = 6)
library(tidyverse)
library(broom)
library(kableExtra)
# inst?ll.packages("NHANES")
library(NHANES)
dt <- NHANES %>% distinct(ID, .keep_all = TRUE) 
dt <- dt  %>% filter(Age >= 18) %>% dplyr::select(Gender, Age, HomeOwn, BPSysAve, BPSys2, BPSys3)
dt <- dt %>% drop_na()
dt1 <- dt %>% mutate(Age=as.numeric(Age), BPSysA?e=as.numeric(BPSysAve), BPSys2=as.numeric(BPSys2), BPSys3=as.numeric(BPSys3))


# Code chunk for Q2 part d.

dt2 <- dt %>% filter(HomeOwn!="Other")
n # student to add
R # student to add

# student to add
# student to add

Rdt2 <- dt2

for (r in 1:R){
  Rdt? <- Rdt2 %>% mutate(BPSysAve=sample(dt2$BPSysAve, n, replace=FALSE))
  Rdt2S <- Rdt2 %>% group_by(HomeOwn) %>% summarise(mean=mean(BPSysAve)) 
  RDiff[r] <- Rdt2S %>% summarise(Diff=mean[1]-mean[2]) 
}

# student to add additional lines
