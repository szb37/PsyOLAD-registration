---
title: "PsyOLAD Meta Analysis Script - Frequentist models"
author: "Balazs Szigeti"
date: "2023-07-18"
---

library(metafor)
library(here)
library(brms)

rm(list=ls())
df = read.csv(paste(here(),'test_data_v3_freq.csv',sep='/'))
df$dtype <- as.factor(df$dtype)
df$design <- as.factor(df$design)
df$tp_tag <- as.factor(df$tp_tag )
df$bsl <- df$bsl-mean(df$bsl, na.rm=TRUE)
n_trials = 70

### Hypothesis 1
# df1 contains data from open-label SSRI (OLAD) trials and psychedelic-assisted therapy (PAT) trials that
# we selected based on our predefined search criteria. 
#
# We hypothesize that PAT will be superior to OLAD with a margin larger than the minimal important difference.
# This means that in model1 the term dtype(ssri) will have 
# - a significant p-value (p<.05) 
# - and that the estimate is going to be >3 on the HAMD scale. 

df1 <- head(subset(df, tp_tag=='pep'), n=n_trials)
freq_model1 <- rma.mv(
  yi = pep, 
  V = var, 
  slab = study,
  data = df1,
  random = ~ 1+bsl+dtype|study, 
  mods = ~ dtype+bsl,
  struct = "GEN",
  method = 'REML')
summary(freq_model1)
  

### Hypothesis 2
# df2 contains data from open-label SSRI (OLAD) trials that we selected based on our predefined search criteria and
# blinded SSRI trials from Cipriani 2018. 
#
# We hypothesize that OLADs will be superior to blinded SSRI trials with a margin larger than the minimal important difference (MID; Plöderl 2022). 
# This means that in model2 the term is_blinded will have
# - a significant p-value (p<.05) 
# - and that the estimate is going to be >3 on the HAMD scale. 

df2 <- head(subset(df, tp_tag=='pep' & dtype=='ssri'), n=n_trials)
freq_model2 <- rma.mv(
  yi = pep, 
  V = var, 
  slab = study,
  data = df2,
  random = ~ 1+bsl+is_blind|study, 
  mods = ~ is_blind+bsl,
  struct = "GEN",
  method = 'REML')
summary(freq_model2)


### Hypothesis 3
# df3 contains data from psychedelic-assisted therapy (PAT) trials that we selected based on our predefined search criteria.
#
# We hypothesize that whether the trials was formally blinded or not, will not make a difference. 
# This means that in model3 the term is_blinded will have 
# - a non-significant p-value (p>=.05) 
# - and that the estimate is going to be <3 on the HAMD scale. 

df3 <- head(subset(df, tp_tag=='pep' & dtype=='psy'), n=n_trials)
freq_model3 <- rma.mv(
  yi = pep, 
  V = var, 
  slab = study,
  data = df3,
  random = ~ 1+bsl+is_blind|study, 
  mods = ~ is_blind+bsl,
  struct = "GEN",
  method = 'REML')
summary(freq_model3)

