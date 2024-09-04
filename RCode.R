---
title: "ST308 Final Project Code File"
author: "Candidate 23861"
date: "09/05/2024"
output: html_document
---

### Load data and packages
```{r}
library(rstanarm)
library(tidyverse)
star = read.csv("star.csv", header = TRUE)
library("RColorBrewer")
display.brewer.all(colorblindFriendly = TRUE)
colours<-brewer.pal(n = 12, name = "Paired")
```

```{r}
summary(star)
```

# Subset kindergarteners and change character to factor
```{r}
star_K <- star %>% subset(gr == 'K') %>% select(id,sch,cltype,math)
star_K$sch = as.factor(star_K$sch)
star_K$cltype = as.factor(star_K$cltype)
summary(star_K)
```
```{r}
table(star_K$sch)
```
# Explantory Data Analysis
```{r}
boxplot(math~cltype, data=star_K,col=colours[c(2,6,9)],names= c('regular','regularA', 'small'))
boxplot(math ~ sch, data=star_K,xlab='schools')
```

### Linear regression modes

# Pooled model
```{r}
mod1 = lm(math~cltype,data=star_K)
summary(mod1_math)
```

# Fixed effect model
```{r}
mod2 = lm(math~cltype+sch,data=star_K)
summary(mod2)
```


### Bayesian Linear Regression - single level

# Single-level pooled model 
```{r}
set.seed(1)
bmod1 = stan_glm(math~cltype,data=star_K, 
                prior = normal(0,15),
                prior_intercept = normal(0,150))
```

```{r}
prior_summary(bmod1)
```

```{r}
bmod1
ci95 <- posterior_interval(bmod1, prob = 0.95)
round(ci95,2)
```

```{r}
launch_shinystan(bmod1, ppd = FALSE)
```

# Single-level fixed-effect model 
```{r}
set.seed(1)
bmod2 = stan_glm(math~cltype+sch,data=star_K,
                 prior = normal(0,150),
                 prior_intercept = normal(0,150))
```

```{r}
prior_summary(bmod2)
```

```{r}
bmod2
ci95 <- posterior_interval(bmod2, prob = 0.95)
round(ci95,2)
```

```{r}
launch_shinystan(bmod2, ppd = FALSE)
```


### Bayesian Multi-level models

# Varying intercepts
```{r}
set.seed(1)
bhmod1 = stan_lmer(math~cltype + (1 | sch), data=star_K,
                   prior = normal(0,15),
                   prior_intercept = normal(0,150))
```

```{r}
bhmod1
prior_summary(bhmod1)
```

```{r}
launch_shinystan(bhmod1, ppd = FALSE)
```

# Sensitivity Check
```{r}
set.seed(1)
bhmod2 = stan_lmer(math~cltype + (1 | sch), data=star_K,
                   prior = normal(0,500),
                   prior_intercept = normal(0,500))
```

```{r}
launch_shinystan(bhmod2, ppd = FALSE)
```

# Varying intercepts and slopes
```{r}
set.seed(1)
bhmod3 = stan_lmer(math~cltype + (1 + cltype| sch), data=star_K)
```

```{r}
bhmod3
prior_summary(bhmod3)
```

```{r}
launch_shinystan(bhmod3, ppd = FALSE)
```

