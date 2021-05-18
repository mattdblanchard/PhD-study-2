library(tidyverse)
library(corrr)
library(broom)
library(lattice)
library(nFactors) # Determine Number of Factors to Extract

d.grp <- read_csv("data/190619_dyad_data.csv")

# PCA for accuracy
x <- d.grp %>% 
  dplyr::select(contains("ind")) %>% 
  dplyr::select(contains("acc"),
                -contains("accR"), -contains("accUR"),
                -contains("confR"), -contains("confUR"))

fit <- prcomp(na.omit(x))
fit

percents <- tidy(fit, "d")
ggplot(percents, aes(PC, percent)) +
  geom_line() + geom_point()

pc_x <- augment(fit, na.omit(x))
head(pc_x)

corr <- pc_x %>% 
  select_if(is.numeric) %>% 
  correlate()

acc <- d.grp %>% 
  dplyr::select(group, contains("ind")) %>% 
  dplyr::select(group, contains("acc"),
         -contains("accR"), -contains("accUR"),
         -contains("confR"), -contains("confUR")) %>% 
  na.omit %>% 
  dplyr::select(group) %>% 
  cbind(acc_factor = pc_x$.fittedPC1)


# PCA for confidence
x <- d.grp %>% 
  dplyr::select(contains("ind")) %>% 
  dplyr::select(contains("conf"),
         -contains("accR"), -contains("accUR"),
         -contains("confR"), -contains("confUR"))

fit <- prcomp(na.omit(x))
fit

percents <- tidy(fit, "d")
ggplot(percents, aes(PC, percent)) +
  geom_line() + geom_point()

pc_x <- augment(fit, na.omit(x))
head(pc_x)

corr <- pc_x %>% 
  select_if(is.numeric) %>% 
  correlate()

conf <- d.grp %>% 
  dplyr::select(group, contains("ind")) %>% 
  dplyr::select(group, contains("conf"),
         -contains("accR"), -contains("accUR"),
         -contains("confR"), -contains("confUR")) %>% 
  na.omit %>% 
  dplyr::select(group) %>% 
  cbind(conf_factor = pc_x$.fittedPC1)

d.grp <- d.grp %>% left_join(acc, by = "group")

d.grp <- d.grp %>% left_join(conf, by = "group") 

d.grp %>% 
  mutate(acc_factor = acc_factor * -1) %>% 
  write.csv("data/190718_dyad_pca.csv")


d.grp <- read_csv("data/190718_dyad_pca.csv")

d.grp %>% 
  dplyr::select(acc_factor, contains("ind")) %>% 
  dplyr::select(acc_factor, contains("acc"),
                -contains("accR"), -contains("accUR"),
                -contains("confR"), -contains("confUR")) %>% 
  correlate() %>% focus(acc_factor)













ev <- eigen(cor(na.omit(x))) # get eigenvalues

ap <- parallel(subject = nrow(na.omit(x)), var = ncol(na.omit(x)),
               rep = 100, cent = .05)

nS <- nScree(x = ev$values, aparallel = ap$eigen$qevpea)

plotnScree(nS) 

factanal(na.omit(x), factors = 2, rotation = "varimax")




# individuals & groups
ev <- eigen(cor(both)) # get eigenvalues
ap <- parallel(subject=nrow(both),var=ncol(both),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) 

factanal(both, factors = 2, rotation = "varimax")

