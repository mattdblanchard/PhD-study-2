library(tidyverse)
# library(dplyr)
library(corrr)
library(broom)
library(mctest)

# Regression --------------------------------------------------------------
# using factor scores
# load data
d.grp <- read_csv("data/190718_dyad_pca.csv") %>%
  mutate(bias_factor = conf_factor - acc_factor)

# correlations between factor scores
d.grp %>% select(acc_factor, conf_factor, bias_factor) %>% 
  correlate()

d.grp %>% select(acc_factor, contains("ind.acc")) %>% 
  correlate() %>% focus(acc_factor)

d.grp %>% select(conf_factor, contains("ind.conf")) %>% 
  correlate() %>% focus(conf_factor)

d.grp %>% select(bias_factor, contains("ind.acc"), contains("ind.conf")) %>% 
  correlate() %>% focus(bias_factor)

# select control variables for correlations
control <- d.grp %>% 
  select(age, prop.female, aus.born, eng.fl, 
         team.familiarity, wm.acc:neuroticism) %>% 
  names()

# list of test names for loop
tests <- c("adr", "crt", "gk", "mdmt", "rapm")

# simple linear regression: no control variables
for (i in tests) {
# select the variables for the series of regression models
  vars <- d.grp %>% 
    select(group, control, acc_factor, conf_factor, bias_factor,
           contains(i)) %>% 
    select(group, contains(paste0(i, ".ind.acc")),
           contains(paste0(i, ".ind.conf")),
           control, acc_factor, conf_factor, bias_factor,
           contains("diff"), -contains("diffUR"),
           -contains("diffR"), -contains("accUR"),
           -contains("accR"),-contains("confUR"),
           -contains("confR"), -contains("post"),
           -matches(paste0(i, ".bias.diff")), 
           -contains("discrimination"))

  # correlate regression variables with control variables
  x <- vars %>% select(-group) %>% 
    correlate() %>% 
    focus(contains(paste0(i, ".acc.diff")):contains(paste0(i, ".hes.diff")))
  
  # suppress trivial correlations
  x[x > -.15 & x < .15] <- NA
  
  # save correlation matrix as csv
  write_csv(x, paste0("output/regression/", i, "_correlations_factor.csv"))
  
  # prepare data for regression
  x <- vars %>% 
    gather(var, score, -group:-bias_factor) %>% 
    separate(var, c("test", "var", "diff")) %>% 
    group_by(var) %>% nest()

  # fit the regression models
  # trait-accuracy and confidence
  # simple model
  fit <- x %>% 
    mutate(fit = map(data, ~ lm(scale(.x$score) ~ 
                                      scale(.x$acc_factor) + scale(.x$conf_factor))),
           tidy_fit = map(fit, tidy)) %>% 
    unnest(tidy_fit) %>% 
    select(-data, -fit)
  
  fit$p.value[fit$p.value > .1] <- NA
  
  # save results as csv
  write_csv(fit, paste0("output/regression/", i, "_regression_trait_factor_simple.csv"))
  
  # model with controls
  fit <- x %>% 
    mutate(fit = map(data, ~ lm(scale(.x$score) ~ 
                                  scale(.x[2]) + scale(.x[3]) +
                                  scale(.x$acc_factor) + scale(.x$conf_factor))),
           tidy_fit = map(fit, tidy)) %>% 
    unnest(tidy_fit) %>% 
    select(-data, -fit)
  
  fit$p.value[fit$p.value > .1] <- NA
  
  # save results as csv
  write_csv(fit, paste0("output/regression/", i, "_regression_trait_factor_control.csv"))
  
  # bias
  # simple model
  fit <- x %>%
    mutate(fit = map(data, ~ lm(scale(.x$score) ~ 
                                  scale(.x$bias_factor))),
           tidy_fit = map(fit, tidy)) %>%
    unnest(tidy_fit) %>%
    select(-data, -fit)

  fit$p.value[fit$p.value > .1] <- NA

  # save results as csv
  write_csv(fit, paste0("output/regression/", i, "_regression_bias_factor_simple.csv"))
  
  # model with controls
  fit <- x %>%
    mutate(fit = map(data, ~ lm(scale(.x$score) ~ 
                                  scale(.x[2]) + scale(.x[3]) +
                                  scale(.x$bias_factor))),
           tidy_fit = map(fit, tidy)) %>%
    unnest(tidy_fit) %>%
    select(-data, -fit)
  
  fit$p.value[fit$p.value > .1] <- NA
  
  # save results as csv
  write_csv(fit, paste0("output/regression/", i, "_regression_bias_factor_control.csv"))
  
}







# using mean scores ------------------------------------------------------
# load data
# if using factor scores
d.grp <- read_csv("data/190718_dyad_pca.csv")

name <- d.grp %>% select(contains("ind.acc"), contains("ind.conf"),
                      -contains("accR"), -contains("accUR"),
                      -contains("confR"), -contains("confUR")) %>%
               names()

d.grp <- d.grp %>%
  gather(var, val, name) %>%
  separate(var, c("test", "ind", "var")) %>%
  group_by(group) %>%
  mutate(var = str_replace(var, "accO", "acc"),
         var = str_replace(var, "confO", "conf"),
         acc_factor = mean(val[var == "acc"], na.rm = T),
         conf_factor = mean(val[var == "conf"], na.rm = T),
         bias_factor = conf_factor - acc_factor) %>%
  unite(var, test, ind, var, sep = ".") %>%
  spread(var, val) %>%
  ungroup()

# correlations between factor/mean scores
d.grp %>% select(acc_factor, conf_factor, bias_factor) %>% 
  correlate()

d.grp %>% select(acc_factor, contains("ind.acc")) %>% 
  correlate() %>% focus(acc_factor)

d.grp %>% select(conf_factor, contains("ind.conf")) %>% 
  correlate() %>% focus(conf_factor)

d.grp %>% select(bias_factor, contains("ind.acc"), contains("ind.conf")) %>% 
  correlate() %>% focus(bias_factor)

# select control variables for correlations
control <- d.grp %>% 
  select(age, prop.female, aus.born, eng.fl, 
         team.familiarity, wm.acc:neuroticism) %>% 
  names()

# list of test names for loop
tests <- c("adr", "crt", "gk", "mdmt", "rapm")

# simple linear regression: no control variables
for (i in tests) {
  # select the variables for the series of regression models
  vars <- d.grp %>% 
    select(group, control, acc_factor, conf_factor, bias_factor,
           contains(i)) %>% 
    select(group, contains(paste0(i, ".ind.acc")),
           contains(paste0(i, ".ind.conf")),
           control, acc_factor, conf_factor, bias_factor,
           contains("diff"), -contains("diffUR"),
           -contains("diffR"), -contains("accUR"),
           -contains("accR"),-contains("confUR"),
           -contains("confR"), -contains("post"),
           -matches(paste0(i, ".bias.diff")), 
           -contains("discrimination"))
  
  # correlate regression variables with control variables
  x <- vars %>% select(-group) %>% 
    correlate() %>% 
    focus(contains(paste0(i, ".acc.diff")):contains(paste0(i, ".hes.diff")))
  
  # suppress trivial correlations
  x[x > -.15 & x < .15] <- NA
  
  # save correlation matrix as csv
  write_csv(x, paste0("output/regression/", i, "_correlations_mean.csv"))
  
  # prepare data for regression
  x <- vars %>% 
    gather(var, score, -group:-bias_factor) %>% 
    separate(var, c("test", "var", "diff")) %>% 
    group_by(var) %>% nest()
  
  # fit the regression models
  # trait-accuracy and confidence
  # simple model
  fit <- x %>% 
    mutate(fit = map(data, ~ lm(scale(.x$score) ~ 
                                  scale(.x$acc_factor) + scale(.x$conf_factor))),
           tidy_fit = map(fit, tidy)) %>% 
    unnest(tidy_fit) %>% 
    select(-data, -fit)
  
  fit$p.value[fit$p.value > .1] <- NA
  
  # save results as csv
  write_csv(fit, paste0("output/regression/", i, "_regression_trait_mean_simple.csv"))
  
  # model with controls
  fit <- x %>% 
    mutate(fit = map(data, ~ lm(scale(.x$score) ~ 
                                  scale(.x[2]) + scale(.x[3]) +
                                  scale(.x$acc_factor) + scale(.x$conf_factor))),
           tidy_fit = map(fit, tidy)) %>% 
    unnest(tidy_fit) %>% 
    select(-data, -fit)
  
  fit$p.value[fit$p.value > .1] <- NA
  
  # save results as csv
  write_csv(fit, paste0("output/regression/", i, "_regression_trait_mean_control.csv"))
  
  # bias
  # simple model
  fit <- x %>%
    mutate(fit = map(data, ~ lm(scale(.x$score) ~ 
                                  scale(.x$bias_factor))),
           tidy_fit = map(fit, tidy)) %>%
    unnest(tidy_fit) %>%
    select(-data, -fit)
  
  fit$p.value[fit$p.value > .1] <- NA
  
  # save results as csv
  write_csv(fit, paste0("output/regression/", i, "_regression_bias_mean_simple.csv"))
  
  # model with controls
  fit <- x %>%
    mutate(fit = map(data, ~ lm(scale(.x$score) ~ 
                                  scale(.x[2]) + scale(.x[3]) +
                                  scale(.x$bias_factor))),
           tidy_fit = map(fit, tidy)) %>%
    unnest(tidy_fit) %>%
    select(-data, -fit)
  
  fit$p.value[fit$p.value > .1] <- NA
  
  # save results as csv
  write_csv(fit, paste0("output/regression/", i, "_regression_bias_mean_control.csv"))
  
}










# run diagnostics on regression models
vars <- d.grp %>% 
  select(group, control, acc_factor, conf_factor, bias_factor,
         contains("ind.acc"),
         contains("ind.conf"),
         control, acc_factor, conf_factor, bias_factor,
         contains("diff"), -contains("diffUR"),
         -contains("diffR"), -contains("accUR"),
         -contains("accR"),-contains("confUR"),
         -contains("confR"), -contains("post"),
         -matches("bias.diff"), 
         -contains("discrimination"))

# fit the regression models
fit <- lm(scale(adr.conf.diff) ~ scale(adr.ind.acc) + scale(adr.ind.conf) +
                scale(acc_factor) + scale(conf_factor), data = vars)
  

summary(fit)

par(mfrow=c(2,2))
plot(fit)

install.packages("corpcor")
library(corpcor)

tests <- c("adr", "crt", "gk", "mdmt", "rapm")

i <- tests[2]

# x <- vars %>% 
#   select(paste0(i, ".ind.acc"), paste0(i, ".ind.conf"), acc_factor, conf_factor) %>% 
#   na.omit
# 
# cor2pcor(cov(x))

x <- vars %>% 
  select(paste0(i, ".ind.acc"), paste0(i, ".ind.conf"), acc_factor, conf_factor)

omcdiag(x, vars$crt.conf.diff)

imcdiag(x, vars$crt.conf.diff)






# PLAY --------------------------------------------------------------------
# Convert loop to lapply --------------------------------------------------
x <- lapply(tests, function(i) {
  
  # select variables
  vars <- d.grp %>% 
    select(group, control, acc_factor, conf_factor, bias_factor,
           contains(i)) %>% 
    select(group, control, acc_factor, conf_factor, bias_factor,
           contains("diff"), contains(paste0(i, ".ind.acc")),
           contains(paste0(i, ".ind.conf")), -contains("diffUR"),
           -contains("diffR"), -contains("accR"), -contains("accUR"), 
           -contains("confR"), -contains("confUR"), 
           -contains("post"), -matches(paste0(i, ".bias.diff")),
           -contains("discrimination"))
  
  # correlate regression variables with control variables
  x <- vars %>% select(-group) %>% 
    correlate() %>% 
    focus(contains(paste0(i, ".acc.diff")):contains(paste0(i, ".hes.diff")), 
          contains(paste0(i, ".ind.acc")), contains(paste0(i, ".ind.conf")))
  
  # suppress trivial correlations
  x[x > -.15 & x < .15] <- NA
  
  # save correlation matrix as csv
  write_csv(x, paste0("output/regression/", i, "_correlations.csv"))
  
  # prepare data for regression
  x <- vars %>% 
    gather(var, score, paste0(i, ".acc.diff"):paste0(i, ".hes.diff")) %>% 
    separate(var, c("test", "var", "diff")) %>% 
    group_by(var) %>% nest()
  
  # fit the regression models
  # trait-accuracy and confidence
  fit <- x %>% 
    mutate(fit = map(data, ~ lm(scale(.x$score) ~ scale(.x[22]) +
                                  scale(.x[23]) +
                                  scale(.x$acc_factor) + scale(.x$conf_factor))),
           # fit_one = map(data, ~ lm(scale(.x$score) ~ scale(.x$acc_factor) + scale(.x$conf_factor))),
           tidy_fit = map(fit, tidy)) %>% 
    unnest(tidy_fit) %>% 
    select(-data, -fit)
  
  fit$p.value[fit$p.value > .1] <- NA
  
  # save results as csv
  write_csv(fit, paste0("output/regression/", i, "_regression_trait.csv"))
  
  # bias
  fit <- x %>%
    mutate(fit = map(data, ~ lm(scale(.x$score) ~ scale(.x$bias_factor))),
           tidy_fit = map(fit, tidy)) %>%
    unnest(tidy_fit) %>%
    select(-data, -fit)
  
  fit$p.value[fit$p.value > .1] <- NA
  
  # save results as csv
  write_csv(fit, paste0("output/regression/", i, "_regression_bias.csv"))
  
}
)



# OLD CODE ----------------------------------------------------------------
# ADR ---------------------------------------------------------------------
# correlate regression variables with control variables
vars <- d.grp %>% 
  select(group, control, adr.trait.conf, contains("adr")) %>% 
  select(group, control, adr.trait.acc, adr.trait.conf, adr.bias, 
         contains("diff"), -contains("post"), 
         -adr.bias.diff, -adr.discrimination.diff)

x <- vars %>% select(-group) %>% correlate() %>% focus(adr.acc.diff:adr.hes.diff)

x[x > -.2 & x < .2] <- NA

# simple regression model (1 IV)
x <- vars %>% 
  rename(trait.acc = adr.trait.acc, trait.conf = adr.trait.conf, bias = adr.bias) %>%
  gather(var, score, adr.acc.diff:adr.hes.diff) %>% 
  separate(var, c("test", "var", "diff")) %>% 
  group_by(var) %>% nest() 

fit <- x %>% 
  mutate(fit = map(data, ~ lm(scale(.x$score) ~ scale(.x$trait.acc) + scale(.x$trait.conf))),
         tidy_fit = map(fit, tidy)) %>% 
  unnest(tidy_fit)

write_csv(fit, "output/regression/adr_regression_trait.csv")

fit <- x %>% 
  mutate(fit = map(data, ~ lm(scale(.x$score) ~ scale(.x$bias))),
         tidy_fit = map(fit, tidy)) %>% 
  unnest(tidy_fit)

write_csv(fit, "output/regression/adr_regression_bias.csv")



# CRT ---------------------------------------------------------------------
# correlate regression variables with control variables
vars <- d.grp %>% 
  select(group, control, crt.trait.conf, contains("crt")) %>% 
  select(group, control, crt.trait.acc, crt.trait.conf, crt.bias, 
         contains("diff"), -contains("post"), 
         -crt.bias.diff, -crt.discrimination.diff)

x <- vars %>% select(-group) %>% correlate() %>% focus(crt.acc.diff:crt.hes.diff)

x[x > -.2 & x < .2] <- NA

# simple regression model (1 IV)
x <- vars %>% 
  rename(trait.acc = crt.trait.acc, trait.conf = crt.trait.conf, bias = crt.bias) %>%
  gather(var, score, crt.acc.diff:crt.hes.diff) %>% 
  separate(var, c("test", "var", "diff")) %>% 
  group_by(var) %>% nest() 

# trait-accuracy and confidence
fit <- x %>% 
  mutate(fit = map(data, ~ lm(scale(.x$score) ~ scale(.x$trait.acc) + scale(.x$trait.conf))),
         tidy_fit = map(fit, tidy)) %>% 
  unnest(tidy_fit)

write_csv(fit, "output/regression/crt_regression_trait.csv")

# bias
fit <- x %>% 
  mutate(fit = map(data, ~ lm(scale(.x$score) ~ scale(.x$bias))),
         tidy_fit = map(fit, tidy)) %>% 
  unnest(tidy_fit)

write_csv(fit, "output/regression/crt_regression_bias.csv")


# GK ---------------------------------------------------------------------
# correlate regression variables with control variables
vars <- d.grp %>% 
  select(group, control, gk.trait.conf, contains("gk")) %>% 
  select(group, control, gk.trait.acc, gk.trait.conf, gk.bias, 
         contains("diff"), -contains("diffR"), -contains("diffUR"),
         -contains("post"), -gk.bias.diffO, -gk.discrimination.diffO)

x <- vars %>% select(-group) %>% correlate() %>% focus(gk.acc.diffO:gk.hes.diffO)

x[x > -.2 & x < .2] <- NA

# simple regression model (1 IV)
x <- vars %>% 
  rename(trait.acc = gk.trait.acc, trait.conf = gk.trait.conf, bias = gk.bias) %>%
  gather(var, score, gk.acc.diffO:gk.hes.diffO) %>% 
  separate(var, c("test", "var", "diff")) %>% 
  group_by(var) %>% nest() 

# trait-accuracy and confidence
fit <- x %>% 
  mutate(fit = map(data, ~ lm(scale(.x$score) ~ scale(.x$trait.acc) + scale(.x$trait.conf))),
         tidy_fit = map(fit, tidy)) %>% 
  unnest(tidy_fit)

write_csv(fit, "output/regression/gk_regression_trait.csv")

# bias
fit <- x %>% 
  mutate(fit = map(data, ~ lm(scale(.x$score) ~ scale(.x$bias))),
         tidy_fit = map(fit, tidy)) %>% 
  unnest(tidy_fit)

write_csv(fit, "output/regression/gk_regression_bias.csv")


# MDMT ---------------------------------------------------------------------
# correlate regression variables with control variables
vars <- d.grp %>% 
  select(group, control, mdmt.trait.conf, contains("mdmt")) %>% 
  select(group, control, mdmt.trait.acc, mdmt.trait.conf, mdmt.bias, 
         contains("diff"), -contains("post"), -mdmt.bias.diff, 
         -mdmt.discrimination.diff)

x <- vars %>% select(-group) %>% correlate() %>% focus(mdmt.acc.diff:mdmt.hes.diff)

x[x > -.2 & x < .2] <- NA

# simple regression model (1 IV)
x <- vars %>% 
  rename(trait.acc = mdmt.trait.acc, trait.conf = mdmt.trait.conf, bias = mdmt.bias) %>%
  gather(var, score, mdmt.acc.diff:mdmt.hes.diff) %>% 
  separate(var, c("test", "var", "diff")) %>% 
  group_by(var) %>% nest() 

# trait-accuracy and confidence
fit <- x %>% 
  mutate(fit = map(data, ~ lm(scale(.x$score) ~ scale(.x$trait.acc) + scale(.x$trait.conf))),
         tidy_fit = map(fit, tidy)) %>% 
  unnest(tidy_fit)

write_csv(fit, "output/regression/mdmt_regression_trait.csv")

# bias
fit <- x %>% 
  mutate(fit = map(data, ~ lm(scale(.x$score) ~ scale(.x$bias))),
         tidy_fit = map(fit, tidy)) %>% 
  unnest(tidy_fit)

write_csv(fit, "output/regression/mdmt_regression_bias.csv")

# RAPM ---------------------------------------------------------------------
# correlate regression variables with control variables
vars <- d.grp %>% 
  select(group, control, rapm.trait.conf, contains("rapm")) %>% 
  select(group, control, rapm.trait.acc, rapm.trait.conf, rapm.bias, 
         contains("diff"), -contains("post"), -rapm.bias.diff, 
         -rapm.discrimination.diff)

x <- vars %>% select(-group) %>% correlate() %>% focus(rapm.acc.diff:rapm.hes.diff)

x[x > -.2 & x < .2] <- NA

# simple regression model (1 IV)
x <- vars %>% 
  rename(trait.acc = rapm.trait.acc, trait.conf = rapm.trait.conf, bias = rapm.bias) %>%
  gather(var, score, rapm.acc.diff:rapm.hes.diff) %>% 
  separate(var, c("test", "var", "diff")) %>% 
  group_by(var) %>% nest() 

# trait-accuracy and confidence
fit <- x %>% 
  mutate(fit = map(data, ~ lm(scale(.x$score) ~ scale(.x$trait.acc) + scale(.x$trait.conf))),
         tidy_fit = map(fit, tidy)) %>% 
  unnest(tidy_fit)

write_csv(fit, "output/regression/rapm_regression_trait.csv")

# bias
fit <- x %>% 
  mutate(fit = map(data, ~ lm(scale(.x$score) ~ scale(.x$bias))),
         tidy_fit = map(fit, tidy)) %>% 
  unnest(tidy_fit)

write_csv(fit, "output/regression/rapm_regression_bias.csv")















fit <- lm(scale(adr.rec.diff) ~ scale(adr.trait.acc) +
            scale(adr.trait.conf), d.grp)

summary(fit)



d.grp %>%
  ggplot(aes(x = adr.trait.conf, y = adr.rec.diff)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, size = .5, colour = "black", se = FALSE)


d.grp %>%
  ggplot(aes(x = adr.trait.acc, y = adr.trait.conf)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, size = .5, colour = "black", se = FALSE)






range(d.grp$rapm.acc.diff, na.rm = T)


plot(d.grp$adr.trait.acc, d.grp$adr.acc.diff)

d.grp %>%
  ggplot(aes(x = adr.trait.acc, y = adr.rec.diff)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, size = .5, colour = "black", se = FALSE)

