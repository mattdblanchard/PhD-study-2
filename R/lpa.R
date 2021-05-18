# This script is used to conduct LPA on the data for Matthew Blanchard's 2nd PhD study

# list packages
packages <- c("tidyverse", "tidyLPA")

# install any packages not currently installed
if (any(!packages %in% installed.packages())) {
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}

# load packages
lapply(packages, library, character.only = TRUE)

# load data
d <- read_csv("data/210312_dyad_data.csv") %>% 
  rename(prop_female = prop.female, emotion_acc = emotion.acc,
         rapm_ind_acc = rapm.ind.acc, rapm_ind_conf = rapm.ind.conf,
         aus_born = aus.born, eng_fl = eng.fl, team_familiarity = team.familiarity,
         mean_disagree = mean.disagree, wm_acc = wm.acc, risk_aversion = risk.aversion,
         adapt_affective = adapt.affective, adapt_cognitive = adapt.cognitive, 
         adapt_global = adapt.global) %>% 
  filter(!is.na(grp_conf_factor)) %>% # remove groups with missing values for conf and acc factors
  group_by(group) %>%
  mutate(ind_bias_mean = mean(c(adr.ind.bias, crt.ind.bias, rapm.ind.bias)),
         grp_bias_mean = mean(c(adr.grp.bias, crt.grp.bias, rapm.grp.bias))) %>%
  ungroup()
  # mutate(across(c(grp_accuracy_cfa, grp_confidence_cfa, rapm_ind_acc, rapm_ind_conf, ind_bias_mean, grp_bias_mean), scale))

# calculate similarity bw team members for acc, conf, and bias
# 1 = similar and 0 = dissimilar
team_similar <- read.csv("data/210416_uid_data.csv") %>% 
  select(group, adr_acc_team_similar:crt_conf_team_similar, rapm_acc_team_similar:rapm_conf_team_similar) %>% 
  pivot_longer(-group, names_to = "var", values_to = "val") %>% 
  group_by(group, var) %>% 
  summarise(val = val[1]) %>% 
  separate(var, into = c("test", "var", "a", "b")) %>% 
  group_by(group, var) %>% 
  summarise(val = mean(val, na.rm = T)) %>% 
  mutate(val = ifelse(group == "19013010_1", NA, val),
         var = paste0(var, "_team_similar")) %>% 
  pivot_wider(names_from = var, values_from = val)

# merge with d
d <- d %>% left_join(team_similar, by = "group")

# select variables for lpa and standardise
# function to standardise variables
# scale() changes the structure of the tibble and estimate_profiles() won't run
std <- function(x) {
  (x - mean(x))/sd(x)  
}

x <- d %>% 
  select(grp_accuracy_cfa, grp_confidence_cfa, rapm_ind_acc, rapm_ind_conf, ind_bias_mean, grp_bias_mean) %>% 
  mutate(across(everything(), std))

# missing value analysis --------------------------------------------------
# % missing values for each variable
# naniar::gg_miss_var(x, show_pct = TRUE)
x %>% 
  pivot_longer(everything(), names_to = "var", values_to = "val") %>% 
  group_by(var) %>% 
  summarise(n = n(),
            miss = sum(is.na(val)),
            pct = miss/n) %>% 
  ggplot(aes(x = pct, y = var)) +
  geom_point(group = 1) +
  theme_classic()
  
# there can't be any missing values in the dataset for LPA
# so let's impute any missing values
# x <- mclust::imputeData(x) %>% as_tibble()


# LPA ---------------------------------------------------------------------
# identify the number of latent profiles
# there are 4 possible models we can run in R. 
# Each treats the variances and covariances differently
# model 1: variances = "equal",    covariances = "zero
# model 2: variances = "varying",  covariances = "zero"
# model 3: variances = "equal",    covariances = "equal"
# model 6: variances = "varying",  covariances = "varying"

# select the models to fit
mods <- c(1:3,6)

# select the number or profiles to extract
profiles <- c(1:6)

# fit R only models
# load models for 4 vars only (models 1,2,3,6)
# 4 vars == grp_accuracy_cfa, grp_confidence_cfa, rapm_ind_acc, rapm_ind_conf
# model_gfi <- read_rds("data/lpa_fit_all_r_4_vars.rds")
# 6 vars == grp_accuracy_cfa, grp_confidence_cfa, rapm_ind_acc, rapm_ind_conf, ind_bias_mean, grp_bias_mean
model_gfi <- read_rds("data/lpa_fit_all_r_6_vars.rds")

# model_gfi <- x %>%
#   # scale() %>% # standardise variables to aid interpretation from profile plots
#   estimate_profiles(models = mods, # select the types of models to fit
#                     n_profiles = profiles, # set the number of profiles to extract
#   ) %>%
#   compare_solutions(statistics = c("AIC", "BIC", "Entropy")) # select the goodness of fit stats to compare models. See documentation for full list.

# model 6 takes a long time to run so save results to load later
# model_gfi %>% write_rds("data/lpa_fit_all_r_4_vars.rds")


# two additional models can be run if MPlus is installed on your computer
# model 4: variances = "varying", covariances = "equal"
# model 5: variances = "equal", covariances = "varying"
#
# model_gfi <- x %>%
#   scale() %>% # standardise variables to aid interpretation from profile plots
#   estimate_profiles(n_profiles = profiles, # set the number of profiles to extract
#                     models = mods, # set the model types to fit
#                     package = "MplusAutomation") %>%  # set how covariances are treated - determines the model type
#   compare_solutions(statistics = c("AIC", "BIC", "Entropy"))

# print fit indices for all models
gfi <- model_gfi$fits %>%
  select(Model, Classes, AIC, SABIC, BIC, Entropy, LogLik) %>%
  mutate(across(where(is.numeric), round, 3))
  # filter(Model %in% c(1, 2))

# conduct chi square test of goodness of model fit on LogLik difference
nested <- model_gfi$fits %>%
  select(Model, Classes, AIC, BIC, SABIC, Entropy, LogLik) %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  group_by(Model) %>% 
  mutate(diff = abs(LogLik - lag(LogLik)),
         df = 1) %>% 
  filter(Classes != 1) %>% 
  nest(data = AIC:df)

test <- nested %>%
  mutate(fit = map(data, ~ chisq.test(data.frame(.x$diff, .x$df), simulate.p.value = TRUE, B = 5000)),
         tidy_fit = map(fit, broom::tidy)) %>% 
  unnest(tidy_fit) %>% 
  rename(Chi = statistic, df = parameter) %>% 
  mutate(df = 1) %>% 
  select(Model, Classes, df, p.value, Chi)

# combine fit indices
gfi <- gfi %>% 
  left_join(test, by = c("Model", "Classes")) 

gfi %>% 
  filter(Model == 2) %>% 
  write_csv("output/lpa_best_model_2_6_vars_gfi.csv")

# take a closer examination of the best model types
# fit all the models to take a closer look at class membership and plot profiles
# select the number or profiles to extract
# profiles <- c(2:6)
# 
# fit <- map(mods, function(m) {
#   map(profiles, function(p) {
#     fit <- x %>%
#       scale() %>%
#       estimate_profiles(models = m,
#                         n_profiles = p)
#   })
# })

# model 6 takes a long time to run so save results to load later
# fit %>% write_rds("data/lpa_fit_all_r_6_vars_sep_models.rds")

# 4 vars == grp_accuracy_cfa, grp_confidence_cfa, rapm_ind_acc, rapm_ind_conf
# fit <- read_rds("data/lpa_fit_all_r_4_vars_sep_models.rds")
# 6 vars == grp_accuracy_cfa, grp_confidence_cfa, rapm_ind_acc, rapm_ind_conf, ind_bias_mean, grp_bias_mean
fit <- read_rds("data/lpa_fit_all_r_6_vars_sep_models.rds")


# proportion of participants in each class
# index 1 = class 1, index 2 = class 2, 
# index 3 = class 3, index 4 = class 6

# save class membership
classes <- map(c(1:4), function(m) {
  map(1:5, function(p) {
    get_data(fit[[m]][[p]])
  })
})

# proportions for all classes
bind_rows(classes) %>% 
  group_by(model_number, classes_number, Class) %>% 
  summarise(n = n()) %>% 
  group_by(model_number, classes_number) %>% 
  mutate(prop = round(n/sum(n),2)) %>% 
  select(-n) %>%
  pivot_wider(names_from = Class, values_from = prop) %>% 
  filter(model_number == 2)
  
# proportions for model 2 only
bind_rows(classes) %>% 
  group_by(model_number, classes_number, Class) %>% 
  summarise(n = n()) %>% 
  group_by(model_number, classes_number) %>% 
  mutate(prop = round(n/sum(n),2)) %>% 
  filter(model_number == 2 & classes_number == 3) %>% 
  write_csv("output/lpa_prop_member_model_2_class_3.csv")

# Examine the average latent class probabilities for most likely latent class membership
class_probs <- bind_rows(classes) %>% 
  group_by(model_number, classes_number, Class) %>% 
summarise(across(matches("CPROB"), mean),
          across(matches("CPROB"), round, 2))

class_probs %>% 
  filter(model_number == 2, classes_number == 3) %>% 
  write_csv("output/lpa_avg_prob_model_2_class_3.csv")

# create plot of all models fitted
# save profile scores
profile_scores <- map(c(1:4), function(m) {
  map(1:5, function(p) {
    get_estimates(fit[[m]][[p]])
  })
})

bind_rows(profile_scores) %>% 
  filter(Model == 2 & Classes == 3 & Category == "Means" & Parameter == "grp_accuracy_cfa")

# plot profile scores
set_colours <- c("#F8766D", "gold", "#00BF7D", "#00B0F6", "#E76BF3", "purple")

bind_rows(profile_scores) %>%
  filter(Category == "Means" & Model == 2) %>%
  select(Model, Classes, Parameter, Estimate, se, Class) %>%
  mutate(Class = factor(Class),
         Parameter = factor(Parameter, 
                            levels = c("rapm_ind_acc", "grp_accuracy_cfa", "rapm_ind_conf", "grp_confidence_cfa",
                                       "ind_bias_mean", "grp_bias_mean"),
                            labels = c("Individual intelligence", "Collective intelligence", "Individual confidence",
                                       "Collective confidence", "Individual bias", "Collective bias"))) %>% 
  ggplot(aes(x = Parameter, y = Estimate, group = Class)) +
  geom_line(aes(linetype = Class, colour = Class)) +
  scale_colour_manual(values = set_colours) +
  geom_errorbar(aes(ymin = Estimate - 1.96*se, ymax = Estimate + 1.96*se, colour = Class),
                width = .1) +
  facet_grid(Model ~ Classes) +
  labs(x = "Variable",
       y = "Mean") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        strip.background = element_blank())

# save plot
ggsave("output/lpa_model_2_r_bias.png", width = 16, height = 6)

# plot model 3 with 5 profiles for presentation
# highlight one profile and grey out the others
# extract colour palette codes for ggplot defaults
# scales::show_col(scales::hue_pal()(5))
# "#F8766D" "gold" "#00BF7D" "#00B0F6" "#E76BF3"
# set_colours <- c("#F8766D", "lightgrey", "lightgrey", "lightgrey", "lightgrey")
# 
# get critical t-values values for 
mod <- 2
class <- 3

scores <- bind_rows(classes) %>% 
  filter(model_number == mod, classes_number == class) %>% 
  select(Class)

x <- bind_cols(d, scores)

# # save data for SPSS
# x %>% 
#   select(group, Class, grp_accuracy_cfa, grp_confidence_cfa, rapm_ind_acc, rapm_ind_conf, ind_bias_mean, grp_bias_mean) %>% 
#   pivot_longer(grp_accuracy_cfa:grp_bias_mean, names_to = "var", values_to = "val") %>% 
#   unite(var, Class, var) %>% 
#   pivot_wider(names_from = var, values_from = val)
# 
# write_csv("output/spss_mod_3_class_5.csv")

cv <- x %>% 
  count(Class) %>% 
  mutate(cv = abs(qt(.025, n)),
         Class = as.numeric(Class)) %>% 
  select(-n)

bind_rows(profile_scores) %>%
  filter(Category == "Means" & Model == mod & Classes == class) %>%
  select(Model, Classes, Parameter, Estimate, se, Class) %>%
  left_join(cv, by = "Class") %>% 
  group_by(Parameter, Class) %>% 
  mutate(ci = cv*se) %>% 
  mutate(Class = factor(Class),
         Parameter = factor(Parameter, 
                            levels = c("rapm_ind_acc", "grp_accuracy_cfa", "rapm_ind_conf", "grp_confidence_cfa",
                                       "ind_bias_mean", "grp_bias_mean"),
                            labels = c("Individual intelligence", "Collective intelligence", "Individual confidence",
                                       "Collective confidence", "Individual bias", "Collective bias"))) %>% 
  ggplot(aes(x = Parameter, y = Estimate, group = Class)) +
  geom_line(aes(linetype = Class, colour = Class)) +
  scale_colour_manual(values = set_colours) +
  geom_errorbar(aes(ymin = Estimate - ci, ymax = Estimate + ci, colour = Class),
                width = .1) +
  labs(x = "Variable",
       y = "Mean") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        strip.background = element_blank())

# ggsave("output/lpa_model_2_profiles_3_class_1.png", height = 8, width = 12)
ggsave("output/lpa_model_2_profiles_3.png", height = 8, width = 12)


# bind_rows(profile_scores) %>%
#   filter(Category == "Means" & Model == 2 & Classes == 3 & Class == 1) %>%
#   select(Model, Classes, Parameter, Estimate, se, Class) %>% 
#   summarise(high_ind_acc = Estimate[Parameter == "rapm_ind_acc"] + 1.96*se[Parameter == "rapm_ind_acc"],
#          low_ind_acc = Estimate[Parameter == "rapm_ind_acc"] - 1.96*se[Parameter == "rapm_ind_acc"],
#          grp_acc = Estimate[Parameter == "grp_accuracy_cfa"],
#          sig = Estimate[Parameter == "grp_accuracy_cfa"] < high_ind_acc | Estimate[Parameter == "grp_accuracy_cfa"] < low_ind_acc )


# Descriptives ------------------------------------------------------------
ord <- c(# lpa vars
  "rapm_ind_acc","grp_accuracy_cfa","rapm_ind_conf","grp_confidence_cfa","ind_bias_mean","grp_bias_mean",
  # demo
  "age","prop_female",
  # cognitive and social sensitivity
  "wm_acc","emotion_acc",
  # personality
  "agreeableness","conscientiousness","extraversion","intellect","neuroticism","adapt_affective","adapt_cognitive","adapt_global","risk_aversion",
  # team vars
  "speak_ratio","speak_duration","mean_disagree","trust")

x %>% 
  select(Class,rapm_ind_acc,grp_accuracy_cfa,rapm_ind_conf,grp_confidence_cfa,ind_bias_mean,grp_bias_mean,
  age,prop_female,wm_acc,emotion_acc,agreeableness,conscientiousness,extraversion,intellect,neuroticism,adapt_affective,
  adapt_cognitive,adapt_global,risk_aversion,speak_ratio,speak_duration,mean_disagree,trust) %>% 
  pivot_longer(-Class, names_to = "var", values_to = "val") %>% 
  group_by(Class, var) %>% 
  summarise(val = paste0(round(mean(val, na.rm = T),2), " ", "(", round(sd(val, na.rm = T),2), ")")) %>% 
  pivot_wider(names_from = Class, values_from = val) %>% 
  mutate(var = factor(var, levels = ord)) %>%
  arrange(var) %>% 
  write_csv("output/lpa_descriptives_6_external_vars.csv")

# Demographic analyses ----------------------------------------------------
# age, aus.born, eng.fl, team.familiarity
# no differences

x %>% 
  select(prop_female, Class) %>% 
  group_by(prop_female, Class) %>% 
  summarise(n = n()) %>% 
  group_by(Class) %>% 
  mutate(prop = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Class, values_from = prop)

x %>% 
  select(aus_born, Class) %>% 
  group_by(aus_born, Class) %>% 
  summarise(n = n()) %>% 
  group_by(Class) %>% 
  mutate(prop = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Class, values_from = prop)

x %>% 
  select(eng_fl, Class) %>% 
  group_by(eng_fl, Class) %>% 
  summarise(n = n()) %>% 
  group_by(Class) %>% 
  mutate(prop = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Class, values_from = prop)

x %>% 
  select(team_familiarity, Class) %>% 
  group_by(team_familiarity, Class) %>% 
  summarise(n = n()) %>% 
  group_by(Class) %>% 
  mutate(prop = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Class, values_from = prop)

fisher.test(x$aus_born, x$Class, simulate.p.value = TRUE)
fisher.test(x$eng_fl, x$Class, simulate.p.value = TRUE)
fisher.test(x$team_familiarity, x$Class, simulate.p.value = TRUE)

# age
fit <- aov(age ~ factor(Class), data = x)
summary(fit)

x %>% 
  group_by(Class) %>% 
  summarise(m = mean(age),
            sd = sd(age))



# Continuous --------------------------------------------------------------
# mean.disagree, trust, adapt.affective, adapt.cognitive, adapt.global, agreeableness, conscientiousness, 
# extraversion, intellect, neuroticism, risk.aversion, wm.acc

# select variables
name <- x %>%
  select(Class, prop_female, wm_acc:neuroticism, n_speak_turn_variance, speak_total_dur_sum, mean_disagree,
         acc_team_similar:conf_team_similar,
         mdmt.ind.comp, mdmt.ind.optim, mdmt.ind.hes, mdmt.ind.dec, mdmt.ind.rec,
         mdmt.grp.comp, mdmt.grp.optim, mdmt.grp.hes, mdmt.grp.dec, mdmt.grp.rec) %>%
  names()


  
  # make Class a factor
x <- x %>% 
  mutate(Class = factor(Class))

# prepare data for anova
nested <- x %>% 
  select(all_of(name)) %>% 
  pivot_longer(-Class, names_to = "var", values_to = "val") %>% 
  # pivot_wider(names_from = Class, values_from = val) %>% 
  nest(data = -var)

# conduct anova and test contrasts
test <- nested %>% 
  mutate(fit = map(data, ~ aov(.x$val ~ .x$Class, data = .)),
         contrast = map(fit, TukeyHSD),
         tidy_fit = map(fit, broom::tidy),
         tidy_contrast = map(contrast, broom::tidy)) %>% 
  unnest(tidy_fit) %>% 
  select(-data, -fit, -contrast) %>%
  group_by(var) %>%
  mutate(dfB = df[term==".x$Class"],
         dfW = df[term=="Residuals"],
         SSB = sumsq[term==".x$Class"],
         SSW = sumsq[term == "Residuals"],
         MSB = meansq[term == ".x$Class"],
         MSW = meansq[term == "Residuals"]) %>%
  filter(term == ".x$Class")

# calc mean for Class
mean_class <- x %>%
  select(group, all_of(name)) %>% 
  pivot_longer(all_of(name[name!= "Class"]), names_to = "var", values_to = "val") %>% 
  group_by(var, Class) %>% 
  summarise(mean = round(mean(val, na.rm = T), 2),
            sd = round(sd(val, na.rm = T), 2)) %>% 
  pivot_longer(mean:sd, names_to = "par", values_to = "val") %>% 
  unite(par, par, Class) %>% 
  pivot_wider(names_from = par, values_from = val) %>% 
  mutate(mean_1 = paste0(mean_1, " (", sd_1, ")"),
         mean_2 = paste0(mean_2, " (", sd_2, ")"),
         mean_3 = paste0(mean_3, " (", sd_3, ")")
         # mean_4 = paste0(mean_4, " (", sd_4, ")")
         # mean_5 = paste0(mean_5, " (", sd_5, ")")
         ) %>% 
  select(-sd_1, -sd_2)

# calc eta squared
eta <- function(fit) {
  ss.tot <- sum((fit$model[, 1] - mean(fit$model[, 1]))^2)
  ss.res <- sum((fit$residuals)^2)
  ss.eff <- ss.tot - ss.res
  
  ss.eff / ss.tot
}

eta <- map(test$var, function(i) {
  # print(i)
  f <- paste0(i, " ~ Class")
  fit <- aov(as.formula(f), x)
  
  data.frame(var = i,
             eta = eta(fit))
  
})

eta <- bind_rows(eta)


datalist <- list(test, eta, mean_class)

reduce(datalist, left_join, by = "var") %>% 
  select(var, mean_1, mean_2, mean_3, dfB, dfW, statistic, eta, p.value, SSB, SSW, MSB, MSW)
  # write_csv("output/lpa_6_external_vars_anova_results.csv")

# plot profile differences on external variables
# Important CI variables
p <- x %>% 
  select(Class, prop_female, emotion_acc, n_speak_turn_variance, speak_total_dur_sum) %>% 
  pivot_longer(-Class, names_to = "var", values_to = "val") %>% 
  mutate(var = factor(var, levels = c("prop_female", "emotion_acc", "n_speak_turn_variance", "speak_total_dur_sum"),
                      labels = c("Proportion females", "Social sensitivity", "Equality turn taking", "Duration speech"))) %>%
  group_by(Class, var) %>% 
  summarise(mean = mean(val, na.rm = T),
            ci = 1.96 * sd(val, na.rm = T) / sqrt(n()))

p %>% 
  ggplot(aes(x = Class, y = mean, colour = var)) +
  geom_line(aes(group = 1)) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci),
                width = .1) +
  facet_wrap(~var, scales = "free_y", ncol = 4) +
  labs(x = "Profile", title = "Differences between profiles",
       subtitle = "Error bars represent 95% confidence intervals") +
  theme_classic() +
  theme(legend.position="none",
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("output/lpa_ci_6_vars.png")

# variables that sig. differed
sig <- test %>% 
  filter(p.value < .05) %>% 
  filter(!var %in% c("prop_female", "emotion_acc", "n_speak_turn_variance", "speak_total_dur_sum"))

p <- x %>% 
  select(Class, sig$var) %>% 
  pivot_longer(-Class, names_to = "var", values_to = "val") %>% 
  group_by(Class, var) %>% 
  summarise(mean = mean(val, na.rm = T),
            ci = 1.96 * sd(val, na.rm = T) / sqrt(n()))

p %>% 
  ggplot(aes(x = Class, y = mean, colour = var)) +
  geom_line(aes(group = 1)) +
    geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci),
                  width = .1) +
    facet_wrap(~var, scales = "free_y", ncol = 4) +
    labs(x = "Profile", title = "Differences between profiles",
         subtitle = "Error bars represent 95% confidence intervals") +
    theme_classic() +
    theme(legend.position="none",
          strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

ggsave("output/lpa_external_sig_6_vars.png")

# examine ccontrasts for omnibus anovas that were sig.
# 
test %>% 
  filter(p.value < .05) %>% 
  select(var, tidy_contrast) %>% 
  unnest(tidy_contrast) %>% 
  mutate(adj.p.value = ifelse(adj.p.value < .001, "<.001***", 
                       ifelse(adj.p.value >= .001 & adj.p.value < .01, paste0(round(adj.p.value,3), "**"),
                       ifelse(adj.p.value >= .01 & adj.p.value < .05, paste0(round(adj.p.value,3), "*"), 
                              as.character(round(adj.p.value,3))))))


