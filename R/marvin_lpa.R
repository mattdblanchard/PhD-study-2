# load packages
packages <- c("tidyverse", "tidyLPA")

# install any packages not currently installed
if (any(!packages %in% installed.packages())) {
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}

lapply(packages, library, character.only = TRUE)

# set the directory to read data from and save files to
dir <- "matt/"

# load data
d <- read_csv(paste0(dir, "210312_dyad_data.csv")) %>% 
  filter(!is.na(grp_conf_factor)) # remove groups with missing values for conf and acc factors

# select variables for LPA
x <- d %>% select(grp_accuracy_cfa, grp_confidence_cfa, rapm.ind.acc, rapm.ind.conf)


# fit LPA ---------------------------------------------------------------------
# there are 6 possible models we can run
# model 1: variances = "equal",    covariances = "zero
# model 2: variances = "varying",  covariances = "zero"
# model 3: variances = "equal",    covariances = "equal"
# model 4: variances = "varying",  covariances = "equal"
# model 5: variances = "equal",  covariances = "varying"
# model 6: variances = "varying",  covariances = "varying"

# number or profiles to extract
profiles <- c(2:6)

# fit R only models
fit <- x %>%
  scale() %>% # standardise variables to aid interpretation from profile plots
  estimate_profiles(models = c(1:3,6), # select the types of models to fit
                    n_profiles = profiles, # set the number of profiles to extract
                    ) %>% 
  compare_solutions(statistics = c("AIC", "BIC", "Entropy")) # select the goodness of fit stats to compare models. See documentation for full list.

fit %>% write_rds(paste0(dir, "lpa_fit_all_r_only.rds"))


# fit all models
fit <- x %>%
  scale() %>% # standardise variables to aid interpretation from profile plots
  estimate_profiles(models = 1:6, # select the types of models to fit
                    n_profiles = profiles, # set the number of profiles to extract
                    package = "MplusAutomation") %>%  # set how covariances are treated - determines the model type
  compare_solutions(statistics = c("AIC", "BIC", "Entropy")) # select the goodness of fit stats to compare models. See documentation for full list.

fit %>% write_rds(paste0(dir, "lpa_fit_all_mplus.rds"))


# fit models 4 and 5 only
fit <- map(4:5, function(m) {
  map(profiles, function(p) {
    fit <- x %>%
      scale() %>%
      estimate_profiles(models = m,
                        n_profiles = p,
                        package = "MplusAutomation")
  })
})

fit %>% write_rds(paste0(dir, "lpa_fit_model_4_and_5.rds"))


# Save plots --------------------------------------------------------------
# save profile scores for plot
profile_scores <- map(c(1:2), function(m) {
  map(1:5, function(p) {
    get_estimates(fit[[m]][[p]])
  })
})

# create plot
bind_rows(profile_scores) %>%
  filter(Category == "Means") %>%
  select(Model, Classes, Parameter, Estimate, se, Class) %>%
  mutate(Class = factor(Class),
         Parameter = factor(Parameter, 
                            levels = c("rapm.ind.acc", "grp_accuracy_cfa", "rapm.ind.conf", "grp_confidence_cfa"),
                            labels = c("Individual intelligence", "Collective intelligence", "Individual confidence",
                                       "Collective confidence"))) %>% 
  ggplot(aes(x = Parameter, y = Estimate, group = Class)) +
  geom_line(aes(linetype = Class, colour = Class)) +
  geom_errorbar(aes(ymin = Estimate - se, ymax = Estimate + se, colour = Class),
                width = .1) +
  facet_grid(Model ~ Classes) +
  labs(x = "Variable",
       y = "Mean") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        strip.background = element_blank())
  
# save plot
ggsave(paste0(dir, "lpa_model_4_and_5_mplus.png"))

