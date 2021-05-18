# This script conducts the same analyses as Woolley et al. 2010

# load packages
packages <- c("tidyverse", "psych", "knitr", "kableExtra", "lavaan", "lavaanPlot")

if (any(!packages %in% installed.packages())) { # install any packages not currently installed
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
} 

lapply(packages, library, character.only = TRUE)

# load functions for PCA
source("R/pca_functions.R")

# load datasets
d.uid <- read_csv("data/210416_uid_data.csv")
d.grp <- read_csv("data/210226_dyad_data.csv")


# check assumptions -------------------------------------------------------
# normality
d.grp %>% 
  select(adr.ind.acc, crt.ind.acc, rapm.ind.acc, 
         adr.ind.conf, crt.ind.conf, rapm.ind.conf) %>% 
  pivot_longer(everything(), names_to = "var", values_to = "val") %>% 
  ggplot(aes(x = val, fill = var)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(colour = "blue", lwd = .7, alpha = 0) +
  facet_wrap(~var) +
  theme_classic() +
  theme(legend.position = "none")
  

# EFA ---------------------------------------------------------------------
# Individual intelligence -------------------------------------------------------------
# select variables
x <- d.uid %>% 
  select(adr.ind.acc, crt.ind.acc, rapm.ind.acc)

# print correlations
corstarsl(x)

# Kaiser-Meyer-Olkin Measure of Sampling Adequacy (KMO)
print(KMO(cor(x, use="pairwise.complete.obs")))

# Bartlett's test of spherecity
print("Bartletts test of spherecity")
print(data.frame(cortest.bartlett(cor(x, use="pairwise.complete.obs"), n = 1295)))

# Scree plot
scree(x)

# 1-component PCA
n_comp <- 1
rotate_method <- "promax"
score_method <- "Bartlett"

fit <- principal(x, rotate = rotate_method, nfactors = n_comp,
                 method = score_method, scores = TRUE)

# variance explained
var_table()

# pattern matrix
pattern_matrix()

# save component scores as dataframe
pca_scores <- data.frame(fit$scores) %>%
  rename(ind_acc_factor = PC1)

# add component scores to d
d.uid <- d.uid %>% bind_cols(pca_scores)

# calculate average and max intelligence for each group
ind_intel <- d.uid %>% 
  group_by(group) %>% 
  summarise(avg_ind_acc = mean(ind_acc_factor),
         max_ind_acc = max(ind_acc_factor))


# Collective intelligence -------------------------------------------------
# select variables
x <- d.grp %>% 
  select(adr.grp.acc, crt.grp.acc, rapm.grp.acc)

# print correlations
corstarsl(x)

# Kaiser-Meyer-Olkin Measure of Sampling Adequacy (KMO)
print(KMO(cor(x, use="pairwise.complete.obs")))

# Bartlett's test of spherecity
print("Bartletts test of spherecity")
print(data.frame(cortest.bartlett(cor(x, use="pairwise.complete.obs"), n = 1295)))

# Scree plot
scree(x)

# 2-component PCA
n_comp <- 1
rotate_method <- "promax"
score_method <- "Bartlett"

fit <- principal(x, rotate = rotate_method, nfactors = n_comp,
                 method = score_method, scores = TRUE)

# variance explained
var_table()

# pattern matrix
pattern_matrix()

# save component scores as dataframe
pca_scores <- data.frame(fit$scores) %>%
  rename(grp_acc_factor = PC1)

# add component scores to d
d.grp <- d.grp %>% 
  select(-grp_acc_factor) %>% 
  bind_cols(pca_scores)

# add average and max individual intelligence
d.grp <- d.grp %>% left_join(ind_intel, by = "group")

# correlations bw ind and grp intelligence
corstarsl(d.grp %>% select(grp_acc_factor, avg_ind_acc, max_ind_acc))


# Individual confidence -------------------------------------------------------------
# select variables
x <- d.uid %>% 
  select(adr.ind.conf, crt.ind.conf, rapm.ind.conf)

# print correlations
corstarsl(x)

# Kaiser-Meyer-Olkin Measure of Sampling Adequacy (KMO)
print(KMO(cor(x, use="pairwise.complete.obs")))

# Bartlett's test of spherecity
print("Bartletts test of spherecity")
print(data.frame(cortest.bartlett(cor(x, use="pairwise.complete.obs"), n = 1295)))

# Scree plot
scree(x)

# 1-component PCA
n_comp <- 1
rotate_method <- "promax"
score_method <- "Bartlett"

fit <- principal(x, rotate = rotate_method, nfactors = n_comp,
                 method = score_method, scores = TRUE)

# variance explained
var_table()

# pattern matrix
pattern_matrix()

# save component scores as dataframe
pca_scores <- data.frame(fit$scores) %>%
  rename(ind_conf_factor = PC1)

# add component scores to d
d.uid <- d.uid %>% bind_cols(pca_scores)

# calculate average and max intelligence for each group
ind_conf <- d.uid %>% 
  group_by(group) %>% 
  summarise(avg_ind_conf = mean(ind_conf_factor),
            max_ind_conf = max(ind_conf_factor))

# Collective confidence ---------------------------------------------------
# select variables
x <- d.grp %>% 
  select(adr.grp.conf, crt.grp.conf, rapm.grp.conf)

# print correlations
corstarsl(x)

# Kaiser-Meyer-Olkin Measure of Sampling Adequacy (KMO)
print(KMO(cor(x, use="pairwise.complete.obs")))

# Bartlett's test of spherecity
print("Bartletts test of spherecity")
print(data.frame(cortest.bartlett(cor(x, use="pairwise.complete.obs"), n = 1295)))

# Scree plot
scree(x)

# 2-component PCA
n_comp <- 1
rotate_method <- "promax"
score_method <- "Bartlett"

fit <- principal(x, rotate = rotate_method, nfactors = n_comp,
                 method = score_method, scores = TRUE)

# variance explained
var_table()

# pattern matrix
pattern_matrix()

# save component scores as dataframe
pca_scores <- data.frame(fit$scores) %>%
  rename(grp_conf_factor = PC1)

# add component scores to d
d.grp <- d.grp %>% 
  select(-grp_conf_factor) %>% 
  bind_cols(pca_scores)

# add average and max individual intelligence
d.grp <- d.grp %>% left_join(ind_conf, by = "group")

# correlations bw ind and grp confidence
corstarsl(d.grp %>% select(grp_conf_factor, avg_ind_conf, max_ind_conf))


# CFA ---------------------------------------------------------------------
# functions
clean_table <- function(data, title) {
  data %>% 
    kable(booktabs = T, caption = title) %>%
    kable_styling(font_size = 12, latex_options = "HOLD_position")
}

# print goodness of fit indices
gfi <- function(fit_data) {
  x <- data.frame(fitMeasures(fit_data, fit.measures= c("chisq", "df", "pvalue", "gfi", "tli", 
                                                        "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "aic"))) %>%
    rownames_to_column()
  
  names(x) <- c("index", "value")  
  
  x %>% 
    mutate(value = round(value, 3))
    # clean_table("Goodness of fit indices")
}

# individual intelligence and confidence
# plot missing values for each group for CFA variables
d.grp %>% 
  select(group, adr.ind.acc, crt.ind.acc, rapm.ind.acc, 
         adr.ind.conf, crt.ind.conf, rapm.ind.conf) %>%
  pivot_longer(-group, names_to = "var", values_to = "val") %>% 
  filter(!is.na(val)) %>% 
  mutate(n = factor(1)) %>% 
  ggplot(aes(x = group, y = var, fill = n)) +
  geom_tile() +
  scale_fill_manual(values = "steelblue1") + 
  labs(title = "group frequency in each variable") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# print missing values for each participant for CFA variables
d.grp %>% 
  select(group, adr.ind.acc, crt.ind.acc, rapm.ind.acc, 
         adr.ind.conf, crt.ind.conf, rapm.ind.conf) %>% 
  group_by(group) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>% 
  group_by(group) %>% 
  summarise(n = sum(adr.ind.acc, crt.ind.acc, rapm.ind.acc, 
                    adr.ind.conf, crt.ind.conf, rapm.ind.conf)) %>% 
  filter(n != 0)




# Fit one factor model (correlated)
cfa_one_corr <- paste0('
                ci  =~ adr.ind.acc + crt.ind.acc + rapm.ind.acc +
                        adr.ind.conf + crt.ind.conf + rapm.ind.conf
                        
                adr.ind.acc ~~ adr.ind.conf
                crt.ind.acc ~~ crt.ind.conf
                rapm.ind.acc ~~ rapm.ind.conf
                ')

fit_one_corr <- cfa(cfa_one_corr, data = d.uid, missing = "ML")

# display summary output
summary(fit_one_corr, fit.measures=TRUE)
gfi(fit_one_corr)

# look at the correlation between the residuals (ideally everything should be <= .1)
residuals(fit_one_corr, type = "cor")$cov

# Fit two factor model (correlated)
cfa_two_corr <- paste0('
                accuracy  =~  adr.ind.acc + crt.ind.acc + rapm.ind.acc
                confidence =~ adr.ind.conf + crt.ind.conf + rapm.ind.conf

                adr.ind.acc ~~ adr.ind.conf
                crt.ind.acc ~~ crt.ind.conf
                rapm.ind.acc ~~ rapm.ind.conf
                     ')

fit_two_corr <- lavaan::cfa(cfa_two_corr, data = d.grp, missing = "ML")

cor(d.grp$rapm.ind.acc, d.grp$rapm.ind.conf, use = "pairwise.complete.obs")

# display summary output
# summary(fit_two_corr, fit.measures=TRUE, standardized=TRUE)
gfi(fit_two_corr)

# print regression coefficients
parameterEstimates(fit_two_corr, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% 
  knitr::kable(digits = 3, booktabs=TRUE, format="markdown", caption="Factor Loadings")

parameterEstimates(fit_two_corr, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('latent_factor'=lhs, indicator=rhs, beta=std.all) %>% 
  pivot_wider(names_from = latent_factor, values_from = beta)



# look at the correlation between the residuals (ideally everything should be <= .1)
# these residual correlations drive RMSEA so if you have too many corrs > .1 this index will be poor
residuals(fit_two_corr, type = "cor")$cov


# plot the residuals
# extract the residuals from the fit1 model
# get rid of the duplicates and diagonal values
# create a vector for a
res1 <- residuals(fit_two_corr, type = "cor")$cov
res1[upper.tri(res1,diag=T)] <- NA
v1 <- as.vector(res1)
v2  <- v1[!is.na(v1)]
car::qqPlot(v2,id=F)

# modification indices
kable(modificationIndices(fit_two_corr, sort.=TRUE, minimum.value=3), booktabs=TRUE, format="markdown")

semPlot::semPaths(fit_two_corr, residuals=F,sizeMan=7,"std",
         posCol=c("skyblue4", "red"),
         #edge.color="skyblue4",
         edge.label.cex=1.2,layout="circle2")

# test the difference between the models
anova(fit_one_corr, fit_two_corr) %>% clean_table("Test model difference")


# collective intelligence and confidence
# plot missing values for each group for CFA variables
d.grp %>% 
  select(group, adr.grp.acc, crt.grp.acc, rapm.grp.acc, 
         adr.grp.conf, crt.grp.conf, rapm.grp.conf) %>%
  pivot_longer(-group, names_to = "var", values_to = "val") %>% 
  filter(!is.na(val)) %>% 
  mutate(n = factor(1)) %>% 
  ggplot(aes(x = group, y = var, fill = n)) +
  geom_tile() +
  scale_fill_manual(values = "steelblue1") + 
  labs(title = "group frequency in each variable") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# print missing values for each participant for CFA variables
d.grp %>% 
  select(group, adr.grp.acc, crt.grp.acc, rapm.grp.acc, 
         adr.grp.conf, crt.grp.conf, rapm.grp.conf) %>% 
  group_by(group) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>% 
  group_by(group) %>% 
  summarise(n = sum(adr.grp.acc, crt.grp.acc, rapm.grp.acc, 
                    adr.grp.conf, crt.grp.conf, rapm.grp.conf)) %>% 
  filter(n != 0)

# Fit one factor model (correlated)
cfa_one_corr <- paste0('
                ci  =~ adr.grp.acc + crt.grp.acc + rapm.grp.acc +
                        adr.grp.conf + crt.grp.conf + rapm.grp.conf
                        
                adr.grp.acc ~~ adr.grp.conf
                crt.grp.acc ~~ crt.grp.conf
                rapm.grp.acc ~~ rapm.grp.conf
                ')

fit_one_corr <- cfa(cfa_one_corr, data = d.uid, missing = "ML")

# display summary output
# summary(fit_one_corr, fit.measures=TRUE)
gfi(fit_one_corr)

# look at the correlation between the residuals (ideally everything should be <= .1)
residuals(fit_one_corr, type = "cor")$cov

# Fit two factor model (correlated)
# ~~ allows the residuals to correlate
cfa_two_corr <- paste0('
                accuracy  =~ adr.grp.acc + crt.grp.acc + rapm.grp.acc 
                confidence =~ adr.grp.conf + crt.grp.conf + rapm.grp.conf

                adr.grp.acc ~~ adr.grp.conf
                crt.grp.acc ~~ crt.grp.conf
                rapm.grp.acc ~~ rapm.grp.conf
                     ')


fit_two_corr <- cfa(cfa_two_corr, data = d.grp, std.lv=TRUE, missing="FIML")

# display summary output
# summary(fit_two_corr, fit.measures=TRUE, standardized=TRUE)
gfi(fit_two_corr)

# look at the correlation between the residuals (ideally everything should be <= .1)
residuals(fit_two_corr, type = "cor")$cov

# test the difference between the models
anova(fit_one_corr, fit_two_corr) %>% clean_table("Test model difference")





# Regression analyses -----------------------------------------------------
# same analysis as Woolley
fit <- lm(grp_acc_factor ~ scale(emotion.acc) + scale(n_speak_turn_variance) + scale(prop.female), d.grp)

summary(fit)

# ind intelligence predict collective intelligence
fit <- lm(grp_acc_factor ~ scale(avg_ind_acc), d.grp)

summary(fit)

# same analysis as Woolley but controlling for individual intelligence
fit <- lm(grp_acc_factor ~ scale(rapm.ind.acc) + # scale(wm.acc) +
            scale(emotion.acc) + 
            scale(n_speak_turn_variance) + scale(prop.female), d.grp)

summary(fit)





# same analysis as Woolley but using collective confidence
fit <- lm(grp_conf_factor ~ scale(emotion.acc) + scale(n_speak_turn_variance) + scale(prop.female), d.grp)

summary(fit)

# ind confidence predict collective confidence
fit <- lm(grp_conf_factor ~ scale(rapm.ind.conf), d.grp)

summary(fit)

# controlling for individual confidence
fit <- lm(grp_conf_factor ~ scale(rapm.ind.conf) + scale(emotion.acc) + 
            scale(n_speak_turn_sum) + scale(prop.female), d.grp)

summary(fit)
