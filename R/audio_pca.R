# Run PCA on comms vars and save dataset

if (file.exists("data/210226_dyad_data.csv")) {
  d <- read_csv("data/210226_dyad_data.csv")
  } else {

library(tidyverse)
library(corrr)
# combine group data and comms data


# read data
# read in acc and conf factor scores
fac <- read_csv("SPSS_SK/130519_dyad_data_sabina_factor_scores.csv") %>% 
  select(group, ind_conf_factor, ind_acc_factor, grp_conf_factor, grp_acc_factor) %>% 
  drop_na

# join with dataset
d <- read_csv("data/190619_dyad_data.csv") %>% 
  left_join(fac, by = "group") %>% 
  group_by(group) %>% 
  mutate(grp_bias_factor = sum(rapm.grp.bias, adr.grp.bias, crt.grp.bias)/3,
         ind_bias_factor = sum(rapm.ind.bias, adr.ind.bias, crt.ind.bias)/3,
         diff_acc = grp_acc_factor - ind_acc_factor,
         diff_conf = grp_conf_factor - ind_conf_factor,
         diff_bias = grp_bias_factor - ind_bias_factor) %>% 
  ungroup()

source("R/audio_vars.R")

# append group and member vars
audio <- audio %>% 
  mutate(uid = str_replace(uid, "-e", "_e"),
         uid = str_remove(uid, "_p1"),
         group = str_remove(uid, "_[a-z][12]"),
         member = str_extract(uid, "[a-z][12]"))

# aggregate comms data for each group
# total talk time
# mean talk time per turn
# sd talk time per turn
# time ratio(less/more time so 1 = equality of talking time)
# turns ratio(less/more time so 1 = equality of talking turns)
# equality of turn taking (CI Woolley var)

vars <- audio %>% 
  gather(var, val, -uid, -group, -member) %>% 
  mutate(var = str_replace(var, "n_speak", "speak_num")) %>% 
  separate(var, into = c("test", "a", "b", "c")) %>% 
  unite(var, a, b, c) %>% 
  group_by(group, test, var) %>% 
  summarise(mean = mean(val),
            sum = sum(val),
            ratio = min(val)/max(val)) %>% 
  gather(var2, val, mean, sum, ratio) %>% 
  unite(var, var, var2) %>% ungroup() %>% 
  filter(var %in% c("speak_total_dur_sum", "speak_total_dur_ratio", 
                    "speak_num_turn_sum", "speak_num_turn_ratio")) %>% 
  unite(var, test, var) %>% 
  spread(var, val)



# PLAY - fix up 4 ids that have 0 or NA for some speak vars ---------------
# ids <- vars %>% pivot_longer(-group, names_to = "var", values_to = "val") %>% 
#   filter(is.na(val)) %>% 
#   select(group) %>% unique()
# 
# c <- vars %>% pivot_longer(-group, names_to = "var", values_to = "val") %>% 
#   filter(group %in% ids$group) %>% 
#   pivot_wider(names_from = var, values_from = val)
# 
# map(c$group, function(i) {
#   c %>% 
#     filter(group == i) %>% 
#     pivot_longer(-group, names_to = "var", values_to = "val") %>% 
#     filter(val != 0)
# })
# 
# 
# 
# c <- audio %>% select(group, uid, matches("speak_turn|speak_total")) %>% filter(group %in% ids$group)
# 

# END of PLAY -------------------------------------------------------------



d <- d %>% left_join(vars, by = "group")

# relationship between comms vars - mdmt and gk not included
tests <- c("adr", "crt", "rapm")

tmp1 <- d %>% 
  select(group, contains("speak")) %>% 
  gather(var, val, -group) %>% 
  separate(var, into = c("test", "a", "b", "c", "d")) %>% 
  unite(var, a, b, c, d) %>% 
  spread(var, val)

cor <- map(tests, function(i) {
tmp1 %>% 
  filter(test == i) %>% 
    select_if(is.numeric) %>% 
    correlate()
  
})

# # relationship bw comms, accuracy, confidence and post
var <- c("acc", "conf", "post")

map(var, function(i) {
  map(tests, function(t) {
tmp2 <- d %>% 
  select(group, contains("grp")) %>% 
  select(group, contains(i), 
         -contains(paste0(i, "R")), -contains(paste0(i, "UR"))) %>% 
  gather(var, val, -group) %>% 
  separate(var, into = c("test", "a", "b")) %>% 
  select(-a, -b)

  # print(t)
  
tmp2 %>% left_join(tmp1, by = c("group", "test")) %>% 
  filter(test == t) %>% 
  select_if(is.numeric) %>% 
  correlate() %>% focus(val)
})
})


# efa on comms vars
# Conduct EFA to extract comms factors
# Reduce variables to smaller number of factors and correlate with each other. It looks like 3 components can be extracted from the comms variables.

## Correlations between original comms variables
library(corrplot)

# identify participants to remove
ids <- d %>% select(group, contains("speak")) %>%
  pivot_longer(-group, names_to = "var", values_to = "val") %>%
  group_by(group) %>% 
  summarise(na = sum(is.na(val))) %>% 
  filter(na >= 10)

# select original comms variables
pca <- d %>% select(group, contains("speak")) %>% 
  select(-contains("mdmt"), -contains("gk")) %>% 
  filter(!group %in% ids$group) %>% 
  select(-group)

# missing vars analysis
naniar::gg_miss_var(pca, show_pct = TRUE)
naniar::gg_miss_case(pca, show_pct = TRUE)

# impute missing data
pca <- data.frame(mclust::imputeData(pca))

# create the correlation matrix for PCA so we know how it was done (e.g., how missing values were treated)
cor_pca <- cor(pca, use="pairwise.complete.obs")

# print correlations
round(cor_pca, 2)

# Visualise correlations to see if variables appear to cluster
corrplot(cor(pca, use="complete.obs"), order = "hclust", tl.col='black', tl.cex=.75)

## KMO and Bartlett's test of Spherecity
library(psych)

# Kaiser-Meyer-Olkin Measure of Sampling Adequacy (KMO)
print(KMO(cor_pca))

# Bartlett's test of spherecity
print("Bartletts test of spherecity")
print(data.frame(cortest.bartlett(cor_pca, n = 105)))

## Communalities
library(GPArotation)

# 3-component PCA
n_comp <- 2
rotate_method <- "promax"
score_method <- "Bartlett"

fit <- principal(pca, rotate = rotate_method, nfactors = n_comp, 
                 method = score_method, scores = TRUE, n.obs = 55)

# communalities
x <- data.frame(communalities = fit$communality)

x

## Variance explained by the extracted components
x <- data.frame(component = 1:nrow(x),
                eigen = fit$values,
                prop_var = c(fit$Vaccounted[2,c(1:n_comp)], rnorm(nrow(x)-n_comp, 0, 0)),
                cum_var = c(fit$Vaccounted[3,c(1:n_comp)], rnorm(nrow(x)-n_comp, 0, 0)),
                rotation_SS_load = c(fit$Vaccounted[1,c(1:n_comp)], rnorm(nrow(x)-n_comp, 0, 0))) %>% 
  round(2)

x[x == 0] <- ""

x

# Scree plot
p <- data.frame(component = 1:nrow(x),
                eigen = fit$values)

p %>% ggplot(aes(x = component, y = eigen)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = 1:nrow(x)) +
  theme_minimal() +
  labs(title = "Scree plot") +
  theme(plot.title = element_text(hjust = 0.5))


## Pattern matrix
load <- data.frame(var = rownames(fit$loadings),
                   PC1 = round(fit$loadings[1:nrow(x)], 4),
                   PC2 = round(fit$loadings[(nrow(x)+1):(nrow(x)*2)], 4)
                   # PC3 = round(fit$loadings[(nrow(x)*2+1):(nrow(x)*3)], 4)
                   ) %>% 
  mutate(PC1 = ifelse(PC1 < .3 & PC1 > -.3, "", PC1),
         PC2 = ifelse(PC2 < .3 & PC2 > -.3, "", PC2)
         # PC3 = ifelse(PC3 < .3 & PC3 > -.3, "", PC3)
         ) %>% 
  arrange(desc(PC1), desc(PC2)
          # desc(PC3)
          )

load


## Component correlations matrix
# Component correlations matrix
rownames(fit$r.scores) <- c("ratio", "duration")
colnames(fit$r.scores) <- c("ratio", "duration")

round(fit$r.scores,2)

# save component scores as dataframe
pca_scores <- data.frame(fit$scores) %>% 
  rename(speak_ratio = RC1, speak_duration = RC2)

# append component scores to dataset
vars <- cbind(d %>% filter(!group %in% ids$group), pca_scores)

# calculate mean scores for each comms variable and turn taking SD
# old variables
# x <- audio %>% 
#   filter(!group %in% ids$group) %>% 
#   gather(var, val, -uid, -group, -member) %>% 
#   mutate(var = str_replace(var, "n_speak", "speak_num")) %>% 
#   separate(var, into = c("test", "a", "b", "c")) %>% 
#   unite(var, a, b, c) %>% 
#   group_by(group, test, var) %>% 
#   summarise(mean = mean(val),
#             sum = sum(val),
#             ratio = min(val)/max(val)) %>% 
#   gather(var2, val, mean, sum, ratio) %>% 
#   unite(var, var, var2) %>% ungroup() %>% 
#   filter(var %in% c("speak_total_dur_sum", "speak_total_dur_ratio", 
#                     "speak_num_turn_sum", "speak_num_turn_ratio")) %>% 
#   filter(!test %in% c("gk", "mdmt")) %>% 
#   group_by(group, var) %>% 
#   summarise(m = mean(val)) %>% 
#   spread(var, m)

x <- audio %>% 
  select(group, uid, matches("n_speak|total_dur"), -matches("gk|mdmt")) %>% 
  pivot_longer(-group:-uid, names_to = "var", values_to = "val") %>% 
  separate(var, into = c("test", "a", "b", "c")) %>% 
  unite(var, a, b, c) %>% 
  group_by(group, test, var) %>% 
  summarise(sum = sum(val),
            sd = sd(val),
            ratio = min(val)/max(val)) %>% 
  group_by(group, var) %>% 
  summarise(sum = sum(sum),
            variance = mean(sd),
            ratio = mean(ratio)) %>% 
  pivot_longer(-group:-var, names_to = "metric", values_to = "val") %>% 
  unite(var, var, metric) %>% 
  pivot_wider(names_from = var, values_from = val)

vars <- vars %>% left_join(x, by = "group")

vars %>% write_csv("data/210226_dyad_data.csv")

}

mean_sd <- list(mean = ~mean(.x, na.rm = T),
                sd = ~sd(.x, na.rm = T))



# names(vars)
# vars %>% select(ind_conf_factor, ind_acc_factor, grp_conf_factor, grp_acc_factor,
#                 ind_bias_factor, grp_bias_factor, diff_acc, diff_conf, diff_bias,
#                 speak_volume, speak_ratio) %>% 
#   correlate() %>% focus(speak_volume, speak_ratio)
# 
# 
# cor.test(vars$grp_conf_factor, vars$wm.acc, paired = T)
# cor.test(vars$speak_ratio, vars$grp_acc_factor, paired = T)
# 
# 
# 
# 
# names(vars)
# 
# vars %>% 
#   select(age, prop.female, aus.born, eng.fl, team.familiarity, wm.acc:neuroticism,
#          speak_volume, speak_ratio) %>% 
#   correlate() %>% focus(speak_volume, speak_ratio)
# 
# vars %>% 
#   select(age, prop.female, aus.born, eng.fl, team.familiarity, wm.acc:neuroticism,
#          speak_volume, speak_ratio, ind_conf_factor, ind_acc_factor, grp_conf_factor, grp_acc_factor,
#          ind_bias_factor, grp_bias_factor) %>% 
#   correlate() %>% focus(grp_conf_factor, grp_acc_factor, grp_bias_factor, speak_volume, speak_ratio)
# 
# 
# vars %>% 
#   select(grp_acc_factor, speak_volume, speak_ratio) %>% 
#   gather(var1, val1, -grp_acc_factor) %>% 
#   ggplot(aes(x = val1, y = grp_acc_factor)) +
#   geom_smooth() +
#   facet_grid(~var1)
# 
# 
# vars %>% 
#   select(grp_acc_factor, grp_conf_factor, grp_bias_factor, speak_ratio) %>% 
#   gather(var1, val1, -speak_ratio) %>% 
#   ggplot(aes(x = speak_ratio, y = val1)) +
#   geom_smooth() +
#   facet_grid(~var1, scales = "free_y")
# 
# 
# vars %>% 
#   select(grp_acc_factor, grp_conf_factor, grp_bias_factor, speak_volume) %>% 
#   gather(var1, val1, -speak_volume) %>% 
#   ggplot(aes(x = speak_volume, y = val1)) +
#   geom_smooth() +
#   facet_grid(~var1, scales = "free_y")
