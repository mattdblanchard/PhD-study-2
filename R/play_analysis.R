#' *********************************************************************************
#' ************************************* Script 3 **********************************
#' *********************************************************************************
#' 
#' This script is used to conduct preliminary analyses
{library(tidyverse)
library(GGally)
library(naniar)
library(lavaanPlot)
library(lavaan)
library(psych)
library(broom)
library(corrr)
library(ez)
}




d.uid <- read_csv("data/190619_uid_data.csv")
d.grp <- read_csv("data/190619_dyad_data.csv")

# d.uid <- read_csv("data/190513_uid_consensus_data.csv")
# d.grp <- read_csv("data/190513_dyad_consensus_data.csv")

# demographic info
d.uid %>% 
  select(age:aus_born, eng_fl, team_familiarity) %>% 
  psych::describe()

# create df of numeric variable names
numeric_vars_uid <- d.uid %>% select_if(is.numeric) %>% select(-member) %>% names
numeric_vars_grp <- d.grp %>% select_if(is.numeric) %>% names

## Calculate number of missing values for each variable
# individual data
colSums(sapply(d.uid, is.na)) %>% 
  as.data.frame() %>% 
  rename(Missing = ".") %>% 
  tibble::rownames_to_column() %>% 
  arrange(desc(Missing))

# group data
colSums(sapply(d.grp, is.na)) %>% 
  as.data.frame() %>% 
  rename(Missing = ".") %>% 
  tibble::rownames_to_column() %>% 
  arrange(desc(Missing)) %>% 
  filter(str_detect(rowname, "post"))

## Plot the number of missing values for each variable
gg_miss_var(d.uid)

## An alternative method of plotting missing values
gg_miss_upset(d.uid)


# RAPM
d.uid %>% 
  ggpairs(columns = numeric_vars_uid[c(1:14)],
          title = "RAPM")

# function to save ggpairs plots
ggpairs_plot <- function(path, filename, df) {
  corr_plot <- ggpairs(
    df, diag=list(continuous="density"), axisLabels='show')
  png(file.path(path, filename), height=1000, width=1600)
  print(corr_plot)
  dev.off()
}

# RAPM
ggpairs_plot("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/output/", "ggpairs_rapm.png", select(d.uid, numeric_vars_uid[c(1:14)]))
 
# MDMT
ggpairs_plot("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/output/", "ggpairs_mdmt.png", select(d.uid, numeric_vars_uid[c(15:28)]))

# GK
ggpairs_plot("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/output/", "ggpairs_gk.png", 
             select(d.uid, numeric_vars_uid[c(29, 31, 33, 35, 37, 39, 41, 30, 32, 34, 36, 38, 40, 42)]))


# CRT
ggpairs_plot("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/output/", "ggpairs_crt.png", 
             select(d.uid, numeric_vars_uid[c(43, 45, 47, 49, 51, 53, 55, 44, 46, 48, 50, 52, 54, 56)]))

# ADR
ggpairs_plot("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/output/", "ggpairs_adr.png", 
             select(d.uid, numeric_vars_uid[c(57, 59, 61, 63, 65, 67, 69, 58, 60, 62, 64, 66, 68, 70)]))

# R2F
ggpairs_plot("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/output/", "ggpairs_r2f.png", 
             select(d.uid, numeric_vars_uid[c(71:72, 73:86)]))

# Other
# Accuracy
ggpairs_plot("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/output/", "ggpairs_acc.png", 
             select(d.uid, numeric_vars_uid[c(1, 15, 29, 43, 57, 8, 22, 30, 44, 58)]))


# Confidence
ggpairs_plot("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/output/", "ggpairs_conf.png", 
             select(d.uid, numeric_vars_uid[c(2, 16, 31, 45, 59, 9, 23, 32, 46, 60)]))

# Decisiveness
ggpairs_plot("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/output/", "ggpairs_dec.png", 
             select(d.uid, numeric_vars_uid[c(3, 17, 33, 47, 61, 10, 24, 34, 48, 62)]))

# Recklessness
ggpairs_plot("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/output/", "ggpairs_rec.png", 
             select(d.uid, numeric_vars_uid[c(4, 18, 35, 49, 63, 11, 25, 36, 50, 64)]))

# Competence
ggpairs_plot("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/output/", "ggpairs_comp.png", 
             select(d.uid, numeric_vars_uid[c(5, 19, 37, 51, 65, 12, 26, 38, 52, 66)]))

# Optimality
ggpairs_plot("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/output/", "ggpairs_optim.png", 
             select(d.uid, numeric_vars_uid[c(6, 20, 39, 53, 67, 13, 27, 40, 54, 68)]))

# Hesitancy
ggpairs_plot("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/output/", "ggpairs_hes.png", 
             select(d.uid, numeric_vars_uid[c(7, 21, 41, 55, 69, 14, 28, 42, 56, 70)]))

# Other variables
ggpairs_plot("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/output/", "ggpairs_other.png", 
             select(d.uid, numeric_vars_uid[c(73:86)]))


# Let's plot the correlations between all of the variables in the dataset
d_cor <- cor(d.uid[,numeric_vars_uid],
             use = "pairwise.complete.obs")

d_cor <- cor(d.grp[,numeric_vars_grp],
             use = "pairwise.complete.obs")

library(corrplot)

cp <- corrplot(d_cor,
               order = "hclust",
               method = "square",
               tl.col = "black")


# NOT WORKING: attempt to save ggpairs() plots using a for loop
# list_of_vars <- list(
#   numeric_vars_uid[c(1:14)],
#   numeric_vars_uid[c(15:28)],
#   numeric_vars_uid[c(29, 31, 33, 35, 37, 39, 41, 30, 32, 34, 36, 38, 40, 42)],
#   numeric_vars_uid[c(57, 59, 61, 63, 65, 67, 69, 58, 60, 62, 64, 66, 68, 70)],
#   numeric_vars_uid[c(71:72)],
#   numeric_vars_uid[c(1, 15, 29, 43, 57, 8, 22, 30, 44, 58)],
#   numeric_vars_uid[c(2, 16, 31, 45, 59, 9, 23, 32, 46, 60)],
#   numeric_vars_uid[c(3, 17, 33, 47, 61, 10, 24, 34, 48, 62)],
#   numeric_vars_uid[c(4, 18, 35, 49, 63, 11, 25, 36, 50, 64)],
#   numeric_vars_uid[c(5, 19, 37, 51, 65, 12, 26, 38, 52, 66)],
#   numeric_vars_uid[c(6, 20, 39, 53, 67, 13, 27, 40, 54, 68)],
#   numeric_vars_uid[c(7, 21, 41, 55, 69, 14, 28, 42, 56, 70)],
#   numeric_vars_uid[c(73:86)])
# 
# for (i in list_of_vars) {
#   plot <- ggpairs(data = d.uid, 
#                   columns = numeric_vars_uid[[i]],
#                   ggsave(paste("plot_", i, ".png", sep = ""), device = "png", width = (length(i) + 2), height = 10))
#   # dev.off(plot)
# }



# Descriptives ------------------------------------------------------------
# Descriptives for uid variables
# d.uid.desc <- d.uid[, sapply(d.uid, is.numeric)] %>%
#   describe() %>% round(2)

x <- d.uid %>% 
  select(wm.acc, emotion.acc, risk.aversion, 
         trust, adapt.cognitive:adapt.global, agreeableness, conscientiousness, 
         extraversion, intellect, neuroticism)

x <- x[, sapply(x, is.numeric)] %>%
  describe() %>% round(2)

# Descriptives and correlations for grouped variables
# order variables
x <- d.grp %>% 
  select(group:issue_note_2, 
         contains("ind.acc"), contains("grp.acc"), contains("acc.diff"),
         contains("ind.conf"), contains("grp.conf"), contains("conf.diff"),
         contains("ind.bias"), contains("grp.bias"), contains("conf.bias"),
         contains("ind.discrimination"), contains("grp.discrimination"), contains("conf.discrimination"),
         contains("ind.post"), contains("grp.post"), contains("post.diff"),
         contains("ind.dec"), contains("grp.dec"), contains("dec.diff"),
         contains("ind.rec"), contains("grp.rec"), contains("rec.diff"),
         contains("ind.comp"), contains("grp.comp"), contains("comp.diff"),
         contains("ind.optim"), contains("grp.optim"), contains("optim.diff"),
         contains("ind.hes"), contains("grp.hes"), contains("hes.diff"),
         r2f.ind, r2f.grp, age: how_well, wm.acc, emotion.acc, risk.aversion, 
         trust, adapt.affective:adapt.global, agreeableness, conscientiousness, 
         extraversion, intellect, neuroticism)
  # write_csv("output/grouped_data.csv") # saved data in nice order for Sabina

# descriptives for all vars
x[, sapply(x, is.numeric)] %>%
  describe() %>% round(2) %>% 
  mutate(vars = row.names(.)) %>% 
  write_csv("output/descriptives_grouped_data.csv")

# correlations 
# For all vars
corr <- as.data.frame(x[, sapply(x, is.numeric)] %>%
  correlate()) %>% 
  write_csv("output/correlations_all.csv")

corr[corr < .2 & corr > -.2] <- ""

corr %>% write_csv("output/correlations_sig.csv")

# Relationship between accuracy vars across tests
x <- d.grp %>% 
  select(group, recruit, 
         contains("ind.acc"), contains("grp.acc"), contains("acc.diff"))

x <- d.grp %>% 
  select(group, recruit, 
         contains("ind.conf"), contains("grp.conf"), -contains("UR"), -contains("confO"))

corr <- as.data.frame(x[, sapply(x, is.numeric)] %>%
                        correlate() %>% focus(adr.ind.acc:rapm.ind.acc))
  # write_csv("output/cor_accuracy_all.csv")

corr[corr < .2 & corr > -.2] <- ""

# corr %>% write_csv("output/accuracy_sig.csv")

# Relationship between confidence vars across tests
x <- d.grp %>% 
  select(group:issue_note_2, 
         contains("ind.conf"), contains("grp.conf"), contains("conf.diff"),
         age: how_well, wm.acc, emotion.acc, risk.aversion, 
         trust, adapt.affective:adapt.global, agreeableness, conscientiousness, 
         extraversion, intellect, neuroticism)

# For accuracy and confidence vars
x <- d.grp %>% 
  select(group:issue_note_2, 
         contains("ind.acc"), contains("grp.acc"), contains("acc.diff"),
         contains("ind.conf"), contains("grp.conf"), contains("conf.diff"),
         age: how_well, wm.acc, emotion.acc, risk.aversion, 
         trust, adapt.affective:adapt.global, agreeableness, conscientiousness, 
         extraversion, intellect, neuroticism)

# Relationship between bias vars across tests
x <- d.grp %>% 
  select(group, recruit, 
         contains("ind.bias"), contains("grp.bias"), # contains("bias.diff")
         age: how_well, wm.acc, emotion.acc, risk.aversion, 
         trust, adapt.affective:adapt.global, agreeableness, conscientiousness, 
         extraversion, intellect, neuroticism)

corr <- as.data.frame(x[, sapply(x, is.numeric)] %>%
                        correlate() %>% focus(adr.ind.bias:rapm.ind.bias))
# write_csv("output/cor_bias_all.csv")

corr[corr < .2 & corr > -.2] <- ""

# corr %>% write_csv("output/bias_sig.csv")

# Relationship between confidence vars across tests
x <- d.grp %>% 
  select(contains("ind.discrimination"), contains("grp.discrimination"), # contains("conf.diff"),
         age: how_well, wm.acc, emotion.acc, risk.aversion, 
         trust, adapt.affective:adapt.global, agreeableness, conscientiousness, 
         extraversion, intellect, neuroticism)

corr <- as.data.frame(x[, sapply(x, is.numeric)] %>%
                        correlate() %>% focus(adr.ind.discrimination:rapm.ind.discrimination))

corr[corr < .2 & corr > -.2] <- ""


# For POST vars
d.grp.desc <- d.grp %>% 
  select(group:issue_note_2, 
         contains("ind.post"), contains("grp.post"), contains("post.diff"),
         r2f.ind, r2f.grp, age: how_well, wm.acc, emotion.acc, risk.aversion, 
         trust, adapt.affective:adapt.global, agreeableness, conscientiousness, 
         extraversion, intellect, neuroticism)

# For DPA vars
  

# For accurracy and DPA


# For confidence and DPA


# For POST and DPA
# ANOVA -------------------------------------------------------------------
# Function to mean centre data...
standardise <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# function to calculate cohen's d
cohens_d <- function(x, y) {
  lx <- NROW(x)- 1
  ly <- NROW(y)- 1
  md  <- abs(mean(x) - mean(y))        ## mean difference (numerator)
  csd <- (lx * var(x)) + (ly * var(y))
  csd <- csd/(lx + ly)
  csd <- sqrt(csd)                     ## common sd computation
  
  cd  <- md/csd                        ## cohen's d
}

# function to calculate eta squared and partial eta squared
eta_square_two <- function(x){
  group.eta <- fit$ANOVA$ges[3]
  group.part <- fit$ANOVA$SSn[3] / (fit$ANOVA$SSd[3] + fit$ANOVA$SSn[3])
  cc.eta <- fit$ANOVA$ges[2]
  cc.part <- fit$ANOVA$SSn[2] / (fit$ANOVA$SSd[2] + fit$ANOVA$SSn[2])
  interact.eta <- fit$ANOVA$ges[4]
  interact.part <- fit$ANOVA$SSn[4] / (fit$ANOVA$SSd[4] + fit$ANOVA$SSn[4])
  print("grouping eta squared")
  print(group.eta)
  print("grouping partial eta")
  print(group.part)
  print("consensus eta squared")
  print(cc.eta)
  print("consensus partial eta")
  print(cc.part)
  print("interaction eta squared")
  print(interact.eta)
  print("interaction partial eta")
  print(interact.part)
}

# eta_square_three <- function(x){
#   group.eta <- fit$ANOVA$ges[2]
#   group.part <- fit$ANOVA$SSn[2] / (fit$ANOVA$SSd[2] + fit$ANOVA$SSn[2])
#   consensus.eta <- fit$ANOVA$ges[3]
#   consensus.part <- fit$ANOVA$SSn[3] / (fit$ANOVA$SSd[3] + fit$ANOVA$SSn[3])
#   cc.eta <- fit$ANOVA$ges[4]
#   cc.part <- fit$ANOVA$SSn[4] / (fit$ANOVA$SSd[4] + fit$ANOVA$SSn[4])
#   consensusXgroup.eta <- fit$ANOVA$ges[5]
#   consensusXgroup.part <- fit$ANOVA$SSn[5] / (fit$ANOVA$SSd[5] + fit$ANOVA$SSn[5])
#   ccXgroup.eta <- fit$ANOVA$ges[6]
#   ccXgroup.part <- fit$ANOVA$SSn[6] / (fit$ANOVA$SSd[6] + fit$ANOVA$SSn[6])
#   consensusXcc.eta <- fit$ANOVA$ges[7]
#   consensusXcc.part <- fit$ANOVA$SSn[7] / (fit$ANOVA$SSd[7] + fit$ANOVA$SSn[7])
#   all_interact.eta <- fit$ANOVA$ges[8]
#   all_interact.part <- fit$ANOVA$SSn[8] / (fit$ANOVA$SSd[8] + fit$ANOVA$SSn[8])
#   print("grouping eta squared")
#   print(group.eta)
#   print("grouping partial eta")
#   print(group.part)
#   print("consensus eta squared")
#   print(consensus.eta)
#   print("consensus partial eta")
#   print(consensus.part)
#   print("cc eta squared")
#   print(cc.eta)
#   print("cc partial eta")
#   print(cc.part)
#   print("consensus x grouping eta squared")
#   print(consensusXgroup.eta)
#   print("consensus x grouping eta")
#   print(consensusXgroup.part)
#   print("grouping x CC eta squared")
#   print(ccXgroup.eta)
#   print("grouping x CC partial eta")
#   print(ccXgroup.part)
#   print("consensus x CC eta squared")
#   print(consensusXcc.eta)
#   print("consensus x CC partial eta")
#   print(consensusXcc.part)
#   print("Three way interaction eta squared")
#   print(all_interact.eta)
#   print("Three way interaction partial eta")
#   print(all_interact.part)
# }

pd <- position_dodge(width = 0.2)

# plot individuals vs real dyads
# prepare data
plot <- d.grp %>%
  ungroup() %>% 
  select(group, contains("mdmt"), contains("rapm"), -contains("diff")) %>% 
  gather(var, score, -group) %>% 
  separate(var, c("test", "grouping", "var")) %>% 
  group_by(test, grouping, var) %>% 
  summarise(mean = mean(score, na.rm = T),
            mean_ci = 1.96 * sd(score, na.rm = T) / sqrt(n())) %>% 
  ungroup() %>% 
  mutate(
         grouping = factor(grouping, levels = c("ind", "grp"), 
                           labels = c("Individuals", "Real dyads")),
         var = factor(var, levels = c("acc", "conf", "post", "dec", "rec", "comp", "optim", "hes"), 
                      labels = c("Accuracy", "Confidence", "POST", "Decisiveness", "Recklessness", "Competence", "Optimality", "Hesitancy")))

plot %>% 
  # filter(var == "Accuracy") %>% 
  ggplot(aes(x = grouping, y = mean)) +
  facet_wrap(var ~ test, ncol = 4, scales = "free_y") +
  geom_point(size = 4, position = pd) +
  geom_point(size = 3, colour = "white", position = pd) +
  geom_line(aes(group = 1), position = pd) +
  geom_errorbar(aes(ymin = mean - mean_ci, ymax = mean + mean_ci),
                width = .1, position = pd) +
  # guides(linetype = guide_legend(" ")) +
  labs(x = "Answers given by",
       y = "Mean") +
  theme(
    panel.background = element_rect(colour = "white", fill = "white"), # Set 
    strip.background = element_rect(colour="white", fill="white"),
    strip.text.x = element_text(size = 18, face="bold"),
    # plot background to white
    legend.text = element_text(size = 10, face = "bold"), 
    legend.key = element_rect(colour = "white", fill = "white"), # Set 
    # legend item backgrounds to white
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 12, colour = "black"),
    axis.line.x = element_line(colour = "black", size = .5), # Add
    # line to x axis
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12, colour = "black"),
    axis.line.y = element_line(colour = "black", size = .5) # Add line to y axis
  ) +
  scale_y_continuous(labels = scales::comma)
  # ggsave("output/ANOVA_rapm_mdmt.png", width = 20, height = 20)


# consensus variables
plot <- d.grp %>%
  ungroup() %>% 
  select(group, contains("adr"), contains("crt"), contains("gk"), -contains("diff")) %>% 
  gather(var, score, -group) %>% 
  separate(var, c("test", "grouping", "var", "consensus")) %>% 
  group_by(test, grouping, var, consensus) %>% 
  summarise(mean = mean(score, na.rm = T),
            mean_ci = 1.96 * sd(score, na.rm = T) / sqrt(n())) %>% 
  ungroup() %>% 
  mutate(
    consensus = factor(consensus, levels = c("agree", "disagree"),
                       labels = c("Agreement", "Disagreement")),
    grouping = factor(grouping, levels = c("ind", "grp"), 
                      labels = c("Individuals", "Real dyads")),
    var = factor(var, levels = c("acc", "accO", "accR", "accUR", "conf",  "confO", "confR", "confUR", "post", "postO", "postR", "postUR", 
                                 "dec", "decO", "decR", "decUR", "rec", "recO", "recR", "recUR", "comp", "compO", "compR", "compUR", 
                                 "optim", "optimO", "optimR", "optimUR", "hes", "hesO", "hesR", "hesUR"),
                 labels = c("Accuracy", "Accuracy Overall", "Accuracy Representative", "Accuracy Non-representative", 
                            "Confidence", "Confidence Overall", "Confidence Representative", "Confidence Non-representative",
                            "POST", "POST Overall", "POST Representative", "POST Non-representative",
                            "Decisiveness", "Decisiveness Overall", "Decisiveness Representative", "Decisiveness Non-representative",
                            "Recklessness", "Recklessness Overall", "Recklessness Representative", "Recklessness Non-representative",
                            "Competence", "Competence Overall", "Competence Representative", "Competence Non-representative",
                            "Optimality", "Optimality Overall", "Optimality Representative", "Optimality Non-representative", 
                            "Hesitancy", "Hesitancy Overall", "Hesitancy Representative", "Hesitancy Non-representative")))
    
plot %>% 
  # filter(var == "Accuracy") %>% 
  ggplot(aes(x = grouping, y = mean, group = consensus)) +
  facet_wrap(var ~ test, ncol = 4, scales = "free_y") +
  geom_point(size = 4, position = pd) +
  geom_point(size = 3, colour = "white", position = pd) +
  geom_line(aes(linetype = consensus), position = pd) +
  geom_errorbar(aes(ymin = mean - mean_ci, ymax = mean + mean_ci),
                width = .1, position = pd) +
  guides(linetype = guide_legend(" ")) +
  labs(x = "Answers given by",
       y = "Mean") +
  theme(
    panel.background = element_rect(colour = "white", fill = "white"), # Set 
    strip.background = element_rect(colour="white", fill="white"),
    strip.text.x = element_text(size = 18, face="bold"),
    # plot background to white
    legend.text = element_text(size = 10, face = "bold"), 
    legend.key = element_rect(colour = "white", fill = "white"), # Set 
    # legend item backgrounds to white
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 12, colour = "black"),
    axis.line.x = element_line(colour = "black", size = .5), # Add
    # line to x axis
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12, colour = "black"),
    axis.line.y = element_line(colour = "black", size = .5) # Add line to y axis
  ) +
  scale_y_continuous(labels = scales::comma)
  # ggsave("output/ANOVA_consensus.png", width = 20, height = 20)




# 2 WAY ANOVA
# conduct ANOVA using nested data
# individuals vs real dyads
# in paper reported recklessness results for separately run ANOVA 
# because this method here removed a few more NA rows
x <- d.grp %>%
  ungroup() %>% 
  select(group, contains("adr"), contains("crt"), contains("gk"), -contains("diff")) %>% 
  gather(var, score, -group) %>% 
  separate(var, c("test", "grouping", "var", "consensus")) %>% 
  mutate(
    group = factor(group),
    consensus = factor(consensus, levels = c("agree", "disagree"),
                       labels = c("Agreement", "Disagreement")),
    grouping = factor(grouping, levels = c("ind", "grp"), 
                      labels = c("Individuals", "Real dyads")),
    var = factor(var, levels = c("acc", "accO", "accR", "accUR", "conf",  "confO", "confR", "confUR", "post", "postO", "postR", "postUR", 
                                 "dec", "decO", "decR", "decUR", "rec", "recO", "recR", "recUR", "comp", "compO", "compR", "compUR", 
                                 "optim", "optimO", "optimR", "optimUR", "hes", "hesO", "hesR", "hesUR"),
                 labels = c("Accuracy", "Accuracy Overall", "Accuracy Representative", "Accuracy Non-representative", 
                            "Confidence", "Confidence Overall", "Confidence Representative", "Confidence Non-representative",
                            "POST", "POST Overall", "POST Representative", "POST Non-representative",
                            "Decisiveness", "Decisiveness Overall", "Decisiveness Representative", "Decisiveness Non-representative",
                            "Recklessness", "Recklessness Overall", "Recklessness Representative", "Recklessness Non-representative",
                            "Competence", "Competence Overall", "Competence Representative", "Competence Non-representative",
                            "Optimality", "Optimality Overall", "Optimality Representative", "Optimality Non-representative", 
                            "Hesitancy", "Hesitancy Overall", "Hesitancy Representative", "Hesitancy Non-representative"))) %>% 
  group_by(test, var) %>% 
  nest()

fit <- x %>% 
  mutate(fit = map(data, ~ aov(.x$score ~ .x$grouping * .x$consensus +
                                 Error(.x$group / (.x$grouping * .x$consensus)))),
         tidy_fit = map(fit, tidy)) %>% 
  unnest(tidy_fit) %>% 
  filter(stratum != ".x$group", stratum != "Within") %>% 
  filter(term != "Residuals") %>% 
  select(test, var, stratum, term, statistic, p.value)
# filter(stratum == ".x$group:.x$consensus" & term == ".x$consensus")

fit %>% write_csv("output/ANOVA_consensus_results.csv")



# exploratory factor analysis ---------------------------------------------
d.uid <- read_csv("data/190520_uid_data.csv")
d.grp <- read_csv("data/190520_dyad_data.csv")


ind <- d.uid %>% 
  dplyr::select(contains("ind")) %>% dplyr::select(contains("acc"), contains("conf"), -contains("gk"))

grp <- d.grp %>% 
  dplyr::select(contains("grp")) %>% dplyr::select(contains("acc"), contains("conf"), -contains("gk"))

# both <- d.grp %>% 
#   dplyr::select(contains("acc"), contains("conf")) %>% na.omit()

# Determine Number of Factors to Extract
library(nFactors)

# individuals
ev <- eigen(cor(na.omit(ind))) # get eigenvalues
ap <- parallel(subject=nrow(na.omit(ind)),var=ncol(na.omit(ind)),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) 

factanal(na.omit(ind), factors = 3, rotation = "varimax")

# groups
ev <- eigen(cor(na.omit(grp))) # get eigenvalues
ap <- parallel(subject=nrow(na.omit(grp)),var=ncol(na.omit(grp)),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) 

factanal(na.omit(grp), factors = 2, rotation = "varimax")

# individuals & groups
ev <- eigen(cor(both)) # get eigenvalues
ap <- parallel(subject=nrow(both),var=ncol(both),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) 

factanal(both, factors = 2, rotation = "varimax")





# Confirmatory factor analysis --------------------------------------------
# used this tutorial: http://lavaan.ugent.be/tutorial/cfa.html
library(knitr)
# individual responses (individual-level data)
# 1-factor model
cognitive =~ adr.ind.acc + crt.ind.acc + rapm.ind.acc + adr.ind.conf + crt.ind.conf + rapm.ind.conf

# enter the model syntax
CI.model <- 'cognitive  =~ adr.ind.acc + crt.ind.acc + rapm.ind.acc + 
             adr.ind.conf + crt.ind.conf + rapm.ind.conf'

# fit the model
fit_one <- cfa(CI.model, data=d.uid, missing = "ML")

# display summary output
summary(fit_one, fit.measures=TRUE)

fitMeasures(fit_one, fit.measures= c("chisq", "df", "pvalue", "gfi", "tli", 
                                 "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "aic"))

# look at the correlation between the residuals
residuals(fit_one, type = "cor")

# individual responses (individual-level data)
# 2-factor model
# 2 latent factors for accuracy and confidence
accuracy =~ adr.ind.acc + crt.ind.acc + rapm.ind.acc
confidence =~ adr.ind.conf + crt.ind.conf + rapm.ind.conf

# enter the model syntax
CI.model <- 'accuracy  =~ adr.ind.acc + crt.ind.acc + rapm.ind.acc 
              confidence =~ adr.ind.conf + crt.ind.conf + rapm.ind.conf'

# fit the model
fit_two <- cfa(CI.model, data=d.uid, missing = "ML")

# display summary output
summary(fit_two, fit.measures=TRUE)

fitMeasures(fit_two, fit.measures= c("chisq", "df", "pvalue", "gfi", "tli", 
                                 "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "aic"))

# look at the correlation between the residuals
# ideally all correlations are below .1
residuals(fit_two, type = "cor")

# test the difference between the models
anova(fit_one, fit_two)      


# set up the model
CI.model <- '
accuracy  =~ adr.ind.acc + crt.ind.acc + rapm.ind.acc 
confidence =~ adr.ind.conf + crt.ind.conf + rapm.ind.conf

# correlate residuals
adr.ind.acc ~~ adr.ind.conf
crt.ind.acc ~~ crt.ind.conf
rapm.ind.acc ~~ rapm.ind.conf
'

# fit the model
fit_two <- cfa(CI.model, data=d.uid, missing = "ML")

# display summary output
summary(fit_two, fit.measures=TRUE)

fitMeasures(fit_two, fit.measures= c("chisq", "df", "pvalue", "gfi", "tli", 
                                     "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "aic"))

# look at the correlation between the residuals
# ideally all correlations are below .1
residuals(fit_two, type = "cor")

# test the difference between the models
anova(fit_one, fit_two)            


# predict factor scores for each participant
d.uid <- d.uid %>% cbind(data.frame(predict(fit_two))) %>% 
  rename(ind.accuracy = accuracy,
         ind.confidence = confidence)


# x <- d.uid %>% select(wm.acc:how_well, ind.accuracy, ind.confidence) %>% 
#   correlate() %>% focus(ind.accuracy, ind.confidence)
# 
# x[x > -.2 & x < .2] <- NA

cor.test(d.uid$ind.accuracy, d.uid$ind.confidence)







# group responses (individual-level data)
# 1-factor model
cognitive =~ adr.grp.acc + crt.grp.acc + rapm.grp.acc + adr.grp.conf + crt.grp.conf + rapm.grp.conf

# enter the model syntax
CI.model <- 'cognitive  =~ adr.grp.acc + crt.grp.acc + rapm.grp.acc + 
adr.grp.conf + crt.grp.conf + rapm.grp.conf'

# fit the model
fit_one <- cfa(CI.model, data=d.uid, missing = "ML")

# display summary output
summary(fit_one, fit.measures=TRUE)

fitMeasures(fit_one, fit.measures= c("chisq", "df", "pvalue", "gfi", "tli", 
                                     "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "aic"))

# look at the correlation between the residuals
residuals(fit_one, type = "cor")

# 2-factor model
# 2 latent factors for accuracy and confidence
accuracy =~ adr.grp.acc + crt.grp.acc + rapm.grp.acc
confidence =~ adr.grp.conf + crt.grp.conf + rapm.grp.conf

# enter the model syntax
CI.model <- 'accuracy  =~ adr.grp.acc + crt.grp.acc + rapm.grp.acc 
confidence =~ adr.grp.conf + crt.grp.conf + rapm.grp.conf'

# fit the model
fit_two <- cfa(CI.model, data=d.uid, missing = "ML")

# display summary output
summary(fit_two, fit.measures=TRUE)

fitMeasures(fit_two, fit.measures= c("chisq", "df", "pvalue", "gfi", "tli", 
                                     "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "aic"))

# look at the correlation between the residuals
# ideally all correlations are below .1
residuals(fit_two, type = "cor")

# test the difference between the models
anova(fit_one, fit_two) 

# correlate residuals
# enter the model syntax
CI.model <- 'accuracy  =~ adr.grp.acc + crt.grp.acc + rapm.grp.acc 
confidence =~ adr.grp.conf + crt.grp.conf + rapm.grp.conf
# correlate residuals
adr.grp.acc ~~ adr.grp.conf
crt.grp.acc ~~ crt.grp.conf
rapm.grp.acc ~~ rapm.grp.conf
'

# fit the model
fit_two <- cfa(CI.model, data=d.uid, missing = "ML")

# display summary output
summary(fit_two, fit.measures=TRUE)

fitMeasures(fit_two, fit.measures= c("chisq", "df", "pvalue", "gfi", "tli", 
                                     "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "aic"))


# predict factor scores for each participant
d.uid <- d.uid %>% cbind(data.frame(predict(fit_two))) %>% 
  rename(grp.accuracy = accuracy,
         grp.confidence = confidence)

# save factor scores to datafile
# d.uid %>% write_csv("data/190521_uid_data.csv")

# x <- d.uid %>% select(wm.acc:how_well, ind.accuracy, ind.confidence, grp.accuracy, grp.confidence) %>% 
#   correlate() %>% focus(ind.accuracy, grp.accuracy, ind.confidence, grp.confidence)
# 
# x[x > -.2 & x < .2] <- NA
# 
# cor.test(d.uid$grp.accuracy, d.uid$grp.confidence)


# Factor loadings
parameterEstimates(fit_two, standardized=TRUE) %>%  
  filter(op == "=~") %>% 
  mutate(stars = ifelse(pvalue < .001, "***", 
                        ifelse(pvalue < .01, "**", 
                               ifelse(pvalue < .05, "*", "")))) %>%
  select('Latent Factor'=lhs, 
         Indicator=rhs, 
         B=est, 
         SE=se, Z=z, 
         Beta=std.all, 
         sig=stars) %>% 
  kable(digits = 3, format="pandoc", caption="Table 2: Factor Loadings")


# Latent Factor Correlations
parameterEstimates(fit_two, standardized=TRUE) %>% 
  filter(op == "~~", 
         lhs %in% c("accuracy", "confidence"), 
         !is.na(pvalue)) %>% 
  mutate(stars = ifelse(pvalue < .001, "***", 
                        ifelse(pvalue < .01, "**", 
                               ifelse(pvalue < .05, "*", "")))) %>% 
  select('Factor 1'=lhs, 
         'Factor 2'=rhs, 
         Correlation=est, 
         sig=stars) %>% 
  kable(digits = 3, format="pandoc", caption="Table 3: Latent Factor Correlations")













# individual responses (dyad-level data)
# 1-factor model
cognitive =~ adr.ind.acc + crt.ind.acc + rapm.ind.acc + adr.ind.conf + crt.ind.conf + rapm.ind.conf

# enter the model syntax
CI.model <- 'cognitive  =~ adr.ind.acc + crt.ind.acc + rapm.ind.acc + 
adr.ind.conf + crt.ind.conf + rapm.ind.conf'

# fit the model
fit_one <- cfa(CI.model, data=d.grp, missing = "ML")

summary(fit_one, fit.measures=TRUE)

fitMeasures(fit_one, fit.measures= c("chisq", "df", "pvalue", "gfi", "tli", 
                                     "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "aic"))

# look at the correlation between the residuals
residuals(fit_one, type = "cor")

# 2-factor model
# 2 latent factors for accuracy and confidence
accuracy =~ adr.ind.acc + crt.ind.acc + rapm.ind.acc
confidence =~ adr.ind.conf + crt.ind.conf + rapm.ind.conf

# enter the model syntax
CI.model <- 'accuracy  =~ adr.ind.acc + crt.ind.acc + rapm.ind.acc 
confidence =~ adr.ind.conf + crt.ind.conf + rapm.ind.conf'

# fit the model
fit_two <- cfa(CI.model, data=d.grp, missing = "ML")

# display summary output
summary(fit_two, fit.measures=TRUE)

fitMeasures(fit_two, fit.measures= c("chisq", "df", "pvalue", "gfi", "tli", 
                                     "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "aic"))

# look at the correlation between the residuals
# ideally all correlations are below .1
residuals(fit_two, type = "cor")

# test the difference between the models
anova(fit_one, fit_two)       


# enter the model syntax
CI.model <- 'accuracy  =~ adr.ind.acc + crt.ind.acc + rapm.ind.acc 
confidence =~ adr.ind.conf + crt.ind.conf + rapm.ind.conf
adr.ind.acc ~~ adr.ind.conf
crt.ind.acc ~~ crt.ind.conf
rapm.ind.acc ~~ rapm.ind.conf'

# fit the model
fit_two <- cfa(CI.model, data=d.grp, missing = "ML")

# display summary output
summary(fit_two, fit.measures=TRUE)

fitMeasures(fit_two, fit.measures= c("chisq", "df", "pvalue", "gfi", "tli", 
                                     "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "aic"))

# predict factor scores for each participant
d.grp <- d.grp %>% cbind(data.frame(predict(fit_two))) %>% 
  rename(ind.accuracy = accuracy,
         ind.confidence = confidence)

# # set labels for plot
# labels <- list(adr.ind.acc = "Accuracy ADR", crt.ind.acc = "Accuracy CRT", rapm.ind.acc = "Accuracy CRT", adr.ind.conf = "Confidence ADR", crt.ind.conf = "Confidence ADR", rapm.ind.conf = "Confidence ADR")
# 
# # create plot
# lavaanPlot("two_factor_model", fit_two, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE)
# 
# resid(fit_two)


# group responses (dyad-level data)
# 1-factor model
cognitive =~ adr.grp.acc + crt.grp.acc + rapm.grp.acc + adr.grp.conf + crt.grp.conf + rapm.grp.conf

# enter the model syntax
CI.model <- 'cognitive  =~ adr.grp.acc + crt.grp.acc + rapm.grp.acc + 
adr.grp.conf + crt.grp.conf + rapm.grp.conf'

# fit the model
fit_one <- cfa(CI.model, data=d.grp, missing = "ML")

# display summary output
summary(fit_one, fit.measures=TRUE)

fitMeasures(fit_one, fit.measures= c("chisq", "df", "pvalue", "gfi", "tli", 
                                     "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "aic"))

# look at the correlation between the residuals
residuals(fit_one, type = "cor")

# 2-factor model
# 2 latent factors for accuracy and confidence
accuracy =~ adr.grp.acc + crt.grp.acc + rapm.grp.acc
confidence =~ adr.grp.conf + crt.grp.conf + rapm.grp.conf

# enter the model syntax
CI.model <- 'accuracy  =~ adr.grp.acc + crt.grp.acc + rapm.grp.acc 
confidence =~ adr.grp.conf + crt.grp.conf + rapm.grp.conf'

# fit the model
fit_two <- cfa(CI.model, data=d.grp, missing = "ML")

# display summary output
summary(fit_two, fit.measures=TRUE)

fitMeasures(fit_two, fit.measures= c("chisq", "df", "pvalue", "gfi", "tli", 
                                     "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "aic"))

# look at the correlation between the residuals
# ideally all correlations are below .1
residuals(fit_two, type = "cor")

# test the difference between the models
anova(fit_one, fit_two)       


# enter the model syntax
CI.model <- 'accuracy  =~ adr.grp.acc + crt.grp.acc + rapm.grp.acc 
confidence =~ adr.grp.conf + crt.grp.conf + rapm.grp.conf
# correlate residuals
adr.grp.acc ~~ adr.grp.conf
crt.grp.acc ~~ crt.grp.conf
rapm.grp.acc ~~ rapm.grp.conf'

# fit the model
fit_two <- cfa(CI.model, data=d.grp, missing = "ML")

# display summary output
summary(fit_two, fit.measures=TRUE)

fitMeasures(fit_two, fit.measures= c("chisq", "df", "pvalue", "gfi", "tli", 
                                     "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "aic"))

# predict factor scores for each participant
d.grp <- d.grp %>% cbind(data.frame(predict(fit_two))) %>% 
  rename(grp.accuracy = accuracy,
         grp.confidence = confidence)

# save factor scores to datafile
# d.grp %>% write_csv("data/190521_dyad_data.csv")

citation("lavaan") # here's the citation for your current version of lavaan

## 
## To cite lavaan in publications use:
## 
##   Yves Rosseel (2012). lavaan: An R Package for Structural
##   Equation Modeling. Journal of Statistical Software, 48(2), 1-36.
##   URL http://www.jstatsoft.org/v48/i02/.


# examine correlations between factor scores and other vars
# individual-level data

x <- d.uid %>% select(age:how_well, mean_disagree, wm.acc:neuroticism, ind.accuracy:grp.confidence) %>% 
  correlate() %>% focus(ind.accuracy, ind.confidence, grp.accuracy, grp.confidence)

x[x > -.2 & x < .2] <- NA

d.uid %>% select(ind.accuracy:grp.confidence) %>% 
  correlate()


# dyad-level data
x <- d.grp %>% select(age:how_well, mean_disagree, wm.acc:grp.confidence) %>% 
  correlate() %>% focus(ind.accuracy, ind.confidence, grp.accuracy, grp.confidence)

x[x > -.2 & x < .2] <- NA

d.grp %>% select(ind.accuracy:grp.confidence) %>% 
  correlate()


# look at correlations with groups that consistently performed better than individual members
x <- d.uid %>% 
  group_by(uid) %>% 
  mutate(collective_benefit = grp.accuracy > ind.accuracy) %>% 
  group_by(group) %>% 
  mutate(both_benefit = all(collective_benefit)) %>%
  ungroup() %>% 
  filter(both_benefit == TRUE) %>% 
  select(age:how_well, mean_disagree, wm.acc:neuroticism, ind.accuracy:grp.confidence) %>% 
  correlate() %>% focus(ind.accuracy:grp.confidence)

x[x > -.2 & x < .2] <- NA
















