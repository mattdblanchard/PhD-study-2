#' *********************************************************************************
#' ************************************* Script 2 **********************************
#' *********************************************************************************
#' 
#' This script is used to complete final manual cleaning based on session notes and 
#' prepare and save individual-level data (each row contains the data for an individual participant)
#' and dyad-level data (each row contains the data for a team of two participants - for individual responses 
#' this is the average of both team members' individual responses in the individual-level dataset)

library(tidyverse)

# create list of file paths to check if files exist
file <- c("data/190619_uid_data.csv", "data/190619_dyad_data.csv")

# read in the cleaned data
if (all(file.exists(file))) {
  d.uid <- read_csv("data/190619_uid_data.csv")
  d.grp <- read_csv("data/190619_dyad_data.csv")
  
} else {
  
# item-level cleaning and variable calculation for each measure
# consent and demographics have been excluded for now
source("R/import_data.R")

scripts <- list.files("R/survey_prep/", pattern = ".R")[c(-2, -5, -6, -9, -22)]

for (i in scripts) {
  source(paste0("R/survey_prep/", i))
}

# merge all data into a single df
datalist <- list(rapm_ind.uid, rapm_team.uid, mdmt_ind.uid, mdmt_team.uid,  
                 gk.uid, crt.uid, adr.uid, r2f.uid, run.uid, cet.uid, 
                 adapt.uid, risk.uid, trust.uid, ipip.uid, demo)

# combine test vars into single df
# d <- Reduce(function(x, y) merge(x, y), datalist)
d.uid <- datalist %>% reduce(left_join, by = "uid")

# session notes reveal incorrectly recorded uids
# group == 18073109_1 recorded as sona recruits (g2, d2) when they were externally recruited (e1, e2)
# group == 17040610_1 recorded as participating in 2017 but were actually 2018
# group == 1800509_1 recorded as having participated in May (05) when they participated in June (06)
# group == 18050712_1 recorded as having participated in May (05) when they participated in June (06)
# group == 18073110_2 recorded as sona recruits (g1, d1) when they were externally recruited (e1, e2)
# group == 18073114_1 recorded as sona recruits (g1, d1) when they were externally recruited (e1, e2)
d.uid <- d.uid %>% 
  mutate(uid = ifelse(uid == "18073109_1_g2", "18073109_1_e1",
                      ifelse(uid == "18073109_1_d2", "18073109_1_e2", 
                             ifelse(uid == "17040610_1_d2", "18040610_1_d2",
                                    ifelse(uid == "17040610_1_g2", "18040610_1_g2", 
                                           ifelse(uid == "18050509_1_e1", "18060509_1_e1", 
                                                  ifelse(uid == "18050509_1_e2", "18060509_1_e2",
                                                         ifelse(uid == "18050712_1_e1", "18060712_1_e1", 
                                                                ifelse(uid == "18050712_1_e2", "18060712_1_e2", 
                                                                       ifelse(uid == "18073110_2_d1", "18073110_2_e2",
                                                                              ifelse(uid == "18073110_2_g1", "18073110_2_e1", 
                                                                                     ifelse(uid == "18073114_1_d2", "18073114_1_e2",
                                                                                            ifelse(uid == "18073114_1_g2", "18073114_1_e1", uid)))))))))))),
         group = ifelse(group == "18050509_1", "18060509_1", 
                        ifelse(group == "17040610_1", "18040610_1", 
                               ifelse(group == "18050712_1", "18060712_1", group))),
         recruit = ifelse(group == "18073109_1", "ext",
                          ifelse(group == "18073110_2", "ext",
                                        ifelse(group == "18073114_1", "ext", recruit))))

# Count the number of UIDs in each test
as.data.frame(sapply(datalist, count))

# check which participants have missing data for each test ----------------
# # participants that did not complete the protocol and are candidates for removal
exclude <- c("18041210_2_g1", "18041210_2_d1", "18080915_1_e1", "18080915_1_e2", "18072412_1_e1", "18072412_1_e2",
             "19013010_1_g1", "19013010_1_d1", "17102710_2_d1", "17102710_2_g1")
 
# # create list of all participants to check against
uids <- rapm_ind.uid %>% select(uid) %>%
  filter(!uid %in% exclude)
#   # filter(!uid %in% mod$uid)
 
# # identify the participants that completed the protocol before the ethics mod was approved
# # the mod added adapt, risk, trust to the protocol
mod <- uids %>% filter(!uid %in% adapt.uid$uid)
 
# # change the test df to check which participants are missing
# uids %>% filter(!uid %in% risk.uid$uid)


# create issue and issue_note variables to detail any issues --------------
# add "issue" and "issue_note_1" column to data
d.uid <- d.uid %>% mutate(
  issue = FALSE,
  issue.note.1 = ifelse(issue == TRUE, "", NA))

# add notes about issues in dataset
d.uid <- d.uid %>% 
  mutate(issue = ifelse(group == "18041210_2", TRUE, 
                        ifelse(group == "18080915_1", TRUE,
                               ifelse(group == "18072412_1", TRUE,
                                      ifelse(group == "19013010_1", TRUE, 
                                             ifelse(group == "17102710_2", TRUE,
                                                    ifelse(group %in% c("18080213_2", "18080610_1", "18080913_1"), TRUE,
                                                           ifelse(group == "18110716_1", TRUE,
                                                                  ifelse(group == "18110810_2", TRUE,
                                                                         ifelse(group == "18081413_1", TRUE,
                                                                                ifelse(group == "18081316_1", TRUE, 
                                                                                       ifelse(group == "18081613_1", TRUE, issue))))))))))),
         issue.note.1 = ifelse(group == "18041210_2", "Protocol incomplete: due to computer issue data is missing for GK, ADR, R2F, CRT, Adapt, Risk, and Trust. Removed RAPM data for now because they completed RAPM_team as individuals and RAPM_ind as a team", 
                               ifelse(group == "18080915_1", "Protocol incomplete: team had to leave after 2 hours. Did not complete GK, ADR, R2F, trust, CET, ipip",
                                      ifelse(group == "18072412_1", "Protocol incomplete: qualtrics surveys crashed part way through protocol. Did not complete R2F, ADR, GK, CRT, trust, risk and adapt",
                                             ifelse(group == "19013010_1", "Protocol incomplete: team had to leave after 2 hours. Did not complete CRT, ADR, R2F, CET, trust, ipip", 
                                                    ifelse(group == "17102710_2", "Protocol incomplete: d1 had to leave after 2 hours. d1 did not complete ADR, R2F, CET, adapt, risk, trust, ipip.", 
                                                           ifelse(group %in% c("18080213_2", "18080610_1", "18080913_1"), "Participants had to leave so skipped R2F",
                                                                  ifelse(group == "18110716_1", "Participants had to leave so both members skipped R2F. g2 also skipped CET",
                                                                         ifelse(group == "18110810_2", "Participants had to leave so both members skipped R2F and CET",
                                                                                ifelse(group == "18081413_1", "Participants had to leave so both members skipped R2F and CET",
                                                                                       ifelse(group == "18081316_1", "e1 had to leave so they skipped CET. e2 completed everything",
                                                                                              ifelse(uid %in% mod$uid, "no data for adapt, risk and trust because hadn't been added to the protocol", 
                                                                                                     ifelse(group == "18081613_1", "Non-genuine attempt: e2 sent an email apologising they mentioned that their teammate kept racing ahead (guessing) and putting pressure on them to speed up. It wasn't for all tasks just R2F, GK and potentially ADR", issue.note.1)))))))))))))

# remove ADR responses for group == 17102710_2
# One member had to leave and the other completed this task on thier own thus the data is meaningless
# select adr variables
adr_vars <- names(d.uid %>% select(contains("adr")))

# replace values with NA
d.uid[d.uid$group == "17102710_2", adr_vars] <- NA

# remove teams with major issues ------------------------------------------
# That is, teams that missed many of the tasks
# this removes 3 teams leaving 119 teams
d.uid <- d.uid %>% filter(!group %in% c("18041210_2", "18072412_1", "18081613_1"))

# Final clean based on session notes --------------------------------------
# group == 18110810_2 responded to most individual ADR items together as a team need to NA individual responses
# vars <- c("adr.ind.acc", "adr.ind.conf", "adr.ind.dec", "adr.ind.rec",
#           "adr.ind.comp","adr.ind.optim", "adr.ind.hes")
vars <- adr.uid %>% 
  ungroup() %>% 
  select(contains("adr")) %>% 
  select(contains("ind")) %>% 
  names()
 
d.uid[d.uid$group == "18110810_2", vars] <- NA

# group == 17110114_2 responded to most individual GK items together as a team need to NA individual responses
# vars <- c("gk.ind.acc", "gk.ind.conf", "gk.ind.dec", "gk.ind.rec",
#           "gk.ind.comp","gk.ind.optim", "gk.ind.hes")
vars <- gk.uid %>% 
  ungroup() %>% 
  select(contains("gk")) %>% 
  select(contains("ind")) %>% 
  names()

d.uid[d.uid$group == "17110114_2", vars] <- NA

# Several groups provided confidence ratings for group tasks together instead of individually need to NA for these tasks
d.uid[d.uid$group == "18080213_1", "mdmt.grp.conf"] <- NA
d.uid[d.uid$group == "18110716_1", "rapm.grp.conf"] <- NA

# group == "18081613_1" did not discuss responses for R2F_neg so remove R2F
d.uid[d.uid$group == "18081613_1", c("r2f.ind", "r2f.grp")] <- NA

# record teams with issues and a description
d.uid <- d.uid %>% 
  mutate(issue.note.2 = NA) %>% 
  mutate(issue = ifelse(group == "18110810_2", TRUE,
                        ifelse(group == "17110114_2", TRUE,
                               ifelse(group == "18080213_1", TRUE,
                                      ifelse(group == "18110716_1", TRUE,
                                             ifelse(group == "18081613_1", TRUE, issue))))),
         issue.note.2 = ifelse(group == "18110810_2", "Confused instructions: answered most individual ADR items together as team. NA data for individual responses to this test",
                             ifelse(group == "17110114_2", "Confused instructions: answered most individual GK items together as team. NA data for individual responses to this test",
                                    ifelse(group == "18080213_1", "Confused instructions: answered MDMT_team confidence together. NA data for this variable in this test only",
                                           ifelse(group == "18110716_1", "Confused instructions: answered RAPM_team confidence together. NA data for this variable in this test only",
                                                  ifelse(group == "18081613_1", "Fatigued: participants did not discuss responses for R2F_neg so removed R2F responses", issue.note.2))))))

# To see issue notes for teams with issues
# check <- d.uid %>% filter(issue == TRUE) %>% select(group, issue_note_1, issue_note_2)

# # add demographic info
# d.uid <- d.uid %>% left_join(demo %>% select(-member, -recruit, -group), by = "uid")

# calculate mean disagreement
d.uid <- d.uid %>% 
  gather(var, val, adr.disagree, crt.disagree, gk.disagree) %>% 
  group_by(uid) %>% 
  mutate(mean.disagree = mean(val, na.rm = TRUE)) %>% 
  spread(var, val)

# calculate trait confidence and bias across tests for regression
# compute mean confidence and bias for all tests except the one being predicted
adr <- d.uid %>% 
  select(uid, crt.ind.conf, gk.ind.confO, mdmt.ind.conf, rapm.ind.conf,
         crt.ind.acc, gk.ind.accO, mdmt.ind.acc, rapm.ind.acc) %>% 
  gather(var, val, -uid) %>% 
  separate(var, c("test", "ind", "var")) %>% 
  mutate(var = str_replace(var, "O", "")) %>%
  group_by(uid) %>% 
  summarise(adr.trait.acc = mean(val[var == "acc"], na.rm = T),
            adr.trait.conf = mean(val[var == "conf"], na.rm = T),
            adr.bias = mean(val[var == "conf"], na.rm = TRUE) - mean(val[var == "acc"], na.rm = TRUE))

crt <- d.uid %>% 
  select(uid, adr.ind.conf, gk.ind.confO, mdmt.ind.conf, rapm.ind.conf,
         adr.ind.acc, gk.ind.accO, mdmt.ind.acc, rapm.ind.acc) %>% 
  gather(var, val, -uid) %>% 
  separate(var, c("test", "ind", "var")) %>% 
  mutate(var = str_replace(var, "O", "")) %>%
  group_by(uid) %>% 
  summarise(crt.trait.acc = mean(val[var == "acc"], na.rm = T),
            crt.trait.conf = mean(val[var == "conf"], na.rm = T),
            crt.bias = mean(val[var == "conf"], na.rm = TRUE) - mean(val[var == "acc"], na.rm = TRUE))

gk <- d.uid %>% 
  select(uid, adr.ind.conf, crt.ind.conf, mdmt.ind.conf, rapm.ind.conf,
         adr.ind.acc, crt.ind.acc, mdmt.ind.acc, rapm.ind.acc) %>% 
  gather(var, val, -uid) %>% 
  separate(var, c("test", "ind", "var")) %>% 
  mutate(var = str_replace(var, "O", "")) %>%
  group_by(uid) %>% 
  summarise(gk.trait.acc = mean(val[var == "acc"], na.rm = T),
            gk.trait.conf = mean(val[var == "conf"], na.rm = T),
            gk.bias = mean(val[var == "conf"], na.rm = TRUE) - mean(val[var == "acc"], na.rm = TRUE))

mdmt <- d.uid %>% 
  select(uid, adr.ind.conf, crt.ind.conf, gk.ind.confO, rapm.ind.conf,
         adr.ind.acc, crt.ind.acc, gk.ind.accO, rapm.ind.acc) %>% 
  gather(var, val, -uid) %>% 
  separate(var, c("test", "ind", "var")) %>%
  mutate(var = str_replace(var, "O", "")) %>%
  group_by(uid) %>% 
  summarise(mdmt.trait.acc = mean(val[var == "acc"], na.rm = T),
            mdmt.trait.conf = mean(val[var == "conf"], na.rm = T),
            mdmt.bias = mean(val[var == "conf"], na.rm = TRUE) - mean(val[var == "acc"], na.rm = TRUE))

rapm <- d.uid %>% 
  select(uid, adr.ind.conf, crt.ind.conf, gk.ind.confO, mdmt.ind.conf,
         adr.ind.acc, crt.ind.acc, gk.ind.accO, mdmt.ind.acc) %>% 
  gather(var, val, -uid) %>% 
  separate(var, c("test", "ind", "var")) %>%
  mutate(var = str_replace(var, "O", "")) %>%
  group_by(uid) %>% 
  summarise(rapm.trait.acc = mean(val[var == "acc"], na.rm = T),
            rapm.trait.conf = mean(val[var == "conf"], na.rm = T),
            rapm.bias = mean(val[var == "conf"], na.rm = TRUE) - mean(val[var == "acc"], na.rm = TRUE))

# # calculate variance between team members' individual responses for acc, conf, and bias
# team_var <- d.uid %>% 
#   select(uid, group, matches("ind.acc"), matches("ind.conf"), matches("ind.bias"), 
#          -matches("UR"), -matches("accR"), -matches("confR"), -matches("biasR")) %>% 
#   pivot_longer(-uid:-group, names_to = "var", values_to = "val") %>% 
#   group_by(group, var) %>% 
#   summarise(uid = uid,
#             sd = sd(val)) %>% 
#   separate(var, into = c("a", "b", "c")) %>% 
#   unite(var, a,c, sep = "_") %>% 
#   mutate(var = paste0(var, "_team_variance")) %>% 
#   pivot_wider(names_from = var, values_from = sd) %>% 
#   ungroup() %>% 
#   select(-b, -group)

# calculate normalised difference score between team members' individual responses for acc, conf, and bias
# 1 = similar and 0 = dissimilar
team_similar <- d.uid %>% 
  select(uid, group, matches("ind.acc"), matches("ind.conf"), matches("ind.bias"), 
         -matches("UR"), -matches("accR"), -matches("confR"), -matches("biasR")) %>% 
  pivot_longer(-uid:-group, names_to = "var", values_to = "val") %>% 
  group_by(group, var) %>% 
  summarise(uid = uid,
            dist = abs(val[1] - val[2])) %>% 
  group_by(var) %>% 
  summarise(uid = uid, 
            group = group, 
            val = 1 - (dist - min(dist, na.rm = T)) / (max(dist, na.rm = T) - min(dist, na.rm = T))) %>% 
  separate(var, into = c("a", "b", "c")) %>% 
  unite(var, a,c, sep = "_") %>% 
  mutate(var = paste0(var, "_team_similar")) %>% 
  pivot_wider(names_from = var, values_from = val) %>% 
  ungroup() %>%
  select(-b, -group)

# merge all data into a single df
datalist <- list(d.uid, adr, crt, gk, mdmt, rapm, team_similar)

# combine into single df
d.uid <- datalist %>% reduce(left_join, by = "uid")

# clean environment
rm(list = setdiff(ls(), c("d.uid")))

# save individual-level data as csv
# last save 16/4/21
d.uid %>% write_csv("data/210416_uid_data.csv")

# calculate dyad-level dataset --------------------------------------------
d.grp <- d.uid %>% 
  gather(var, val, -uid:-version, -age:-issue.note.2) %>% 
  group_by(group, var) %>% 
  summarise(
    recruit = recruit[1],
    version = version[1],
    age = mean(age),
    prop.female = mean(gender == 0),
    aus.born = mean(aus_born),
    aus.years = mean(aus_years, na.rm = TRUE),
    eng.fl = mean(eng_fl),
    dic.use = mean(dic_use, na.rm = TRUE),
    team.familiarity = mean(team_familiarity),
    how.long = mean(how_long, na.rm = TRUE),
    how.well = mean(how_well, na.rm = TRUE),
    issue = issue[1],
    issue.note.1 = issue.note.1[1],
    issue.note.2 = issue.note.2[1],
    score = mean(val, na.rm = TRUE)) %>% 
  spread(var, score)

# calculate dyad-individual difference variables for regression
# non-consensus variables
tmp <- d.grp %>% 
  select(group, contains("adr"), contains("crt"), 
         contains("gk"), contains("mdmt"), contains("rapm"), 
         -contains("disagree"), -contains("trait"), 
         -adr.bias, -crt.bias, -gk.bias, -mdmt.bias, -rapm.bias) %>% 
  gather(var, val, -group) %>% 
  separate(var, c("test", "grouping", "var")) %>% 
  spread(grouping, val) %>% 
  group_by(group, test, var) %>% 
  mutate(diff = grp - ind) %>% 
  select(-grp, -ind) %>% 
  ungroup() %>% 
  unite(var, test, var, sep = ".") %>% 
  mutate(var = paste0(var, ".diff"),
         var = str_replace(var, "O.diff", ".diffO"),
         var = str_replace(var, "UR.diff", ".diffUR"),
         var = str_replace(var, "R.diff", ".diffR")
         ) %>%
  spread(var, diff)

# join difference varaibles to d.grp
d.grp <- d.grp %>% left_join(tmp, by = "group")

# reorder variables before saving
d.grp <- d.grp %>%
  select(group, recruit, version, age:how.well, issue:issue.note.2, 
         contains("ind.acc"), contains("grp.acc"), contains("acc.diff"), 
         contains("ind.conf"), contains("grp.conf"), contains("conf.diff"), 
         contains("ind.bias"), contains("grp.bias"), contains("bias.diff"), 
         contains("ind.discrimination"), contains("grp.discrimination"), contains("discrimination.diff"), 
         contains("ind.post"), contains("grp.post"), contains("post.diff"), 
         contains("ind.dec"), contains("grp.dec"), contains("dec.diff"), 
         contains("ind.rec"), contains("grp.rec"), contains("rec.diff"), 
         contains("ind.comp"), contains("grp.comp"), contains("comp.diff"), 
         contains("ind.optim"), contains("grp.optim"), contains("optim.diff"), 
         contains("ind.hes"), contains("grp.hes"), contains("hes.diff"), 
         contains("disagree"), contains("trait"), adr.bias, crt.bias, gk.bias, 
         mdmt.bias, rapm.bias, r2f.ind, r2f.grp, wm.acc, adapt.affective, 
         adapt.cognitive, adapt.global, trust, risk.aversion, emotion.acc, 
         agreeableness, conscientiousness, extraversion, intellect, neuroticism)

# clean environment
rm(list = setdiff(ls(), c("d.grp")))

# save dyad-level data as csv
# last save 19/6/19
# d.grp %>% write_csv("data/190619_dyad_data.csv")

}

# # Save data for Sabina (SPSS friendly) ------------------------------------
# # library(tidyverse)
# 
# # individual-level dataset
# d <- read_csv("data/190520_uid_data.csv") %>%
#   select(-member:-version, -wm.sum, -emotion.ubhr, -issue:-issue_note_2)
#   # gather(var, val, -group) %>%
#   # mutate(val = as.numeric(val)) %>%
#   # spread(var, val)
# 
# 
# {
# names(d) <- gsub(x = names(d), pattern = "adr", replacement = "Adr")
# names(d) <- gsub(x = names(d), pattern = "crt", replacement = "Crt")
# names(d) <- gsub(x = names(d), pattern = "mdmt", replacement = "Mdmt")
# names(d) <- gsub(x = names(d), pattern = "rapm", replacement = "Rapm")
# names(d) <- gsub(x = names(d), pattern = "gk", replacement = "Gk")
# names(d) <- gsub(x = names(d), pattern = "\\.ind", replacement = "Ind")
# names(d) <- gsub(x = names(d), pattern = "\\.grp", replacement = "Grp")
# names(d) <- gsub(x = names(d), pattern = "\\.acc", replacement = "Acc")
# names(d) <- gsub(x = names(d), pattern = "\\.conf", replacement = "Conf")
# names(d) <- gsub(x = names(d), pattern = "\\.bias", replacement = "Bias")
# names(d) <- gsub(x = names(d), pattern = "\\.discrimination", replacement = "Discrimination")
# names(d) <- gsub(x = names(d), pattern = "\\.post", replacement = "Post")
# names(d) <- gsub(x = names(d), pattern = "\\.dec", replacement = "Dec")
# names(d) <- gsub(x = names(d), pattern = "\\.rec", replacement = "Rec")
# names(d) <- gsub(x = names(d), pattern = "\\.comp", replacement = "Comp")
# names(d) <- gsub(x = names(d), pattern = "\\.optim", replacement = "Optim")
# names(d) <- gsub(x = names(d), pattern = "\\.hes", replacement = "Hes")
# names(d) <- gsub(x = names(d), pattern = "\\.diff", replacement = "Diff")
# names(d) <- gsub(x = names(d), pattern = "adapt\\.affective", replacement = "AdaptAffective")
# names(d) <- gsub(x = names(d), pattern = "adapt\\.cognitive", replacement = "AdaptCognitive")
# names(d) <- gsub(x = names(d), pattern = "adapt\\.global", replacement = "AdaptGlobal")
# names(d) <- gsub(x = names(d), pattern = "risk\\.aversion", replacement = "RiskAversion")
# names(d) <- gsub(x = names(d), pattern = "emotion", replacement = "Emotion")
# names(d) <- gsub(x = names(d), pattern = "group", replacement = "Group")
# names(d) <- gsub(x = names(d), pattern = "agreeableness", replacement = "Agreeableness")
# names(d) <- gsub(x = names(d), pattern = "conscientiousness", replacement = "Conscientiousness")
# names(d) <- gsub(x = names(d), pattern = "extraversion", replacement = "Extraversion")
# names(d) <- gsub(x = names(d), pattern = "intellect", replacement = "Intellect")
# names(d) <- gsub(x = names(d), pattern = "neuroticism", replacement = "Neuroticism")
# names(d) <- gsub(x = names(d), pattern = "r2f", replacement = "R2f")
# names(d) <- gsub(x = names(d), pattern = "trust", replacement = "Trust")
# names(d) <- gsub(x = names(d), pattern = "wm", replacement = "Wm")
# names(d) <- gsub(x = names(d), pattern = "eng_fl", replacement = "EngFL")
# names(d) <- gsub(x = names(d), pattern = "aus_born", replacement = "AusBorn")
# names(d) <- gsub(x = names(d), pattern = "aus_years", replacement = "AusYears")
# names(d) <- gsub(x = names(d), pattern = "dic_use", replacement = "DicUse")
# names(d) <- gsub(x = names(d), pattern = "team_familiarity", replacement = "TeamFamiliarity")
# names(d) <- gsub(x = names(d), pattern = "how_long", replacement = "HowLong")
# names(d) <- gsub(x = names(d), pattern = "how_well", replacement = "HowWell")
#   }
# 
# d <- d %>%
#   select(uid, Group, age:HowWell, contains("IndAcc"), contains("GrpAcc"), contains("AccDiff"),
#          contains("IndConf"), contains("GrpConf"), contains("ConfDiff"),
#          contains("IndBias"), contains("GrpBias"), contains("BiasDiff"),
#          contains("IndDiscrimination"), contains("GrpDiscrimination"), contains("DiscriminationDiff"),
#          contains("IndPost"), contains("GrpPost"), contains("PostDiff"),
#          contains("IndDec"), contains("GrpDec"), contains("DecDiff"),
#          contains("IndRec"), contains("GrpRec"), contains("RecDiff"),
#          contains("IndComp"), contains("GrpComp"), contains("CompDiff"),
#          contains("IndOptim"), contains("GrpOptim"), contains("OptimDiff"),
#          contains("IndHes"), contains("GrpHes"), contains("HesDiff"),
#          R2fInd, R2fGrp, WmAcc, AdaptAffective, AdaptCognitive,
#          AdaptGlobal, Trust, RiskAversion, EmotionAcc, Agreeableness,
#          Conscientiousness, Extraversion, Intellect, Neuroticism)
# 
# # save data for SK
# d %>% write_csv("output/sabina/200519_individual_data_sabina.csv")
# 
# # group-level dataset
# d <- read_csv("data/190520_dyad_data.csv") %>%
#   select(-recruit, -version, -issue:-issue_note_2)
# 
# {
#   names(d) <- gsub(x = names(d), pattern = "adr", replacement = "Adr")
#   names(d) <- gsub(x = names(d), pattern = "crt", replacement = "Crt")
#   names(d) <- gsub(x = names(d), pattern = "mdmt", replacement = "Mdmt")
#   names(d) <- gsub(x = names(d), pattern = "rapm", replacement = "Rapm")
#   names(d) <- gsub(x = names(d), pattern = "gk", replacement = "Gk")
#   names(d) <- gsub(x = names(d), pattern = "\\.ind", replacement = "Ind")
#   names(d) <- gsub(x = names(d), pattern = "\\.grp", replacement = "Grp")
#   names(d) <- gsub(x = names(d), pattern = "\\.acc", replacement = "Acc")
#   names(d) <- gsub(x = names(d), pattern = "\\.conf", replacement = "Conf")
#   names(d) <- gsub(x = names(d), pattern = "\\.bias", replacement = "Bias")
#   names(d) <- gsub(x = names(d), pattern = "\\.discrimination", replacement = "Discrimination")
#   names(d) <- gsub(x = names(d), pattern = "\\.post", replacement = "Post")
#   names(d) <- gsub(x = names(d), pattern = "\\.dec", replacement = "Dec")
#   names(d) <- gsub(x = names(d), pattern = "\\.rec", replacement = "Rec")
#   names(d) <- gsub(x = names(d), pattern = "\\.comp", replacement = "Comp")
#   names(d) <- gsub(x = names(d), pattern = "\\.optim", replacement = "Optim")
#   names(d) <- gsub(x = names(d), pattern = "\\.hes", replacement = "Hes")
#   names(d) <- gsub(x = names(d), pattern = "\\.diff", replacement = "Diff")
#   names(d) <- gsub(x = names(d), pattern = "adapt\\.affective", replacement = "AdaptAffective")
#   names(d) <- gsub(x = names(d), pattern = "adapt\\.cognitive", replacement = "AdaptCognitive")
#   names(d) <- gsub(x = names(d), pattern = "adapt\\.global", replacement = "AdaptGlobal")
#   names(d) <- gsub(x = names(d), pattern = "risk\\.aversion", replacement = "RiskAversion")
#   names(d) <- gsub(x = names(d), pattern = "emotion", replacement = "Emotion")
#   names(d) <- gsub(x = names(d), pattern = "group", replacement = "Group")
#   names(d) <- gsub(x = names(d), pattern = "agreeableness", replacement = "Agreeableness")
#   names(d) <- gsub(x = names(d), pattern = "conscientiousness", replacement = "Conscientiousness")
#   names(d) <- gsub(x = names(d), pattern = "extraversion", replacement = "Extraversion")
#   names(d) <- gsub(x = names(d), pattern = "intellect", replacement = "Intellect")
#   names(d) <- gsub(x = names(d), pattern = "neuroticism", replacement = "Neuroticism")
#   names(d) <- gsub(x = names(d), pattern = "r2f", replacement = "R2f")
#   names(d) <- gsub(x = names(d), pattern = "trust", replacement = "Trust")
#   names(d) <- gsub(x = names(d), pattern = "wm", replacement = "Wm")
#   names(d) <- gsub(x = names(d), pattern = "eng_fl", replacement = "EngFL")
#   names(d) <- gsub(x = names(d), pattern = "aus_born", replacement = "AusBorn")
#   names(d) <- gsub(x = names(d), pattern = "aus_years", replacement = "AusYears")
#   names(d) <- gsub(x = names(d), pattern = "dic_use", replacement = "DicUse")
#   names(d) <- gsub(x = names(d), pattern = "team_familiarity", replacement = "TeamFamiliarity")
#   names(d) <- gsub(x = names(d), pattern = "how_long", replacement = "HowLong")
#   names(d) <- gsub(x = names(d), pattern = "how_well", replacement = "HowWell")
#   names(d) <- gsub(x = names(d), pattern = "prop.female", replacement = "PropFemale")
#   }
# 
# d <- d %>%
#   select(Group, age:HowWell, contains("IndAcc"), contains("GrpAcc"), contains("AccDiff"),
#          contains("IndConf"), contains("GrpConf"), contains("ConfDiff"),
#          contains("IndBias"), contains("GrpBias"), contains("BiasDiff"),
#          contains("IndDiscrimination"), contains("GrpDiscrimination"), contains("DiscriminationDiff"),
#          contains("IndPost"), contains("GrpPost"), contains("PostDiff"),
#          contains("IndDec"), contains("GrpDec"), contains("DecDiff"),
#          contains("IndRec"), contains("GrpRec"), contains("RecDiff"),
#          contains("IndComp"), contains("GrpComp"), contains("CompDiff"),
#          contains("IndOptim"), contains("GrpOptim"), contains("OptimDiff"),
#          contains("IndHes"), contains("GrpHes"), contains("HesDiff"),
#          R2fInd, R2fGrp, WmAcc, AdaptAffective, AdaptCognitive,
#          AdaptGlobal, Trust, RiskAversion, EmotionAcc, Agreeableness,
#          Conscientiousness, Extraversion, Intellect, Neuroticism)
# 
# # save data for SK
# d %>% write_csv("output/sabina/200519_dyad_data_sabina.csv")
