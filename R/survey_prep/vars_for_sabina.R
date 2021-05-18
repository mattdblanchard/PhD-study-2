# This script computes variables for SK
# load data
{
source("R/import_data.R")

scripts <- list.files("R/survey_prep/", pattern = ".R")[c(3, 7, 8, 10, 11, 12, 17, 18)] 

for (i in scripts) {
  source(paste0("R/survey_prep/", i))
}
}

# prepare demographic data
demo <- demo %>%
  group_by(uid) %>% 
  summarise(
    Recruit = ifelse(recruit == "sona", 0,
                     ifelse(recruit == "ext", 1, recruit)),
    Age = age,
    Gender = gender,
    AusBorn = as.numeric(aus_born),
    AusYears = aus_years,
    EngFL = as.numeric(eng_fl),
    DicUse = dic_use,
    Relationship = as.numeric(team_familiarity),
    RelationshipLength = as.numeric(how_long),
    RelationshipStrength = as.numeric(how_well)) %>%
  mutate(group = gsub("_[a-z][12]", "", uid),
         uid = ifelse(uid == "18073109_1_e1", "18073109_1_g2",
                        ifelse(uid == "18073109_1_e2", "18073109_1_d2", 
                               ifelse(uid == "18073110_2_e2", "18073110_2_d1", 
                                      ifelse(uid == "18073110_2_e1", "18073110_2_g1", uid))))
         # group = ifelse(group == "18050509_1", "18060509_1", 
         #                  ifelse(group == "17040610_1", "18040610_1", 
         #                         ifelse(group == "18050712_1", "18060712_1", group))),
         # Recruit = ifelse(group == "18073109_1", "1",         # exteral == 1
         #                    ifelse(group == "18073110_2", "1", Recruit))
         )

# add version to demographics and make numeric
tmp <- rapm_ind %>% 
  group_by(uid) %>% 
summarise(version = version[1]) %>% 
  mutate(
  Version = ifelse(version == "a1", 1,
                   ifelse(version == "b1", 2,
                          ifelse(version == "b_multi", 3,
                                 ifelse(version == "a2", 4, 
                                        ifelse(version == "b2", 5, version))))),
  Version = as.numeric(Version)) %>% 
  select(-version)

demo <- demo %>% left_join(tmp, by = "uid")

# General Knowledge Test --------------------------------------------------
# join demographics
d <- gk.uid %>% left_join(demo, by = "uid")

# session notes reveal incorrectly recorded uids
d <- d %>% 
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
         Recruit = ifelse(group == "18073109_1", 1,         # exteral == 1
                          ifelse(group == "18073110_2", 1,
                                 ifelse(group == "18073114_1", 1, Recruit))))

# group == 17110114_2 responded to most individual GK items together as a team need to NA individual responses
vars <- d %>% 
  ungroup() %>% 
  select(contains("gk")) %>% 
  select(contains("ind")) %>% 
  names()

d[d$group == "17110114_2", vars] <- NA

# remove teams that have been excluded
d <- d %>% filter(!group %in% c("18041210_2", "18072412_1","18081613_1"))

# rename vars for SPSS
{
names(d) <- gsub(x = names(d), pattern = "gk", replacement = "Gk")
names(d) <- gsub(x = names(d), pattern = "\\.ind", replacement = "Ind")
names(d) <- gsub(x = names(d), pattern = "\\.grp", replacement = "Grp")
names(d) <- gsub(x = names(d), pattern = "\\.accO", replacement = "AccO")
names(d) <- gsub(x = names(d), pattern = "\\.confO", replacement = "ConfO")
names(d) <- gsub(x = names(d), pattern = "\\.postO", replacement = "PostO")
names(d) <- gsub(x = names(d), pattern = "\\.decO", replacement = "DecO")
names(d) <- gsub(x = names(d), pattern = "\\.recO", replacement = "RecO")
names(d) <- gsub(x = names(d), pattern = "\\.compO", replacement = "CompO")
names(d) <- gsub(x = names(d), pattern = "\\.optimO", replacement = "OptimO")
names(d) <- gsub(x = names(d), pattern = "\\.hesO", replacement = "HesO")
names(d) <- gsub(x = names(d), pattern = "\\.diffO", replacement = "DiffO")
names(d) <- gsub(x = names(d), pattern = "\\.accR", replacement = "AccR")
names(d) <- gsub(x = names(d), pattern = "\\.confR", replacement = "ConfR")
names(d) <- gsub(x = names(d), pattern = "\\.postR", replacement = "PostR")
names(d) <- gsub(x = names(d), pattern = "\\.decR", replacement = "DecR")
names(d) <- gsub(x = names(d), pattern = "\\.recR", replacement = "RecR")
names(d) <- gsub(x = names(d), pattern = "\\.compR", replacement = "CompR")
names(d) <- gsub(x = names(d), pattern = "\\.optimR", replacement = "OptimR")
names(d) <- gsub(x = names(d), pattern = "\\.hesR", replacement = "HesR")
names(d) <- gsub(x = names(d), pattern = "\\.diffR", replacement = "DiffR")
names(d) <- gsub(x = names(d), pattern = "\\.accUR", replacement = "AccUR")
names(d) <- gsub(x = names(d), pattern = "\\.confUR", replacement = "ConfUR")
names(d) <- gsub(x = names(d), pattern = "\\.postUR", replacement = "PostUR")
names(d) <- gsub(x = names(d), pattern = "\\.decUR", replacement = "DecUR")
names(d) <- gsub(x = names(d), pattern = "\\.recUR", replacement = "RecUR")
names(d) <- gsub(x = names(d), pattern = "\\.compUR", replacement = "CompUR")
names(d) <- gsub(x = names(d), pattern = "\\.optimUR", replacement = "OptimUR")
names(d) <- gsub(x = names(d), pattern = "\\.hesUR", replacement = "HesUR")
names(d) <- gsub(x = names(d), pattern = "\\.diffUR", replacement = "DiffUR")
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
}

# reorder variables
d <- d %>%
  gather(var, val, -uid, -group) %>% 
  mutate(val = as.numeric(val)) %>% 
  spread(var, val) %>% 
  select(uid, group, Version, Recruit, Age, Gender, AusBorn, AusYears, EngFL,
         DicUse, Relationship, RelationshipLength, RelationshipStrength, 
         contains("IndAcc"), contains("GrpAcc"), contains("AccDiff"),
         contains("IndConf"), contains("GrpConf"), contains("ConfDiff"),
         contains("IndPost"), contains("GrpPost"), contains("PostDiff"),
         contains("IndDec"), contains("GrpDec"), contains("DecDiff"),
         contains("IndRec"), contains("GrpRec"), contains("RecDiff"),
         contains("IndComp"), contains("GrpComp"), contains("CompDiff"),
         contains("IndOptim"), contains("GrpOptim"), contains("OptimDiff"),
         contains("IndHes"), contains("GrpHes"), contains("HesDiff"))

# # save data
d %>% write_csv("output/sabina/12052019_gk_variable_data_sabina.csv")

# clean d from environment
rm(d)

# compute raw data
d <- gk %>% 
  ungroup() %>% 
  select(uid, group, Itemnum, Ind_Stimulus.ACC, Ind_Confidence.RESP, 
         Ind_Decision.RESP, Grp_Stimulus.ACC, Grp_Confidence.RESP, Grp_Decision.RESP) %>% 
  mutate(Itemnum = as.numeric(Itemnum),
         Ind_Stimulus.ACC = as.numeric(Ind_Stimulus.ACC),
         Grp_Stimulus.ACC = as.numeric(Grp_Stimulus.ACC),
         Ind_Decision.RESP = ifelse(Ind_Decision.RESP == "y", 1,
                                    ifelse(Ind_Decision.RESP == "n", 0, Ind_Decision.RESP)),
         Grp_Decision.RESP = ifelse(Grp_Decision.RESP == "y", 1,
                                    ifelse(Grp_Decision.RESP == "n", 0, Grp_Decision.RESP)),
         Ind_Decision.RESP = as.numeric(Ind_Decision.RESP),
         Grp_Decision.RESP = as.numeric(Grp_Decision.RESP))

# join demographic data
d <- d %>% left_join(demo, by = "uid")

# session notes reveal incorrectly recorded uids
d <- d %>% 
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
         Recruit = ifelse(group == "18073109_1", 1,
                          ifelse(group == "18073110_2", 1,
                                 ifelse(group == "18073114_1", 1, Recruit))))

# group == 17110114_2 responded to most individual GK items together as a team need to NA individual responses
vars <- d %>% 
  ungroup() %>% 
  select(contains("Ind")) %>% 
  names()

d[d$group == "17110114_2", vars] <- NA

# remove teams that have been excluded
d <- d %>% filter(!group %in% c("18041210_2", "18072412_1","18081613_1"))

# rename variables
{
names(d) <- gsub(x = names(d), pattern = "Ind_Stimulus\\.ACC", replacement = "IndAcc")
names(d) <- gsub(x = names(d), pattern = "Ind_Confidence\\.RESP", replacement = "IndConf")
names(d) <- gsub(x = names(d), pattern = "Ind_Decision\\.RESP", replacement = "IndDec")
names(d) <- gsub(x = names(d), pattern = "Grp_Stimulus\\.ACC", replacement = "GrpAcc")
names(d) <- gsub(x = names(d), pattern = "Grp_Confidence\\.RESP", replacement = "GrpConf")
names(d) <- gsub(x = names(d), pattern = "Grp_Decision\\.RESP", replacement = "GrpDec")
}

# create wide data for SPSS and reorder vars
# put vars into itemnum order 1-20?
d <- d %>%
  gather(var, val, IndAcc:GrpDec) %>% 
  unite(var, var, Itemnum, sep = "") %>% 
  spread(var, val) %>% 
  gather(var, val, -uid, -group) %>% 
  mutate(val = as.numeric(val)) %>% 
  spread(var, val) %>% 
  select(uid, group, Version, Recruit, Age, Gender, AusBorn, AusYears, EngFL,
         DicUse, Relationship, RelationshipLength, RelationshipStrength, 
         contains("IndAcc"),contains("IndConf"), contains("IndDec"), 
         contains("GrpAcc"), contains("GrpConf"), contains("GrpDec")) %>% 
  select(uid:IndAcc1, IndAcc2, IndAcc3:IndAcc9, IndAcc10:IndAcc19, IndAcc20,
             IndConf1, IndConf2, IndConf3:IndConf9, IndConf10:IndConf19, IndConf20,
             IndDec1, IndDec2, IndDec3:IndDec9, IndDec10:IndDec19, IndDec20,
             GrpAcc1, GrpAcc2, GrpAcc3:GrpAcc9, GrpAcc10:GrpAcc19, GrpAcc20,
             GrpConf1, GrpConf2, GrpConf3:GrpConf9, GrpConf10:GrpConf19, GrpConf20,
             GrpDec1, GrpDec2, GrpDec3:GrpDec9, GrpDec10:GrpDec19, GrpDec20)

# # save data for SK
d %>% write_csv("output/sabina/12052019_gk_item_level_data_sabina.csv")

# clean d from environment
rm(d)


# Ravens Progressive Matrices ---------------------------------------------
# individual data and join demographics
tmp1 <- rapm_ind.uid %>% select(-group:-version) %>% left_join(demo, by = "uid")

# group data
# add group column
tmp2 <- rapm_team.uid

# join individual and group data
d <- tmp1 %>% left_join(tmp2, by = "uid")

# group provided confidence ratings for group part together instead of individually need to NA
d[d$group == "18110716_1", "rapm.grp.conf"] <- NA

# session notes reveal incorrectly recorded uids
d <- d %>% 
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
         Recruit = ifelse(group == "18073109_1", 1,               # external == 1
                          ifelse(group == "18073110_2", 1,
                                 ifelse(group == "18073114_1", 1, Recruit))))

# remove teams that have been excluded
d <- d %>% filter(!group %in% c("18041210_2", "18072412_1", "18081613_1"))

# rename vars for SPSS
{
names(d) <- gsub(x = names(d), pattern = "rapm", replacement = "Rapm")
names(d) <- gsub(x = names(d), pattern = "\\.ind", replacement = "Ind")
names(d) <- gsub(x = names(d), pattern = "\\.grp", replacement = "Grp")
names(d) <- gsub(x = names(d), pattern = "\\.acc", replacement = "Acc")
names(d) <- gsub(x = names(d), pattern = "\\.conf", replacement = "Conf")
names(d) <- gsub(x = names(d), pattern = "\\.post", replacement = "Post")
names(d) <- gsub(x = names(d), pattern = "\\.dec", replacement = "Dec")
names(d) <- gsub(x = names(d), pattern = "\\.rec", replacement = "Rec")
names(d) <- gsub(x = names(d), pattern = "\\.comp", replacement = "Comp")
names(d) <- gsub(x = names(d), pattern = "\\.optim", replacement = "Optim")
names(d) <- gsub(x = names(d), pattern = "\\.hes", replacement = "Hes")
names(d) <- gsub(x = names(d), pattern = "\\.diff", replacement = "Diff")
}

# reorder variables
d <- d %>%
  gather(var, val, -uid, -group) %>% 
  mutate(val = as.numeric(val)) %>% 
  spread(var, val) %>% 
  select(uid, group, Version, Recruit:RelationshipStrength, 
         contains("IndAcc"), contains("GrpAcc"), contains("AccDiff"),
         contains("IndConf"), contains("GrpConf"), contains("ConfDiff"),
         contains("IndPost"), contains("GrpPost"), contains("PostDiff"),
         contains("IndDec"), contains("GrpDec"), contains("DecDiff"),
         contains("IndRec"), contains("GrpRec"), contains("RecDiff"),
         contains("IndComp"), contains("GrpComp"), contains("CompDiff"),
         contains("IndOptim"), contains("GrpOptim"), contains("OptimDiff"),
         contains("IndHes"), contains("GrpHes"), contains("HesDiff"))

# # save data
d %>% write_csv("output/sabina/12052019_rapm_variable_data_sabina.csv")

# clean d from environment
rm(d)

# compute raw data
# individuals
d <- rapm_ind %>% 
  ungroup() %>% 
  select(uid, group, ApmItemNum, Ind_Stimulus.ACC, Ind_Confidence.RESP, 
         Ind_Decision.RESP) %>% 
  mutate(ApmItemNum = as.numeric(ApmItemNum),
         Ind_Stimulus.ACC = as.numeric(Ind_Stimulus.ACC),
         Ind_Decision.RESP = ifelse(Ind_Decision.RESP == "y", 1,
                                    ifelse(Ind_Decision.RESP == "n", 0, Ind_Decision.RESP)),
         Ind_Decision.RESP = as.numeric(Ind_Decision.RESP))

# join demographic data
d <- d %>% left_join(demo, by = "uid")

# session notes reveal incorrectly recorded uids
d <- d %>% 
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
         Recruit = ifelse(group == "18073109_1", 1,               # external == 1
                          ifelse(group == "18073110_2", 1,
                                 ifelse(group == "18073114_1", 1, Recruit))))

# remove teams that have been excluded
d <- d %>% filter(!group %in% c("18041210_2", "18072412_1", "18081613_1"))

# rename variables
{
names(d) <- gsub(x = names(d), pattern = "Ind_Stimulus\\.ACC", replacement = "IndAcc")
names(d) <- gsub(x = names(d), pattern = "Ind_Confidence\\.RESP", replacement = "IndConf")
names(d) <- gsub(x = names(d), pattern = "Ind_Decision\\.RESP", replacement = "IndDec")
}

# create wide data for SPSS and reorder vars
# put vars into itemnum order
d <- d %>%
  gather(var, val, IndAcc:IndDec) %>% 
  unite(var, var, ApmItemNum, sep = "") %>% 
  spread(var, val) %>% 
  select(uid, group, Version, Recruit, Age, Gender, AusBorn, AusYears, EngFL,
         DicUse, Relationship, RelationshipLength, RelationshipStrength, 
         contains("IndAcc"), contains("IndConf"), contains("IndDec")) %>% 
  select(uid, group, Version, Recruit, Age, Gender, AusBorn, AusYears, EngFL,
         DicUse, Relationship, RelationshipLength, RelationshipStrength, 
         IndAcc1, IndAcc3, IndAcc5:IndAcc9, IndAcc11:IndAcc29, IndAcc31:IndAcc35,
         IndConf1, IndConf3, IndConf5:IndConf9, IndConf11:IndConf29, IndConf31:IndConf35,
         IndDec1, IndDec3, IndDec5:IndDec9, IndDec11:IndDec29, IndDec31:IndDec35)

# # save data for SK
d %>% write_csv("output/sabina/12052019_rapm_ind_item_data_sabina.csv")

# clean d from environment
rm(d)

# Group data
d <- rapm_team %>%
  ungroup() %>%
  select(uid, ApmItemNum, Grp_Stimulus.ACC, Grp_Confidence.RESP, Grp_Decision.RESP) %>%
  mutate(ApmItemNum = as.numeric(ApmItemNum),
         Grp_Stimulus.ACC = as.numeric(Grp_Stimulus.ACC),
         Grp_Decision.RESP = ifelse(Grp_Decision.RESP == "y", 1,
                                    ifelse(Grp_Decision.RESP == "n", 0, Grp_Decision.RESP)),
         Grp_Decision.RESP = as.numeric(Grp_Decision.RESP))

# join demographic data
d <- d %>% left_join(demo, by = "uid")

# group provided confidence ratings for group part together instead of individually need to NA
d[d$group == "18110716_1", "Grp_Confidence.RESP"] <- NA

# session notes reveal incorrectly recorded uids
d <- d %>% 
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
         Recruit = ifelse(group == "18073109_1", 1,               # external == 1
                          ifelse(group == "18073110_2", 1,
                                 ifelse(group == "18073114_1", 1, Recruit))))

# remove teams that have been excluded
d <- d %>% filter(!group %in% c("18041210_2", "18072412_1", "18081613_1"))

# rename variables
{
  names(d) <- gsub(x = names(d), pattern = "Grp_Stimulus\\.ACC", replacement = "GrpAcc")
  names(d) <- gsub(x = names(d), pattern = "Grp_Confidence\\.RESP", replacement = "GrpConf")
  names(d) <- gsub(x = names(d), pattern = "Grp_Decision\\.RESP", replacement = "GrpDec")
}

# create wide data for SPSS and reorder vars
# put vars into itemnum order
d <- d %>%
  gather(var, val, GrpAcc:GrpDec) %>% 
  unite(var, var, ApmItemNum, sep = "") %>% 
  spread(var, val) %>% 
  select(uid, group, Version, Recruit, Age, Gender, AusBorn, AusYears, EngFL,
         DicUse, Relationship, RelationshipLength, RelationshipStrength, 
    contains("GrpAcc"), contains("GrpConf"), contains("GrpDec")) %>% 
  select(uid, group, Version, Recruit, Age, Gender, AusBorn, AusYears, EngFL,
         DicUse, Relationship, RelationshipLength, RelationshipStrength, 
         GrpAcc2, GrpAcc4:GrpAcc8, GrpAcc10:GrpAcc18, GrpAcc20:GrpAcc36,
         GrpConf2, GrpConf4:GrpConf8, GrpConf10:GrpConf18, GrpConf20:GrpConf36,
         GrpDec2, GrpDec4:GrpDec8, GrpDec10:GrpDec18, GrpDec20:GrpDec36)

# # save data for SK
d %>% write_csv("output/sabina/12052019_rapm_team_item_data_sabina.csv")

# clean d from environment
rm(d)


# Applying Decision Rules -------------------------------------------------
# join demographics
d <- adr.uid %>% left_join(demo, by = "uid")


# group == 18110810_2 responded to most individual ADR items together as a team need to NA individual responses
vars <- adr.uid %>% 
  ungroup() %>% 
  select(contains("adr")) %>% 
  select(contains("ind")) %>% 
  names()

d[d$group == "18110810_2", vars] <- NA

# remove group == 17102710_2
# One member had to leave and the other completed ADR on thier own thus the data is meaningless
d <- d %>% filter(group != "17102710_2")

# session notes reveal incorrectly recorded uids
d <- d %>% 
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
         Recruit = ifelse(group == "18073109_1", 1,         # exteral == 1
                          ifelse(group == "18073110_2", 1,
                                 ifelse(group == "18073114_1", 1, Recruit))))

# remove teams that have been excluded
d <- d %>% filter(!group %in% c("18041210_2", "18072412_1","18081613_1"))

# rename vars for SPSS
{
  names(d) <- gsub(x = names(d), pattern = "adr", replacement = "Adr")
  names(d) <- gsub(x = names(d), pattern = "\\.ind", replacement = "Ind")
  names(d) <- gsub(x = names(d), pattern = "\\.grp", replacement = "Grp")
  names(d) <- gsub(x = names(d), pattern = "\\.acc", replacement = "Acc")
  names(d) <- gsub(x = names(d), pattern = "\\.conf", replacement = "Conf")
  names(d) <- gsub(x = names(d), pattern = "\\.post", replacement = "Post")
  names(d) <- gsub(x = names(d), pattern = "\\.dec", replacement = "Dec")
  names(d) <- gsub(x = names(d), pattern = "\\.rec", replacement = "Rec")
  names(d) <- gsub(x = names(d), pattern = "\\.comp", replacement = "Comp")
  names(d) <- gsub(x = names(d), pattern = "\\.optim", replacement = "Optim")
  names(d) <- gsub(x = names(d), pattern = "\\.hes", replacement = "Hes")
  names(d) <- gsub(x = names(d), pattern = "\\.diff", replacement = "Diff")
}

# reorder variables
d <- d %>%
  gather(var, val, -uid, -group) %>% 
  mutate(val = as.numeric(val)) %>% 
  spread(var, val) %>% 
  select(uid, group, Version, Recruit, Age, Gender, AusBorn, AusYears, EngFL,
         DicUse, Relationship, RelationshipLength, RelationshipStrength, 
         contains("IndAcc"), contains("GrpAcc"), contains("AccDiff"),
         contains("IndConf"), contains("GrpConf"), contains("ConfDiff"),
         contains("IndPost"), contains("GrpPost"), contains("PostDiff"),
         contains("IndDec"), contains("GrpDec"), contains("DecDiff"),
         contains("IndRec"), contains("GrpRec"), contains("RecDiff"),
         contains("IndComp"), contains("GrpComp"), contains("CompDiff"),
         contains("IndOptim"), contains("GrpOptim"), contains("OptimDiff"),
         contains("IndHes"), contains("GrpHes"), contains("HesDiff"))

# # save data
d %>% write_csv("output/sabina/12052019_adr_variable_data_sabina.csv")

# clean d from environment
rm(d)

# compute raw data
d <- adr %>% 
  ungroup() %>% 
  select(uid, ADRitemNum, Ind_Stimulus.ACC, Ind_Confidence.RESP, 
         Ind_Decision.RESP, Grp_Stimulus.ACC, Grp_Confidence.RESP, Grp_Decision.RESP) %>% 
  mutate(ADRitemNum = as.numeric(ADRitemNum),
         Ind_Stimulus.ACC = as.numeric(Ind_Stimulus.ACC),
         Grp_Stimulus.ACC = as.numeric(Grp_Stimulus.ACC),
         Ind_Decision.RESP = ifelse(Ind_Decision.RESP == "y", 1,
                                    ifelse(Ind_Decision.RESP == "n", 0, Ind_Decision.RESP)),
         Grp_Decision.RESP = ifelse(Grp_Decision.RESP == "y", 1,
                                    ifelse(Grp_Decision.RESP == "n", 0, Grp_Decision.RESP)),
         Ind_Decision.RESP = as.numeric(Ind_Decision.RESP),
         Grp_Decision.RESP = as.numeric(Grp_Decision.RESP))

# join demographic data
d <- d %>% left_join(demo, by = "uid")

# group == 18110810_2 responded to most individual ADR items together as a team need to NA individual responses
vars <- d %>% 
  ungroup() %>% 
  select(contains("ind")) %>% 
  names()

d[d$group == "18110810_2", vars] <- NA

# remove group == 17102710_2
# One member had to leave and the other completed ADR on thier own thus the data is meaningless
d <- d %>% filter(group != "17102710_2")

# session notes reveal incorrectly recorded uids
d <- d %>% 
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
         Recruit = ifelse(group == "18073109_1", 1,
                          ifelse(group == "18073110_2", 1,
                                 ifelse(group == "18073114_1", 1, Recruit))))

# group == 17110114_2 responded to most individual adr items together as a team need to NA individual responses
vars <- d %>% 
  ungroup() %>% 
  select(contains("Ind")) %>% 
  names()

d[d$group == "17110114_2", vars] <- NA

# remove teams that have been excluded
d <- d %>% filter(!group %in% c("18041210_2", "18072412_1","18081613_1"))

# rename variables
{
  names(d) <- gsub(x = names(d), pattern = "Ind_Stimulus\\.ACC", replacement = "IndAcc")
  names(d) <- gsub(x = names(d), pattern = "Ind_Confidence\\.RESP", replacement = "IndConf")
  names(d) <- gsub(x = names(d), pattern = "Ind_Decision\\.RESP", replacement = "IndDec")
  names(d) <- gsub(x = names(d), pattern = "Grp_Stimulus\\.ACC", replacement = "GrpAcc")
  names(d) <- gsub(x = names(d), pattern = "Grp_Confidence\\.RESP", replacement = "GrpConf")
  names(d) <- gsub(x = names(d), pattern = "Grp_Decision\\.RESP", replacement = "GrpDec")
}

# create wide data for SPSS and reorder vars
# put vars into itemnum order 1-20?
d <- d %>%
  gather(var, val, IndAcc:GrpDec) %>% 
  unite(var, var, ADRitemNum, sep = "") %>% 
  spread(var, val) %>% 
  gather(var, val, -uid, -group) %>% 
  mutate(val = as.numeric(val)) %>% 
  spread(var, val) %>% 
  select(uid, group, Version, Recruit, Age, Gender, AusBorn, AusYears, EngFL,
         DicUse, Relationship, RelationshipLength, RelationshipStrength, 
         contains("IndAcc"),contains("IndConf"), contains("IndDec"), 
         contains("GrpAcc"), contains("GrpConf"), contains("GrpDec")) %>% 
  select(uid:IndAcc1, IndAcc2:IndAcc9, IndAcc10, IndConf1, IndConf2:IndConf9, IndConf10,
         IndDec1, IndDec2:IndDec9, IndDec10)

# # save data for SK
d %>% write_csv("output/sabina/12052019_adr_item_level_data_sabina.csv")

# clean d from environment
rm(d)







# Medical Decision Making Test --------------------------------------------
# individual data and join demographics
tmp1 <- mdmt_ind.uid %>% # select(-group:-version) %>% 
  left_join(demo, by = "uid")

# group data
tmp2 <- mdmt_team.uid

# join individual and group data
d <- tmp1 %>% left_join(tmp2, by = "uid")

# group provided confidence ratings for group part together instead of individually need to NA
d[d$group == "18080213_1", "mdmt.grp.conf"] <- NA

# session notes reveal incorrectly recorded uids
d <- d %>% 
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
         Recruit = ifelse(group == "18073109_1", 1,               # external == 1
                          ifelse(group == "18073110_2", 1,
                                 ifelse(group == "18073114_1", 1, Recruit))))

# remove teams that have been excluded
d <- d %>% filter(!group %in% c("18041210_2", "18072412_1", "18081613_1"))

# rename vars for SPSS
{
  names(d) <- gsub(x = names(d), pattern = "mdmt", replacement = "Mdmt")
  names(d) <- gsub(x = names(d), pattern = "\\.ind", replacement = "Ind")
  names(d) <- gsub(x = names(d), pattern = "\\.grp", replacement = "Grp")
  names(d) <- gsub(x = names(d), pattern = "\\.acc", replacement = "Acc")
  names(d) <- gsub(x = names(d), pattern = "\\.conf", replacement = "Conf")
  names(d) <- gsub(x = names(d), pattern = "\\.post", replacement = "Post")
  names(d) <- gsub(x = names(d), pattern = "\\.dec", replacement = "Dec")
  names(d) <- gsub(x = names(d), pattern = "\\.rec", replacement = "Rec")
  names(d) <- gsub(x = names(d), pattern = "\\.comp", replacement = "Comp")
  names(d) <- gsub(x = names(d), pattern = "\\.optim", replacement = "Optim")
  names(d) <- gsub(x = names(d), pattern = "\\.hes", replacement = "Hes")
  names(d) <- gsub(x = names(d), pattern = "\\.diff", replacement = "Diff")
}

# reorder variables
d <- d %>%
  gather(var, val, -uid, -group) %>% 
  mutate(val = as.numeric(val)) %>% 
  spread(var, val) %>% 
  select(uid, group, Version, Recruit, Age, Gender, AusBorn, AusYears, EngFL,
         DicUse, Relationship, RelationshipLength, RelationshipStrength, 
         contains("IndAcc"), contains("GrpAcc"), contains("AccDiff"),
         contains("IndConf"), contains("GrpConf"), contains("ConfDiff"),
         contains("IndPost"), contains("GrpPost"), contains("PostDiff"),
         contains("IndDec"), contains("GrpDec"), contains("DecDiff"),
         contains("IndRec"), contains("GrpRec"), contains("RecDiff"),
         contains("IndComp"), contains("GrpComp"), contains("CompDiff"),
         contains("IndOptim"), contains("GrpOptim"), contains("OptimDiff"),
         contains("IndHes"), contains("GrpHes"), contains("HesDiff"))

# # save data
d %>% write_csv("output/sabina/12052019_mdmt_variable_data_sabina.csv")

# clean d from environment
rm(d)

# compute raw data
# individuals
d <- mdmt_ind %>% 
  ungroup() %>% 
  select(uid, MdmtItemNum, Ind_Stimulus.ACC, Ind_Confidence.RESP, 
         Ind_Decision.RESP) %>% 
  mutate(MdmtItemNum = as.numeric(MdmtItemNum),
         Ind_Stimulus.ACC = as.numeric(Ind_Stimulus.ACC),
         Ind_Decision.RESP = ifelse(Ind_Decision.RESP == "a", 1,
                                    ifelse(Ind_Decision.RESP == "b", 0, Ind_Decision.RESP)),
         Ind_Decision.RESP = as.numeric(Ind_Decision.RESP))

# join demographic data
d <- d %>% left_join(demo, by = "uid")

# session notes reveal incorrectly recorded uids
d <- d %>% 
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
         Recruit = ifelse(group == "18073109_1", 1,               # external == 1
                          ifelse(group == "18073110_2", 1,
                                 ifelse(group == "18073114_1", 1, Recruit))))

# remove teams that have been excluded
d <- d %>% filter(!group %in% c("18041210_2", "18072412_1", "18081613_1"))

# rename variables
{
  names(d) <- gsub(x = names(d), pattern = "Ind_Stimulus\\.ACC", replacement = "IndAcc")
  names(d) <- gsub(x = names(d), pattern = "Ind_Confidence\\.RESP", replacement = "IndConf")
  names(d) <- gsub(x = names(d), pattern = "Ind_Decision\\.RESP", replacement = "IndDec")
}

# create wide data for SPSS and reorder vars
# put vars into itemnum order
d <- d %>%
  gather(var, val, IndAcc:IndDec) %>% 
  unite(var, var, MdmtItemNum, sep = "") %>% 
  spread(var, val) %>% 
  select(uid, group, Version, Recruit, Age, Gender, AusBorn, AusYears, EngFL,
         DicUse, Relationship, RelationshipLength, RelationshipStrength, 
         contains("IndAcc"), contains("IndConf"), contains("IndDec")) %>% 
  select(uid, group, Version, Recruit, Age, Gender, AusBorn, AusYears, EngFL,
         DicUse, Relationship, RelationshipLength, RelationshipStrength, 
         IndAcc1, IndAcc2, IndAcc5, IndAcc8, IndAcc11:IndAcc17, IndAcc20:IndAcc33,
         IndConf1, IndConf2, IndConf5, IndConf8, IndConf11:IndConf17, IndConf20:IndConf33,
         IndDec1, IndDec2, IndDec5, IndDec8, IndDec11:IndDec17, IndDec20:IndDec33)

# # save data for SK
d %>% write_csv("output/sabina/12052019_mdmt_ind_item_data_sabina.csv")

# clean d from environment
rm(d)

# Group data
d <- mdmt_team %>%
  ungroup() %>%
  select(uid, MdmtItemNum, Grp_Stimulus.ACC, Grp_Confidence.RESP, Grp_Decision.RESP) %>%
  mutate(MdmtItemNum = as.numeric(MdmtItemNum),
         Grp_Stimulus.ACC = as.numeric(Grp_Stimulus.ACC),
         Grp_Decision.RESP = ifelse(Grp_Decision.RESP == "a", 1,
                                    ifelse(Grp_Decision.RESP == "b", 0, Grp_Decision.RESP)),
         Grp_Decision.RESP = as.numeric(Grp_Decision.RESP))

# join demographic data
d <- d %>% left_join(demo, by = "uid")

# group provided confidence ratings for group part together instead of individually need to NA
d[d$group == "18080213_1", "Grp_Confidence.RESP"] <- NA

# session notes reveal incorrectly recorded uids
d <- d %>% 
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
         Recruit = ifelse(group == "18073109_1", 1,               # external == 1
                          ifelse(group == "18073110_2", 1,
                                 ifelse(group == "18073114_1", 1, Recruit))))

# remove teams that have been excluded
d <- d %>% filter(!group %in% c("18041210_2", "18072412_1", "18081613_1"))

# rename variables
{
  names(d) <- gsub(x = names(d), pattern = "Grp_Stimulus\\.ACC", replacement = "GrpAcc")
  names(d) <- gsub(x = names(d), pattern = "Grp_Confidence\\.RESP", replacement = "GrpConf")
  names(d) <- gsub(x = names(d), pattern = "Grp_Decision\\.RESP", replacement = "GrpDec")
}

# create wide data for SPSS and reorder vars
# put vars into itemnum order
d <- d %>%
  gather(var, val, GrpAcc:GrpDec) %>% 
  unite(var, var, MdmtItemNum, sep = "") %>% 
  spread(var, val) %>% 
  select(uid, group, Version, Recruit, Age, Gender, AusBorn, AusYears, EngFL,
         DicUse, Relationship, RelationshipLength, RelationshipStrength, 
         contains("GrpAcc"), contains("GrpConf"), contains("GrpDec")) %>% 
  select(uid, group, Version, Recruit, Age, Gender, AusBorn, AusYears, EngFL,
         DicUse, Relationship, RelationshipLength, RelationshipStrength, 
         GrpAcc1, GrpAcc2, GrpAcc5, GrpAcc8, GrpAcc11:GrpAcc17, GrpAcc20:GrpAcc33,
         GrpConf1, GrpConf2, GrpConf5, GrpConf8, GrpConf11:GrpConf17, GrpConf20:GrpConf33,
         GrpDec1, GrpDec2, GrpDec5, GrpDec8, GrpDec11:GrpDec17, GrpDec20:GrpDec33)

# # save data for SK
d %>% write_csv("output/sabina/12052019_mdmt_team_item_data_sabina.csv")

# clean d from environment
rm(d)





# Cognitive Reflection Test -----------------------------------------------
# join demographics
d <- crt.uid %>% left_join(demo, by = "uid")

# session notes reveal incorrectly recorded uids
d <- d %>% 
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
         Recruit = ifelse(group == "18073109_1", 1,         # exteral == 1
                          ifelse(group == "18073110_2", 1,
                                 ifelse(group == "18073114_1", 1, Recruit))))

# remove teams that have been excluded
d <- d %>% filter(!group %in% c("18041210_2", "18072412_1","18081613_1"))

# rename vars for SPSS
{
  names(d) <- gsub(x = names(d), pattern = "crt", replacement = "Crt")
  names(d) <- gsub(x = names(d), pattern = "\\.ind", replacement = "Ind")
  names(d) <- gsub(x = names(d), pattern = "\\.grp", replacement = "Grp")
  names(d) <- gsub(x = names(d), pattern = "\\.acc", replacement = "Acc")
  names(d) <- gsub(x = names(d), pattern = "\\.conf", replacement = "Conf")
  names(d) <- gsub(x = names(d), pattern = "\\.post", replacement = "Post")
  names(d) <- gsub(x = names(d), pattern = "\\.dec", replacement = "Dec")
  names(d) <- gsub(x = names(d), pattern = "\\.rec", replacement = "Rec")
  names(d) <- gsub(x = names(d), pattern = "\\.comp", replacement = "Comp")
  names(d) <- gsub(x = names(d), pattern = "\\.optim", replacement = "Optim")
  names(d) <- gsub(x = names(d), pattern = "\\.hes", replacement = "Hes")
  names(d) <- gsub(x = names(d), pattern = "\\.diff", replacement = "Diff")
}

# reorder variables
d <- d %>%
  gather(var, val, -uid, -group) %>% 
  mutate(val = as.numeric(val)) %>% 
  spread(var, val) %>% 
  select(uid, group, Version, Recruit, Age, Gender, AusBorn, AusYears, EngFL,
         DicUse, Relationship, RelationshipLength, RelationshipStrength, 
         contains("IndAcc"), contains("GrpAcc"), contains("AccDiff"),
         contains("IndConf"), contains("GrpConf"), contains("ConfDiff"),
         contains("IndPost"), contains("GrpPost"), contains("PostDiff"),
         contains("IndDec"), contains("GrpDec"), contains("DecDiff"),
         contains("IndRec"), contains("GrpRec"), contains("RecDiff"),
         contains("IndComp"), contains("GrpComp"), contains("CompDiff"),
         contains("IndOptim"), contains("GrpOptim"), contains("OptimDiff"),
         contains("IndHes"), contains("GrpHes"), contains("HesDiff"))

# # save data
d %>% write_csv("output/sabina/12052019_crt_variable_data_sabina.csv")

# clean d from environment
rm(d)

# compute raw data
d <- crt %>% 
  ungroup() %>% 
  select(uid, CRTItemNum, Ind_Stimulus.ACC, Ind_Confidence.RESP, 
         Ind_Decision.RESP, Grp_Stimulus.ACC, Grp_Confidence.RESP, Grp_Decision.RESP) %>% 
  mutate(CRTItemNum = as.numeric(CRTItemNum),
         Ind_Stimulus.ACC = as.numeric(Ind_Stimulus.ACC),
         Grp_Stimulus.ACC = as.numeric(Grp_Stimulus.ACC),
         Ind_Decision.RESP = ifelse(Ind_Decision.RESP == "y", 1,
                                    ifelse(Ind_Decision.RESP == "n", 0, Ind_Decision.RESP)),
         Grp_Decision.RESP = ifelse(Grp_Decision.RESP == "y", 1,
                                    ifelse(Grp_Decision.RESP == "n", 0, Grp_Decision.RESP)),
         Ind_Decision.RESP = as.numeric(Ind_Decision.RESP),
         Grp_Decision.RESP = as.numeric(Grp_Decision.RESP))

# join demographic data
d <- d %>% left_join(demo, by = "uid")

# session notes reveal incorrectly recorded uids
d <- d %>% 
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
         Recruit = ifelse(group == "18073109_1", 1,
                          ifelse(group == "18073110_2", 1,
                                 ifelse(group == "18073114_1", 1, Recruit))))

# remove teams that have been excluded
d <- d %>% filter(!group %in% c("18041210_2", "18072412_1","18081613_1"))

# rename variables
{
  names(d) <- gsub(x = names(d), pattern = "Ind_Stimulus\\.ACC", replacement = "IndAcc")
  names(d) <- gsub(x = names(d), pattern = "Ind_Confidence\\.RESP", replacement = "IndConf")
  names(d) <- gsub(x = names(d), pattern = "Ind_Decision\\.RESP", replacement = "IndDec")
  names(d) <- gsub(x = names(d), pattern = "Grp_Stimulus\\.ACC", replacement = "GrpAcc")
  names(d) <- gsub(x = names(d), pattern = "Grp_Confidence\\.RESP", replacement = "GrpConf")
  names(d) <- gsub(x = names(d), pattern = "Grp_Decision\\.RESP", replacement = "GrpDec")
}

# create wide data for SPSS and reorder vars
# put vars into itemnum order 1-20?
check <- d %>%
  gather(var, val, IndAcc:GrpDec) %>% 
  unite(var, var, CRTItemNum, sep = "") %>% 
  spread(var, val) %>% 
  gather(var, val, -uid, -group) %>% 
  mutate(val = as.numeric(val)) %>% 
  spread(var, val) %>% 
  select(uid, group, Version, Recruit, Age, Gender, AusBorn, AusYears, EngFL,
         DicUse, Relationship, RelationshipLength, RelationshipStrength, 
         contains("IndAcc"),contains("IndConf"), contains("IndDec"), 
         contains("GrpAcc"), contains("GrpConf"), contains("GrpDec"))

# # save data for SK
d %>% write_csv("output/sabina/12052019_crt_item_level_data_sabina.csv")

# clean d from environment
rm(d)



