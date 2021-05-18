# compute start and end times for each survey so can map to audio files
# at which point was recording started?
# differed for those who completed A and B
# could use epoch times for the first item to match to recording
library(tidyverse)

if (file.exists("data/audio_duration.csv")) {
  time <- read_csv("data/audio_duration.csv")
  
} else {
  
source("R/import_data.R")

scripts <- list.files("R/survey_prep/", pattern = ".R")[c(-2, -5, -6, -9, -22)]

for (i in scripts) {
  source(paste0("R/survey_prep/", i))
}

tests <- c("adr", "crt", "gk", "mdmt_team", "rapm_team", "adapt", "ipip")

tmp <- map(tests, function(i) {
  if (i == "mdmt_team") {
    mdmt_team %>% 
      ungroup() %>% 
      select(uid, StartDate, EndDate, MdmtItemNum, Grp_Stimulus.OnsetTime, Grp_Confidence.OffsetTime,
             version) %>% 
      rename(itemnum = MdmtItemNum) %>%
      mutate(itemnum = as.numeric(itemnum)) %>% 
      mutate(min = ifelse(uid %in% c("17103012_2_d1", "17103012_2_g1", "18072412_1_e1", "18072412_1_e2"), 33, 2),
             max = ifelse(uid %in% c("18082114_1_e1", "18082114_1_e2"), 29, 20),
             test = i) %>% 
      group_by(uid) %>% 
      filter(itemnum %in% c(min, max)) %>% 
      mutate(start = ifelse(itemnum == max, NA, Grp_Stimulus.OnsetTime),
             end = ifelse(itemnum == min, NA, Grp_Confidence.OffsetTime)) %>% 
      gather(var, val, start, end) %>% 
      filter(!is.na(val)) %>% 
      select(-itemnum:-Grp_Confidence.OffsetTime, -min, -max) %>% 
      spread(var, val) %>% 
      mutate(duration = (end - start)/1000)
  } else if (i == "adr") {
    adr %>% 
      ungroup() %>% 
      select(uid, StartDate, EndDate, ADRitemNum, Ind_Stimulus.OnsetTime, Grp_Confidence.OffsetTime,
             version) %>% 
      rename(itemnum = ADRitemNum) %>%
      mutate(itemnum = as.numeric(itemnum)) %>% 
      mutate(min = min(itemnum, na.rm = T),
             max = max(itemnum, na.rm = T),
             Ind_Stimulus.OnsetTime = as.numeric(Ind_Stimulus.OnsetTime),
             Grp_Confidence.OffsetTime = as.numeric(Grp_Confidence.OffsetTime),
             test = i) %>% 
      filter(itemnum %in% c(min, max)) %>% 
      mutate(start = ifelse(itemnum == max, NA, Ind_Stimulus.OnsetTime),
             end = ifelse(itemnum == min, NA, Grp_Confidence.OffsetTime)) %>% 
      gather(var, val, start, end) %>% 
      filter(!is.na(val)) %>% 
      select(-itemnum:-Grp_Confidence.OffsetTime, -min, -max) %>% 
      spread(var, val) %>% 
      mutate(duration = (end - start)/1000)
    
  } else if (i == "crt") {
    crt %>% 
      ungroup() %>% 
      select(uid, StartDate, EndDate, CRTItemNum, Ind_Stimulus.OnsetTime, 
             Wait.OffsetTime, Grp_Confidence.OffsetTime, version) %>% 
      rename(itemnum = CRTItemNum) %>%
      mutate(itemnum = as.numeric(itemnum)) %>% 
      mutate(min = min(itemnum, na.rm = T),
             max = max(itemnum, na.rm = T),
             Ind_Stimulus.OnsetTime = as.numeric(Ind_Stimulus.OnsetTime),
             Wait.OffsetTime = as.numeric(Wait.OffsetTime),
             Grp_Confidence.OffsetTime = as.numeric(Grp_Confidence.OffsetTime),
             test = i) %>% 
      filter(itemnum %in% c(min, max)) %>% 
      mutate(start = ifelse(itemnum == min, Ind_Stimulus.OnsetTime, NA),
             end = ifelse(itemnum == max & uid %in% c("18062010_2_e1", "18062010_2_e2", "18062815_1_e1" ,"18062815_1_e2"),
                          Wait.OffsetTime,
                          ifelse(itemnum == max & !uid %in% c("18062010_2_e1", "18062010_2_e2", "18062815_1_e1" ,"18062815_1_e2"),
                                 Grp_Confidence.OffsetTime, NA))) %>% 
      gather(var, val, start, end) %>% 
      filter(!is.na(val)) %>% 
      select(-itemnum:-Grp_Confidence.OffsetTime, -min, -max) %>% 
      spread(var, val) %>% 
      mutate(duration = (end - start)/1000)
  } else if (i == "gk") {
    gk %>% 
      ungroup() %>% 
      select(uid, StartDate, EndDate, Itemnum, Ind_Stimulus.OnsetTime, 
             Wait.OffsetTime, Grp_Confidence.OffsetTime, version) %>% 
      rename(itemnum = Itemnum) %>%
      mutate(itemnum = as.numeric(itemnum)) %>% 
      mutate(min = min(itemnum, na.rm = T),
             max = max(itemnum, na.rm = T),
             Ind_Stimulus.OnsetTime = as.numeric(Ind_Stimulus.OnsetTime),
             Wait.OffsetTime = as.numeric(Wait.OffsetTime),
             Grp_Confidence.OffsetTime = as.numeric(Grp_Confidence.OffsetTime),
             test = i) %>% 
      filter(itemnum %in% c(min, max)) %>% 
      mutate(start = ifelse(itemnum == min, Ind_Stimulus.OnsetTime, NA),
             end = ifelse(itemnum == max & uid %in%
                            c("17100612_2_d1", "17100612_2_g1", "17102312_1_d2" ,"17102312_1_g2",
                              "19021412_1_d1", "19021412_1_g1"), Wait.OffsetTime,
                   ifelse(itemnum == max & !uid %in% 
                            c("17100612_2_d1", "17100612_2_g1", "17102312_1_d2" ,"17102312_1_g2",
                              "19021412_1_d1", "19021412_1_g1"), Grp_Confidence.OffsetTime, NA))) %>% 
      gather(var, val, start, end) %>% 
      filter(!is.na(val)) %>% 
      select(-itemnum:-Grp_Confidence.OffsetTime, -min, -max) %>% 
      spread(var, val) %>% 
      mutate(duration = (end - start)/1000)
  } else if (i == "rapm_team") {
    rapm_team %>% 
      ungroup() %>% 
      select(uid, StartDate, EndDate, ApmItemNum, Grp_Stimulus.OnsetTime, Grp_Confidence.OffsetTime, version) %>% 
      rename(itemnum = ApmItemNum) %>%
      mutate(itemnum = as.numeric(itemnum)) %>% 
      mutate(min = min(itemnum, na.rm = T),
             max = max(itemnum, na.rm = T),
             Grp_Stimulus.OnsetTime = as.numeric(Grp_Stimulus.OnsetTime),
             Grp_Confidence.OffsetTime = as.numeric(Grp_Confidence.OffsetTime),
             test = i) %>% 
      filter(itemnum %in% c(min, max)) %>% 
      mutate(start = ifelse(itemnum == min, Grp_Stimulus.OnsetTime, NA),
             end = ifelse(itemnum == max, Grp_Confidence.OffsetTime, NA)) %>% 
      gather(var, val, start, end) %>% 
      filter(!is.na(val)) %>% 
      select(-itemnum:-Grp_Confidence.OffsetTime, -min, -max) %>% 
      spread(var, val) %>% 
      mutate(duration = (end - start)/1000)
  } else if (i == "adapt") {
    adapt %>% 
      ungroup() %>% 
      select(uid, StartDate, EndDate, itemnum, Stimulus.OffsetTime, version) %>% 
      mutate(itemnum = as.numeric(itemnum),
             Stimulus.OffsetTime = as.numeric(Stimulus.OffsetTime),
             test = i) %>% 
      filter(itemnum == max(itemnum)) %>% 
      group_by(uid) %>% 
      mutate(start = as.numeric(NA),
             end = Stimulus.OffsetTime,
             duration = as.numeric(NA)) %>% 
      select(-itemnum, -Stimulus.OffsetTime)
  } else if (i == "ipip") {
    ipip %>% 
      ungroup() %>% 
      select(uid, StartDate, EndDate, Stimulus.OffsetTime, version) %>% 
      mutate(Stimulus.OffsetTime = as.numeric(Stimulus.OffsetTime),
             test = i) %>% 
      group_by(uid) %>% 
      filter(Stimulus.OffsetTime == max(Stimulus.OffsetTime, na.rm = T)) %>% 
      group_by(uid) %>% 
      mutate(start = as.numeric(NA),
             end = Stimulus.OffsetTime,
             duration = as.numeric(NA)) %>% 
      select(-Stimulus.OffsetTime)
  }
})

# bind rows
# and convert epoch time to date and hours
# make test vector consistent (1 piece)
tmp <- bind_rows(tmp) %>% 
  mutate(start_time = as.POSIXct(start/1000, tz = "Etc/GMT-10", origin = "1970-01-01"),
         end_time = as.POSIXct(end/1000, tz = "Etc/GMT-10", origin = "1970-01-01"),
         test = str_remove(test, "_team"),
         version = str_extract(version, "[ab]"))

# order of presentation:
# A2 - RAPM team, MDMT team, GK, CRT, ADR, ..., IPIP (end only)
# B2 - MDMT team, RAPM team, ADR, GK, CRT, ..., Adapt (end only)

# tests in counterbalance set A
tmp <- map(c("a", "b"), function(i) {
  tmp <- tmp %>% 
    select(-StartDate, -EndDate, -start_time, -end_time) %>% 
    gather(var, val, start, duration, end) %>% 
    unite(var, test, var) %>%
    spread(var, val)
  
  if (i == "a") {
    tmp %>% 
      filter(version == i) %>%
      mutate(group_start = rapm_start) %>% 
      gather(var, start_epoch, adr_start, crt_start, gk_start, mdmt_start, rapm_start, ipip_end) %>% 
      separate(var, into = c("test", "s")) %>% 
      select(-s) %>% 
      group_by(uid) %>% 
      mutate(start_secs = (start_epoch - group_start)/1000) %>% 
      gather(var, val, start_epoch, start_secs) %>% 
      unite(var, test, var) %>% 
      spread(var, val) %>% 
      rename(total_dur_secs = ipip_start_secs, ipip_end_epoch = ipip_start_epoch)
  } else if (i == "b") {
    tmp %>% 
      filter(version == i) %>%
      mutate(group_start = mdmt_start) %>% 
      gather(var, start_epoch, adr_start, crt_start, gk_start, mdmt_start, rapm_start, adapt_end) %>% 
      separate(var, into = c("test", "s")) %>% 
      select(-s) %>% 
      group_by(uid) %>% 
      mutate(start_secs = (start_epoch - group_start)/1000) %>% 
      gather(var, val, start_epoch, start_secs) %>% 
      unite(var, test, var) %>% 
      spread(var, val) %>% 
      rename(total_dur_secs = adapt_start_secs, adapt_end_epoch = adapt_start_epoch)
  }
})

time <- bind_rows(tmp)

# session notes reveal incorrectly recorded uids
# group == 18073109_1 recorded as sona recruits (g2, d2) when they were externally recruited (e1, e2)
# group == 17040610_1 recorded as participating in 2017 but were actually 2018
# group == 1800509_1 recorded as having participated in May (05) when they participated in June (06)
# group == 18050712_1 recorded as having participated in May (05) when they participated in June (06)
# group == 18073110_2 recorded as sona recruits (g1, d1) when they were externally recruited (e1, e2)
# group == 18073114_1 recorded as sona recruits (g1, d1) when they were externally recruited (e1, e2)
time <- time %>% 
  ungroup %>% 
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
               ifelse(uid == "18073114_1_g2", "18073114_1_e1", uid)))))))))))))

time %>% write_csv("data/audio_duration.csv")

# clean environment
rm(list = setdiff(ls(), c("time")))

}

