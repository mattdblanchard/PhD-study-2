# ********************************************************
# ******************* DATA PREPARATION *******************
# ********************************************************
# ******************* MATT BLANCHARD *********************
# ********************************************************

# the purpose of this script is to prepare the data for all 
# tasks for analyses. This includes reading in the csv files 
# as tibbles, cleaning, calculating variables and merging
# the data into a single dataframe.


# Replicate the structure of Simon's VR Dyads survey cleaning scripts:
#   1) survey_import
#   2) survey_items
#   3) survey_variables




source("R/parse_qrtengine.R")

# devtools::install_github("drsimonj/qualtricsr")
library(qualtricsr)
library(tidyverse)
# library(qrtenginer)

# import data for all tasks into a list of tibbles ----------------------------------------
# data are split over A and B versions of each task
# because the QRT surveys broke halfway through data collection
# there are 2 different file structures for A (qrt and single-JSON 
# java versions) and 3 different files for B (qrt, multi-JSON java 
# and single-JSON java)
# Need to read in each different file structure separately
# then change the variable names in multi-JSON and single-JSON java 
# to make consistent with qrt files
# then combine all versions into single list of tibbles for each task

survey_dir <- "/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI_data/qrt"
survey_paths <- file.path(survey_dir, list.files(survey_dir))
names(survey_paths) <- survey_paths %>% str_replace(stringr::str_c(survey_dir, "/"), "") %>% str_replace("\\.csv", "")
# try using q_read instead of parse_qrt_csv
surveys_qrt <- map(survey_paths, parse_qrt_csv)

survey_dir <- "/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI_data/java"
survey_paths <- file.path(survey_dir, list.files(survey_dir))
names(survey_paths) <- survey_paths %>% str_replace(stringr::str_c(survey_dir, "/"), "") %>% str_replace("\\.csv", "")
# try using q_read instead of parse_qrt_csv
surveys_java <- map(survey_paths, parse_csv)


survey_dir <- "data/java_R2F"
survey_paths <- file.path(survey_dir, list.files(survey_dir))
names(survey_paths) <- survey_paths %>% str_replace(stringr::str_c(survey_dir, "/"), "") %>% str_replace("\\.csv", "")
# try using q_read instead of parse_qrt_csv
surveys_r2f <- map(survey_paths, parse_csv)

# survey_dir <- "data/java_multi"
# survey_paths <- file.path(survey_dir, list.files(survey_dir))
# names(survey_paths) <- survey_paths %>% str_replace(stringr::str_c(survey_dir, "/"), "") %>% str_replace("\\.csv", "")
# 
# surveys_java_multi <- map(survey_paths, parse_csv2)


# rename first 10 columns of each QRT test
surveys_qrt <- surveys_qrt %>% map(rename, ResponseID = V1, ResponseSet = V2, Name = V3, 
                       ExternalDataReference = V4, EmailAddress = V5, IPAddress = V6, 
                       Status = V7, StartDate = V8, EndDate = V9, Finished = V10)

# Examine `uid` -----------------------------------------------------------

# Many problems occur with survey collection. This often results in certain 
# participants completing the same survey more than once, or uid information 
# being mixed up and so on. Other data problems exists such as surveys being
# completed by experimenters during extensive testing. This section will examine
# the appearance of uid across the surveys in order to handle any such problems.

# Function to count frequency of `uid` in each test
uid_freqs <- function() {
  map(surveys_qrt, select, uid) %>% 
    map(count, uid) %>% 
    map2(names(surveys_qrt), ~ mutate(.x , test = .y)) %>% 
    reduce(full_join)  
}

# Function to plot these frequencies
plot_freqs <- function() {
  ggplot(uid_freqs(), aes(x = uid, y = test, fill = factor(n))) +
    geom_tile() +
    scale_fill_brewer(palette = "OrRd") +
    labs(title = "uid frequency in each survey", x = NULL, y = NULL, fill = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
plot_freqs()

# Initial plot reveals `uid`s that...
# 
#   were not used by participants:
#   - "test[1-4]"
#   - "{\"ImportId\":\"uid\"}"
#   - NA
#
#   appear more than once on same test
#
#   were entered incorrectly (18051410_2_d2 instead of 18051410_2-d2)
#
#   completed only one measure
#
# The following will fix these issues.

# Drop `uid` missing values (so other functions can be run)
surveys_qrt <- map(surveys_qrt, drop_na, uid)
plot_freqs()

# Remove rows with `uid` containing "TEST"
surveys_qrt <- map(surveys_qrt, ~ filter(., !str_detect(uid, "test")))
plot_freqs()

# Identify and remove multiple attempts
uid_freqs() %>% filter(n > 1) %>% arrange(uid)
# ... 3 cases of multiple attemps

# identified some TEST uids and that the following uids 
# were incorrectly recorded across all tasks:
#     "18050509_1-e"    - should be "18050509_1_e1"
#     "18050509_1-e2"   - should be "18050509_1_e2"
#     "17100609_g1"     - should be "17100609_2_g1"
#     "17100609_d2"     - should be "17100609_1_d2"
#     "17100609_g2"     - should be "17100609_1_g2"
#     "17110112_1_d2"   - should be "17110110_1_d2"
#     "17040910_2_d1"   - should be  "18040910_2_d1"
#     "17040910_2_g1"   - should be  "18040910_2_g1"
#     "17040910_1_d2"   - should be  "18040910_1_d2"
#     "17040910_1_g2"   - should be  "18040910_1_g2"

# SOLUTION: 
# Remove various TEST uids and modify incorrect uids
surveys_qrt <- surveys_qrt %>% map(mutate, uid = ifelse(uid == "18050509_1-e", "18050509_1_e1", uid)) %>%
                      map(mutate, uid = ifelse(uid == "18050509_1-e2", "18050509_1_e2", uid)) %>%
                      map(mutate, uid = ifelse(uid == "17100609_g1", "17100609_2_g1", uid)) %>%
                      map(mutate, uid = ifelse(uid == "17100609_d2", "17100609_1_d2", uid)) %>%
                      map(mutate, uid = ifelse(uid == "17100609_g2", "17100609_1_g2", uid)) %>%
                      map(mutate, uid = ifelse(uid == "17110112_1_d2", "17110110_1_d2", uid)) %>%
                      map(mutate, uid = ifelse(uid == "17040910_2_d1", "18040910_2_d1", uid)) %>%
                      map(mutate, uid = ifelse(uid == "17040910_2_g1", "18040910_2_g1", uid)) %>%
                      map(mutate, uid = ifelse(uid == "17040910_1_d2", "18040910_1_d2", uid)) %>%
                      map(mutate, uid = ifelse(uid == "17040910_1_g2", "18040910_1_g2", uid)) %>%
                      map(~ filter(., !stringr::str_detect(uid, "TEST"))) %>%
                      map(~ filter(., !stringr::str_detect(uid, "test"))) %>%
                      map(filter, uid != "17110310_2_g1") %>%
                      map(filter, uid != "12345") %>%
                      map(filter, uid != "999") %>%
                      map(filter, uid != "343") %>%
                      map(filter, uid != "431") %>%
                      map(filter, uid != "")


surveys_java <- surveys_java %>% map(~ filter(., !stringr::str_detect(uid, "TEST"))) %>%
                                 map(~ filter(., !stringr::str_detect(uid, "test")))


# create group variable across all tasks
# surveys <- surveys %>% map(mutate, id = uid) %>%
#             map(separate, id, into = c("date", "group", "member"), sep = "_")

# Adapt scale A1 -------------------------------------------------------------
surveys_qrt$Adaptability_Scale_A1 <- surveys_qrt$Adaptability_Scale_A1 %>% 
                             select(ResponseID, StartDate:Finished, uid, itemnum,
                                    factor, Stimulus.RESP, Stimulus.OnsetTime, 
                                    Stimulus.OffsetTime, Stimulus.RT) %>%
                              mutate(version = "a1")

surveys_qrt$Adaptability_Scale_B1 <- surveys_qrt$Adaptability_Scale_B1 %>% 
                              select(ResponseID, StartDate:Finished, uid, itemnum,
                                     factor, Stimulus.RESP, Stimulus.OnsetTime, 
                                     Stimulus.OffsetTime, Stimulus.RT) %>%
                              mutate(version = "b1")

surveys_java$Adaptability_Scale_A2 <- surveys_java$Adaptability_Scale_A2 %>%
                              rename(ResponseID = ResponseId, Stimulus.RESP = response, 
                                     Stimulus.OnsetTime = stimulus_onset,
                                     Stimulus.OffsetTime = stimulus_offset, Stimulus.RT = stimulus_rt) %>%
                              select(ResponseID, StartDate, EndDate, Finished, uid, itemnum,
                                     factor, Stimulus.RESP, Stimulus.OnsetTime, 
                                     Stimulus.OffsetTime, Stimulus.RT) %>%
                              mutate(version = "a2")

surveys_java$Adaptability_Scale_B2 <- surveys_java$Adaptability_Scale_B2 %>%
                              rename(ResponseID = ResponseId, Stimulus.RESP = response, 
                                     Stimulus.OnsetTime = stimulus_onset,
                                     Stimulus.OffsetTime = stimulus_offset, Stimulus.RT = stimulus_rt) %>%
                              select(ResponseID, StartDate, EndDate, Finished, uid, itemnum,
                                     factor, Stimulus.RESP, Stimulus.OnsetTime, 
                                     Stimulus.OffsetTime, Stimulus.RT) %>%
                              mutate(version = "b2")

# combine data from a1, b1, a2 and b2 into a single df 
adapt <- rbind(surveys_qrt$Adaptability_Scale_A1, surveys_qrt$Adaptability_Scale_B1, 
               surveys_java$Adaptability_Scale_A2, surveys_java$Adaptability_Scale_B2)

# Data Screening ----------------------------------------------------------

# Check participants appear correct number of times =======================
# ISSUE: 
# identified that the following uids appear twice
# 1.    "18041210_1_d2"   = all responses legit, P from 1pm session assigned incorrect uid
# 1.    "18041210_1_g2"   = all responses legit, P from 1pm session assigned incorrect uid
# 2.    "18042013_1_g2"   = duplicate contains only NAs. ResponseID == R_3JiH3ZnPGYh1jEx
#
# SOLUTION: 
# 1. Legit 10am session Ps have ResponseID = R_3L5P9mV1i0KZ2DP and R_bBj3nNp3amrpb9z
# 1pm session Ps have ResponseID = R_eJV4vfqtz9NxZqZ and R_3qQjxqxllJ5bcTb
# change uids for these Ps to reflect the correct start time
# new uid for ResponseID = R_eJV4vfqtz9NxZqZ becomes 18041213_1_d2
# new uid for ResponseID = R_3qQjxqxllJ5bcTb becomes 18041213_1_g2
#
# 2. Remove ResponseID == R_3JiH3ZnPGYh1jEx from data

adapt <- adapt %>% mutate(uid = ifelse(ResponseID == "R_eJV4vfqtz9NxZqZ", "18041213_1_d2", uid),
                          uid = ifelse(ResponseID == "R_3qQjxqxllJ5bcTb", "18041213_1_g2", uid)) %>%
  filter(ResponseID != "R_3JiH3ZnPGYh1jEx")

n_questions <- 9 # number of questions in this test

x <- table(adapt$uid)

if (any(x != n_questions)) {
  print("Participants not appearing the correct number of times:")
  x[x != n_questions]
} else {
  print("All good! :)")
}

# Check for missing values ================================================
# create vector to select only uid and participants responses
# first convert responses to numeric vector
adapt <- adapt %>%
  mutate(Stimulus.RESP = as.numeric(Stimulus.RESP))

x <- adapt %>%
  select(uid, Stimulus.RESP)

# by column
col_miss <- sapply(x, function(col) {
  col %>%
    is.na() %>%
    sum()
})

# by row
row_miss <- apply(x, 1, function(row) {
  row %>%
    is.na() %>%
    any()
})

if (any(row_miss)) {
  print("missing values for participant:")
  x[row_miss, ] # to print uid if there is a missing value
}  else {
  print("no missing values")
}


# visually check that there are no missing values per user per variable
x %>%
  group_by(uid) %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  gather(key, val, -uid) %>% 
  ggplot(aes(key, uid, fill = factor(val))) +
  geom_tile() +
  scale_fill_brewer(palette = "OrRd") +
  labs(title = "Number of missing values per user, per variable", x = NULL, y = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Check whether vector contains acceptable values (range == 1 - 7) ------------------------
check_values <- function(x, num_min, num_max = NULL) {
  if (is.null(num_max)) {
    any_wrong <- !all(x %in% num_min)
  } else {
    any_wrong <- any(x < num_min | x > num_max, na.rm = TRUE)
  }
  
  if (any_wrong) {
    print("Unacceptable value present in this variable.")
  } else {
    print("All good! :)")
  }
}

check_values(x$Stimulus.RESP, 1, 7)


# factor == "cognitive" is spelt wrong
adapt$factor[adapt$factor == "cogntive"] <- "cognitive"

# calculate variables
adapt.uid <- adapt %>%
  group_by(uid) %>%
  summarise(adapt_cognitive = mean(Stimulus.RESP[factor == "cognitive"], na.rm = T),
            adapt_affective = mean(Stimulus.RESP[factor == "affective"], na.rm = T),
            adapt_global = mean(Stimulus.RESP, na.rm = T))


# Reliability ----------------------------------------------------
# x <- adapt %>%
#   filter(factor == "cognitive") %>% # calculate for each factor
#   select(uid, itemnum, Stimulus.RESP) %>%
#   spread(itemnum, Stimulus.RESP) %>%
#   select(-uid)
# 
# psych::alpha(as.data.frame(x))


# Applying_Decision_Rules ---------------------------------------------------------------------
surveys_qrt$Applying_Decision_Rules_A1 <- as.data.frame(surveys_qrt$Applying_Decision_Rules_A1 %>%
        select(ResponseID, StartDate:Finished, uid, ADRitemNum, Ind_Stimulus.CRESP, 
               Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime,
               Ind_Confidence.RESP, Ind_Confidence.OnsetTime, Ind_Confidence.OffsetTime,
               Ind_Decision.RESP, Ind_Decision.RT, Ind_Decision.OnsetTime, Ind_Decision.OffsetTime,
               Wait.OnsetTime, Wait.OffsetTime, Wait.RT, Grp_Stimulus.RESP, Grp_Stimulus.RT, 
               Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime, Grp_Confidence.RESP, 
               Grp_Confidence.OnsetTime, Grp_Confidence.OffsetTime, Grp_Decision.RESP,
               Grp_Decision.RT, Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
        rename(Stimulus.CRESP = Ind_Stimulus.CRESP) %>%
        mutate(version = "a1") %>%
        group_by(uid, ADRitemNum) %>%
        mutate(Ind_Confidence.OffsetTime = as.numeric(Ind_Confidence.OffsetTime),
               Ind_Confidence.OnsetTime = as.numeric(Ind_Confidence.OnsetTime),
               Grp_Confidence.OffsetTime = as.numeric(Grp_Confidence.OffsetTime),
               Grp_Confidence.OnsetTime = as.numeric(Grp_Confidence.OnsetTime),
               Ind_Confidence.RT = Ind_Confidence.OffsetTime - Ind_Confidence.OnsetTime,
               Grp_Confidence.RT = Grp_Confidence.OffsetTime - Grp_Confidence.OnsetTime))

surveys_qrt$Applying_Decision_Rules_B1 <- as.data.frame(surveys_qrt$Applying_Decision_Rules_B1 %>%
        select(ResponseID, StartDate:Finished, uid, ADRitemNum, Ind_Stimulus.CRESP, 
               Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime,
               Ind_Confidence.RESP, Ind_Confidence.OnsetTime, Ind_Confidence.OffsetTime,
               Ind_Decision.RESP, Ind_Decision.RT, Ind_Decision.OnsetTime, Ind_Decision.OffsetTime,
               Wait.OnsetTime, Wait.OffsetTime, Wait.RT, Grp_Stimulus.RESP, Grp_Stimulus.RT, 
               Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime, Grp_Confidence.RESP, 
               Grp_Confidence.OnsetTime, Grp_Confidence.OffsetTime, Grp_Decision.RESP,
               Grp_Decision.RT, Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
       rename(Stimulus.CRESP = Ind_Stimulus.CRESP) %>%
       mutate(version = "b1") %>%
       group_by(uid, ADRitemNum) %>%
       mutate(Ind_Confidence.OffsetTime = as.numeric(Ind_Confidence.OffsetTime),
              Ind_Confidence.OnsetTime = as.numeric(Ind_Confidence.OnsetTime),
              Grp_Confidence.OffsetTime = as.numeric(Grp_Confidence.OffsetTime),
              Grp_Confidence.OnsetTime = as.numeric(Grp_Confidence.OnsetTime),
              Ind_Confidence.RT = Ind_Confidence.OffsetTime - Ind_Confidence.OnsetTime,
              Grp_Confidence.RT = Grp_Confidence.OffsetTime - Grp_Confidence.OnsetTime))

surveys_java$Applying_Decision_Rules_A2 <- surveys_java$Applying_Decision_Rules_A2 %>%
  rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, ADRitemNum = itemnum, 
         Ind_Stimulus.OnsetTime = ind_stimulus_onset,
         Ind_Stimulus.OffsetTime = ind_stimulus_offset, Ind_Stimulus.RT = ind_stimulus_rt,
         Ind_Stimulus.RESP = ind_response, Ind_Confidence.OnsetTime = ind_confidence_onset,
         Ind_Confidence.OffsetTime = ind_confidence_offset, Ind_Confidence.RT = ind_confidence_rt,
         Ind_Confidence.RESP = ind_confidence, Ind_Decision.OnsetTime = ind_decision_onset,
         Ind_Decision.OffsetTime = ind_decision_offset, Ind_Decision.RT = ind_decision_rt,
         Ind_Decision.RESP = ind_decision, Wait.OnsetTime = wait_onset,
         Wait.OffsetTime = wait_offset, Wait.RT = wait_rt,
         Grp_Stimulus.OnsetTime = grp_stimulus_onset, Grp_Stimulus.OffsetTime = grp_stimulus_offset, 
         Grp_Stimulus.RT = grp_stimulus_rt,Grp_Stimulus.RESP = grp_response, 
         Grp_Confidence.OnsetTime = grp_confidence_onset, Grp_Confidence.OffsetTime = grp_confidence_offset, 
         Grp_Confidence.RT = grp_confidence_rt, Grp_Confidence.RESP = grp_confidence, 
         Grp_Decision.OnsetTime = grp_decision_onset, Grp_Decision.OffsetTime = grp_decision_offset,
         Grp_Decision.RT = grp_decision_rt, Grp_Decision.RESP = grp_decision) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, ADRitemNum, Stimulus.CRESP, 
         Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime,
         Ind_Confidence.RESP, Ind_Confidence.RT, Ind_Confidence.OnsetTime, Ind_Confidence.OffsetTime,
         Ind_Decision.RESP, Ind_Decision.RT, Ind_Decision.OnsetTime, Ind_Decision.OffsetTime,
         Wait.OnsetTime, Wait.OffsetTime, Wait.RT, Grp_Stimulus.RESP, Grp_Stimulus.RT, 
         Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime, Grp_Confidence.RESP, Grp_Confidence.RT,
         Grp_Confidence.OnsetTime, Grp_Confidence.OffsetTime, Grp_Decision.RESP,
         Grp_Decision.RT, Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
  mutate(version = "a2")


surveys_java$Applying_Decision_Rules_B2 <- surveys_java$Applying_Decision_Rules_B2 %>%
  rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, ADRitemNum = itemnum, 
         Ind_Stimulus.OnsetTime = ind_stimulus_onset,
         Ind_Stimulus.OffsetTime = ind_stimulus_offset, Ind_Stimulus.RT = ind_stimulus_rt,
         Ind_Stimulus.RESP = ind_response, Ind_Confidence.OnsetTime = ind_confidence_onset,
         Ind_Confidence.OffsetTime = ind_confidence_offset, Ind_Confidence.RT = ind_confidence_rt,
         Ind_Confidence.RESP = ind_confidence, Ind_Decision.OnsetTime = ind_decision_onset,
         Ind_Decision.OffsetTime = ind_decision_offset, Ind_Decision.RT = ind_decision_rt,
         Ind_Decision.RESP = ind_decision, Wait.OnsetTime = wait_onset,
         Wait.OffsetTime = wait_offset, Wait.RT = wait_rt,
         Grp_Stimulus.OnsetTime = grp_stimulus_onset, Grp_Stimulus.OffsetTime = grp_stimulus_offset, 
         Grp_Stimulus.RT = grp_stimulus_rt,Grp_Stimulus.RESP = grp_response, 
         Grp_Confidence.OnsetTime = grp_confidence_onset, Grp_Confidence.OffsetTime = grp_confidence_offset, 
         Grp_Confidence.RT = grp_confidence_rt, Grp_Confidence.RESP = grp_confidence, 
         Grp_Decision.OnsetTime = grp_decision_onset, Grp_Decision.OffsetTime = grp_decision_offset,
         Grp_Decision.RT = grp_decision_rt, Grp_Decision.RESP = grp_decision) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, ADRitemNum, Stimulus.CRESP, 
         Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime,
         Ind_Confidence.RESP, Ind_Confidence.RT, Ind_Confidence.OnsetTime, Ind_Confidence.OffsetTime,
         Ind_Decision.RESP, Ind_Decision.RT, Ind_Decision.OnsetTime, Ind_Decision.OffsetTime,
         Wait.OnsetTime, Wait.OffsetTime, Wait.RT, Grp_Stimulus.RESP, Grp_Stimulus.RT, 
         Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime, Grp_Confidence.RESP, Grp_Confidence.RT,
         Grp_Confidence.OnsetTime, Grp_Confidence.OffsetTime, Grp_Decision.RESP,
         Grp_Decision.RT, Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
  mutate(version = "b2")


# combine data from a1 and b1 into a single df 
# and rename first 10 columns labelled V1 - V10
adr <- rbind(surveys_qrt$Applying_Decision_Rules_A1, surveys_qrt$Applying_Decision_Rules_B1,
             surveys_java$Applying_Decision_Rules_A2, surveys_java$Applying_Decision_Rules_B2)

# Data Screening ----------------------------------------------------------

# Check participants appear correct number of times =======================
# ISSUE: 
# 1. identified that the following uids appear twice
#   "18041210_1_d2"   = all responses legit, P from 2pm session incorrectly assigned same uid
#   "18041210_1_g2"   = all responses legit, P from 2pm session incorrectly assigned same uid
#   "18041210_2_d1"   = duplicate contains only NAs. ResponseIDs == R_dcjLLy62SrBmSJD
#   "18041210_2_g1"   = duplicate contains only NAs. ResponseID == R_7VWnEvF8GQL0nm5
#
# SOLUTION: 
# 1. Legit 10am Ps have ResponseIDs = R_eWC7cASZvnfdpT7 and R_7Pvv5DcpK46NEDX
# incorrect ResponseIDs = R_6rHBoBnN9DirpWd and R_elgWv0t4ubB3y7j started at 1pm same day
# change uids for these Ps to reflect the correct start time
# new uid for ResponseID = R_6rHBoBnN9DirpWd becomes 18041213_1_d2
# new uid for ResponseID = R_elgWv0t4ubB3y7j becomes 18041213_1_g2

# adr %>% filter(., !str_detect(uid, "[0-9]_[0-9]_[d-g][1-2]")) %>% select(uid)

# x <- adr %>%
#   filter(uid == "17100609_g1" | uid == "17102710_2_g1") %>%
#   select(ResponseID, uid, StartDate:Finished, ADRitemNum, Ind_Stimulus.RESP, Grp_Stimulus.RESP,
#          Ind_Confidence.RESP, Grp_Confidence.RESP, Ind_Decision.RESP, Grp_Decision.RESP)

# x <- adr %>% group_by(uid) %>%
#   summarise(StartDate = StartDate[1])

adr <- adr %>% mutate(uid = ifelse(ResponseID == "R_6rHBoBnN9DirpWd", "18041213_1_d2", uid),
                    uid = ifelse(ResponseID == "R_elgWv0t4ubB3y7j", "18041213_1_g2", uid)) %>%
  filter(ResponseID != "R_7VWnEvF8GQL0nm5" & ResponseID != "R_dcjLLy62SrBmSJD")

n_questions <- 10 # number of questions in this test

x <- table(adr$uid)

if (any(x != n_questions)) {
  print("Participants not appearing the correct number of times:")
  x[x != n_questions]
} else {
  print("All good! :)")
}

# Check for missing values ================================================
# identified 2 uids with all NA. Remove
adr <- adr %>% filter(Finished != "False")

# create vector to select only uid and participants responses
# first convert confidence vars to numeric

adr <- adr %>%
  mutate(Ind_Confidence.RESP = as.numeric(Ind_Confidence.RESP),
         Grp_Confidence.RESP = as.numeric(Grp_Confidence.RESP))

x <- adr %>%
  select(uid, Ind_Stimulus.RESP, Grp_Stimulus.RESP, Ind_Confidence.RESP, 
         Grp_Confidence.RESP, Ind_Decision.RESP, Grp_Decision.RESP)

# by column
col_miss <- sapply(x, function(col) {
  col %>%
    is.na() %>%
    sum()
})

# by row
row_miss <- apply(x, 1, function(row) {
  row %>%
    is.na() %>%
    any()
})

if (any(row_miss)) {
  print("missing values for participant:")
  x[row_miss, ] # to print uid if there is a missing value
}  else {
  print("no missing values")
}

# visually check that there are no missing values per user per variable
x %>%
  group_by(uid) %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  gather(key, val, -uid) %>% 
  ggplot(aes(key, uid, fill = factor(val))) +
  geom_tile() +
  scale_fill_brewer(palette = "OrRd") +
  labs(title = "Number of missing values per user, per variable", x = NULL, y = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Check whether vector contains acceptable values ("a-e" or "none") ------------------------
# first convert all character responses to lower case and arrange strings in 
# alphabetical order

# One participant entered "nona" which is clearly supposed to be "none"
# a number of participants entered invalid responses
# For Ind_Stimulus.RESP: "aq", "av", "f", "r", "w"
# For Grp_Stimulus.RESP: "s", "w"
# Not sure how to handle. Check with SK

# function to sort character strings
string_sort <- function(x) paste0(sort(unlist(strsplit(x, "")), decreasing = F), collapse = "")


adr <- adr %>% mutate(Ind_Stimulus.RESP = tolower(Ind_Stimulus.RESP),
                      Grp_Stimulus.RESP = tolower(Grp_Stimulus.RESP),
                      Ind_Decision.RESP = tolower(Ind_Decision.RESP),
                      Grp_Decision.RESP = tolower(Grp_Decision.RESP))

adr <- adr %>% mutate(Ind_Stimulus.RESP = ifelse(Ind_Stimulus.RESP == "nona", "none", Ind_Stimulus.RESP),
                      Ind_Stimulus.RESP = ifelse(Ind_Stimulus.RESP == "w", "e", Ind_Stimulus.RESP),
                      Ind_Stimulus.RESP = ifelse(Ind_Stimulus.RESP == "r", "e", Ind_Stimulus.RESP),
                      Ind_Stimulus.RESP = ifelse(Ind_Stimulus.RESP == "av", "ac", Ind_Stimulus.RESP),
                      Grp_Stimulus.RESP = ifelse(Grp_Stimulus.RESP == "w", "a", Grp_Stimulus.RESP),
                      Grp_Stimulus.RESP = ifelse(Grp_Stimulus.RESP == "s", "a", Grp_Stimulus.RESP))

# the 2nd row ended with Ind_Stimulus.RESP just changed to Grp_Stimulus.RESP (need to check that this is correct)
adr <- adr %>% mutate(Ind_Stimulus.RESP = ifelse(Ind_Stimulus.RESP != "none", sapply(adr$Ind_Stimulus.RESP, string_sort), Ind_Stimulus.RESP),
                      Grp_Stimulus.RESP = ifelse(Grp_Stimulus.RESP != "none", sapply(adr$Grp_Stimulus.RESP, string_sort), Grp_Stimulus.RESP))


# add Stimulus.ACC column (Stimulus.RESP == Stimulus.CRESP)
adr <- adr %>% 
  group_by(uid, ADRitemNum) %>%
  mutate(Ind_Stimulus.ACC = Ind_Stimulus.RESP == Stimulus.CRESP,
         Grp_Stimulus.ACC = Grp_Stimulus.RESP == Stimulus.CRESP) %>%
  ungroup()


# # Check teams are giving same response and decision =======================
# Create grouping variables 
# group == date + group
# member == g == 1, d == 2
adr <- adr %>% mutate(member = ifelse(grepl("g[1-2]", uid), "1", 
                                      ifelse(grepl("d[1-2]", uid), "2", 
                                             ifelse(grepl("e1", uid), "1", "2"))),
                      recruit = ifelse(grepl("[dg]", uid), "s", "e"),
                      group = gsub("_[a-z][12]", "", uid))


# Decisions
# in cases where member's group decisions differ change to missing
# Justification: If decision is different for members it is impossible to
# determine which decision was agreed upon by members. As decision was provided last
# other data for same item remains valid, if matching.
x <- adr %>% select(group, member, ADRitemNum, Grp_Decision.RESP) %>%
  spread(member, Grp_Decision.RESP) %>%
  group_by(group, ADRitemNum) %>%
  mutate(dif = `1` != `2`) %>%
  filter(dif == TRUE)


# prints out number of times that group decisions differed between members
cat("number of times that group decision differed between members:", nrow(x))


for (i in seq(nrow(x))){
  group <- x$group[i]
  ADRitemNum <- x$ADRitemNum[i]
  adr[adr$group == group & adr$ADRitemNum == ADRitemNum, 
      c("Grp_Decision.RESP", "Grp_Decision.OnsetTime", "Grp_Decision.OffsetTime", 
        "Grp_Decision.RT")] <- NA
}


# # Response
# # in cases where either is different change to missing for all variables
# # (Grp_Stimulus.ACC, Grp_Confidence.RESP, Grp_Decision.RESP)
# # Justification: If accuracy differs for members it is impossible to
# # determine which response was agreed upon by members. Confidence and decision
# # were contingent upon accuracy, as it was provided first.

x <- adr %>% select(group, member, ADRitemNum, Grp_Stimulus.RESP) %>%
  spread(member, Grp_Stimulus.RESP) %>%
  group_by(group, ADRitemNum) %>%
  mutate(dif = `1` != `2`) %>%
  filter(dif == TRUE)

# prints out number of times that group accuracy differed between members
cat("number of times that group accuracy differed between members:", nrow(x))


for (i in seq(nrow(x))){
  group <- x$group[i]
  ADRitemNum <- x$ADRitemNum[i]
  adr[adr$group == group & adr$ADRitemNum == ADRitemNum, 
      c("Grp_Stimulus.RESP", "Grp_Stimulus.ACC", "Grp_Confidence.RESP", "Grp_Decision.RESP")] <- NA
}


# Compute all variables ------------------------------------------------
adr.uid <- adr %>%
  group_by(uid) %>%
  summarise(
    group = group[1],
    adr.ind.acc = mean(Ind_Stimulus.ACC) * 100,
    adr.grp.acc = mean(Grp_Stimulus.ACC, na.rm = TRUE) * 100,
    adr.ind.conf = mean(Ind_Confidence.RESP),
    adr.grp.conf = mean(Grp_Confidence.RESP, na.rm = TRUE),
    adr.ind.dec = mean(Ind_Decision.RESP == "y") * 100,
    adr.grp.dec = mean(Grp_Decision.RESP == "y", na.rm = TRUE) * 100,
    adr.ind.rec = 100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 0, na.rm = TRUE) 
    / mean(Ind_Stimulus.ACC == 0, na.rm = TRUE),
    adr.grp.rec = 100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 0, na.rm = TRUE) 
    / mean(Grp_Stimulus.ACC == 0, na.rm = TRUE))
    # adr.ind.comp = (100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 1, na.rm = TRUE)) + 
    #  (100 * mean(Ind_Decision.RESP == "n" & Ind_Stimulus.ACC == 0, na.rm = TRUE)) / 30,
    # adr.grp.comp = (100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 1, na.rm = TRUE)) + 
    #  (100 * mean(Grp_Decision.RESP == "n" & Grp_Stimulus.ACC == 0, na.rm = TRUE)) / 30,
    # adr.ind.optim = (100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 1, na.rm = TRUE)) 
    # / 30,
    # adr.grp.optim = (100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 1, na.rm = TRUE)) 
    # / 30,
    # adr.ind.hes = 100 * mean(Ind_Decision.RESP == "n" & Ind_Stimulus.ACC == 1, na.rm = TRUE) 
    # / mean(Ind_Stimulus.ACC == 1, na.rm = TRUE),
    # adr.grp.hes = 100 * mean(Grp_Decision.RESP == "n" & Grp_Stimulus.ACC == 1, na.rm = TRUE) 
    # / mean(Grp_Stimulus.ACC == 1, na.rm = TRUE))



# Reliability ----------------------------------------------------
# # FOR INDIVIDUALS
# # Accuracy
# 
# x <-  adr %>%
#   select(uid, ADRitemNum, Ind_Stimulus.ACC) %>%
#   mutate(Ind_Stimulus.ACC = ifelse(Ind_Stimulus.ACC == TRUE, 1, 0)) %>%
#   spread(ADRitemNum, Ind_Stimulus.ACC) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha$total$raw_alpha
# 
# # Confidence
# x <- adr %>%
#   select(uid, ADRitemNum, Ind_Confidence.RESP) %>%
#   spread(ADRitemNum, Ind_Confidence.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Decisiveness
# x <- adr %>%
#   select(uid, ADRitemNum, Ind_Decision.RESP)
# 
# x[x == "y"] <- 1
# x[x == "n"] <- 0
# 
# x <- transform(x, Ind_Decision.RESP = as.numeric(Ind_Decision.RESP))
# 
# x <- x %>%
#   spread(ADRitemNum, Ind_Decision.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Recklessness
# # Using split-half reliability adjusted using the Spearman-Brown prophecy formula
# x <- d %>%
#   select(uid, ADRitemNum, Ind_Stimulus.ACC, Ind_Decision.RESP) %>%
#   filter(Ind_Stimulus.ACC == 0) %>%
#   select(-Ind_Stimulus.ACC) %>% 
#   mutate(Ind_Decision.RESP = ifelse(Ind_Decision.RESP == "y", 1, 0))
# 
# odd1 <- c(1, 3, 5, 7, 9)
# 
# odd <- filter(x, ADRitemNum %in% odd1) %>% 
#   group_by(uid) %>%
#   summarise(odd = sum(Ind_Decision.RESP, na.rm = TRUE))
# 
# even1 <- c(2, 4, 6, 8, 10)
# 
# even <- filter(x, ADRitemNum %in% even1) %>% 
#   group_by(uid) %>%
#   summarise(even = sum(Ind_Decision.RESP, na.rm = TRUE))
# 
# x <- merge(odd, even) %>%
#   mutate(total = odd + even) %>%
#   ungroup(uid) %>%
#   select(-uid)
# 
# cor.test(x$odd, x$even)
# 
# # Adjust r with the Spearman-Brown prophecy formula == (2*r) / (1+r)
# (2*0.4328238) / (1+0.4328238)
#
#
# # FOR TEAMS
# Accuracy
# 
# x <- adr %>%
#   select(uid, ADRitemNum, Grp_Stimulus.ACC) %>%
#   mutate(Grp_Stimulus.ACC = ifelse(Grp_Stimulus.ACC == TRUE, 1, 0)) %>%
#   spread(ADRitemNum, Grp_Stimulus.ACC) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Confidence
# x <- adr %>%
#   select(uid, ADRitemNum, Grp_Confidence.RESP) %>%
#   spread(ADRitemNum, Grp_Confidence.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Decisiveness
# x <- adr %>%
#   select(uid, ADRitemNum, Grp_Decision.RESP)
# 
# x[x == "y"] <- 1
# x[x == "n"] <- 0
# 
# x <- transform(x, Grp_Decision.RESP = as.numeric(Grp_Decision.RESP))
# 
# x <- x %>%
#   spread(ADRitemNum, Grp_Decision.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Recklessness
# x <- adr %>%
#   select(uid, ADRitemNum, Grp_Stimulus.ACC, Grp_Decision.RESP) %>%
#   filter(Grp_Stimulus.ACC == 0) %>%
#   select(-Grp_Stimulus.ACC)
# 
# x[x == "y"] <- 1
# x[x == "n"] <- 0
# 
# x <- transform(x, Grp_Decision.RESP = as.numeric(Grp_Decision.RESP))
# 
# odd1 <- c(1, 3, 5, 7, 9)
# 
# odd <- filter(x, ADRitemNum %in% odd1)
# 
# even1 <- c(2, 4, 6, 8, 10)
# 
# even <- filter(x, ADRitemNum %in% even1)
# 
# even <- even %>%
#   group_by(uid) %>%
#   summarise(even = sum(Grp_Decision.RESP, na.rm = TRUE))
# 
# odd <- odd %>%
#   group_by(uid) %>%
#   summarise(odd = sum(Grp_Decision.RESP, na.rm = TRUE))
# 
# x <- merge(odd, even) %>%
#   mutate(total = odd + even) %>%
#   ungroup(uid) %>%
#   select(-uid)
# 
# # write.csv(x, "RAPM_splithalf.csv")
# 
# cor.test(x$odd, x$even)
# 
# use this website to correct using the spearman-brown prophecy formula
# https://www.cedu.niu.edu/~walker/calculators/sbpf.asp







# Cognitive_Reflection_Test -----------------------------------------------
surveys_qrt$CRT_A1 <- as.data.frame(surveys_qrt$CRT_A1 %>% 
  select(ResponseID, StartDate:Finished, uid, CRTItemNum, Ind_Stimulus.CRESP, 
         Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime,
         Ind_Confidence.RESP, Ind_Confidence.OnsetTime, Ind_Confidence.OffsetTime,
         Ind_Decision.RESP, Ind_Decision.RT, Ind_Decision.OnsetTime, Ind_Decision.OffsetTime,
         Wait.OnsetTime, Wait.OffsetTime, Wait.RT, Grp_Stimulus.RESP, Grp_Stimulus.RT, 
         Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime, Grp_Confidence.RESP, 
         Grp_Confidence.OnsetTime, Grp_Confidence.OffsetTime, Grp_Decision.RESP,
         Grp_Decision.RT, Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
  rename(Stimulus.CRESP = Ind_Stimulus.CRESP) %>%
  mutate(version = "a1") %>%
  group_by(uid, CRTItemNum) %>%
  mutate(Ind_Confidence.OffsetTime = as.numeric(Ind_Confidence.OffsetTime),
         Ind_Confidence.OnsetTime = as.numeric(Ind_Confidence.OnsetTime),
         Grp_Confidence.OffsetTime = as.numeric(Grp_Confidence.OffsetTime),
         Grp_Confidence.OnsetTime = as.numeric(Grp_Confidence.OnsetTime),
         Ind_Confidence.RT = Ind_Confidence.OffsetTime - Ind_Confidence.OnsetTime,
         Grp_Confidence.RT = Grp_Confidence.OffsetTime - Grp_Confidence.OnsetTime))

surveys_qrt$CRT_B1 <- as.data.frame(surveys_qrt$CRT_B1 %>%
                select(ResponseID, StartDate:Finished, uid, CRTItemNum, Ind_Stimulus.CRESP, 
                       Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime,
                       Ind_Confidence.RESP, Ind_Confidence.OnsetTime, Ind_Confidence.OffsetTime,
                       Ind_Decision.RESP, Ind_Decision.RT, Ind_Decision.OnsetTime, Ind_Decision.OffsetTime,
                       Wait.OnsetTime, Wait.OffsetTime, Wait.RT, Grp_Stimulus.RESP, Grp_Stimulus.RT, 
                       Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime, Grp_Confidence.RESP, 
                       Grp_Confidence.OnsetTime, Grp_Confidence.OffsetTime, Grp_Decision.RESP,
                       Grp_Decision.RT, Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
                rename(Stimulus.CRESP = Ind_Stimulus.CRESP) %>%
                mutate(version = "b1") %>%
                group_by(uid, CRTItemNum) %>%
                mutate(Ind_Confidence.OffsetTime = as.numeric(Ind_Confidence.OffsetTime),
                       Ind_Confidence.OnsetTime = as.numeric(Ind_Confidence.OnsetTime),
                       Grp_Confidence.OffsetTime = as.numeric(Grp_Confidence.OffsetTime),
                       Grp_Confidence.OnsetTime = as.numeric(Grp_Confidence.OnsetTime),
                       Ind_Confidence.RT = Ind_Confidence.OffsetTime - Ind_Confidence.OnsetTime,
                       Grp_Confidence.RT = Grp_Confidence.OffsetTime - Grp_Confidence.OnsetTime))

surveys_java$CRT_A2 <- surveys_java$CRT_A2 %>%
  rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, CRTItemNum = itemnum,
         Ind_Stimulus.OnsetTime = ind_stimulus_onset,
         Ind_Stimulus.OffsetTime = ind_stimulus_offset, Ind_Stimulus.RT = ind_stimulus_rt,
         Ind_Stimulus.RESP = ind_response, Ind_Confidence.OnsetTime = ind_confidence_onset,
         Ind_Confidence.OffsetTime = ind_confidence_offset, Ind_Confidence.RT = ind_confidence_rt,
         Ind_Confidence.RESP = ind_confidence, Ind_Decision.OnsetTime = ind_decision_onset,
         Ind_Decision.OffsetTime = ind_decision_offset, Ind_Decision.RT = ind_decision_rt,
         Ind_Decision.RESP = ind_decision, Wait.OnsetTime = wait_onset,
         Wait.OffsetTime = wait_offset, Wait.RT = wait_rt,
         Grp_Stimulus.OnsetTime = grp_stimulus_onset, Grp_Stimulus.OffsetTime = grp_stimulus_offset,
         Grp_Stimulus.RT = grp_stimulus_rt,Grp_Stimulus.RESP = grp_response,
         Grp_Confidence.OnsetTime = grp_confidence_onset, Grp_Confidence.OffsetTime = grp_confidence_offset,
         Grp_Confidence.RT = grp_confidence_rt, Grp_Confidence.RESP = grp_confidence,
         Grp_Decision.OnsetTime = grp_decision_onset, Grp_Decision.OffsetTime = grp_decision_offset,
         Grp_Decision.RT = grp_decision_rt, Grp_Decision.RESP = grp_decision) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, CRTItemNum, Stimulus.CRESP, 
         Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime,
         Ind_Confidence.RESP, Ind_Confidence.RT, Ind_Confidence.OnsetTime, Ind_Confidence.OffsetTime,
         Ind_Decision.RESP, Ind_Decision.RT, Ind_Decision.OnsetTime, Ind_Decision.OffsetTime,
         Wait.OnsetTime, Wait.OffsetTime, Wait.RT, Grp_Stimulus.RESP, Grp_Stimulus.RT, 
         Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime, Grp_Confidence.RESP, Grp_Confidence.RT,
         Grp_Confidence.OnsetTime, Grp_Confidence.OffsetTime, Grp_Decision.RESP,
         Grp_Decision.RT, Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
  mutate(version = "a2")

surveys_java$CRT_B2 <- surveys_java$CRT_B2 %>%
  rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, CRTItemNum = itemnum,
         Ind_Stimulus.OnsetTime = ind_stimulus_onset,
         Ind_Stimulus.OffsetTime = ind_stimulus_offset, Ind_Stimulus.RT = ind_stimulus_rt,
         Ind_Stimulus.RESP = ind_response, Ind_Confidence.OnsetTime = ind_confidence_onset,
         Ind_Confidence.OffsetTime = ind_confidence_offset, Ind_Confidence.RT = ind_confidence_rt,
         Ind_Confidence.RESP = ind_confidence, Ind_Decision.OnsetTime = ind_decision_onset,
         Ind_Decision.OffsetTime = ind_decision_offset, Ind_Decision.RT = ind_decision_rt,
         Ind_Decision.RESP = ind_decision, Wait.OnsetTime = wait_onset,
         Wait.OffsetTime = wait_offset, Wait.RT = wait_rt,
         Grp_Stimulus.OnsetTime = grp_stimulus_onset, Grp_Stimulus.OffsetTime = grp_stimulus_offset,
         Grp_Stimulus.RT = grp_stimulus_rt,Grp_Stimulus.RESP = grp_response,
         Grp_Confidence.OnsetTime = grp_confidence_onset, Grp_Confidence.OffsetTime = grp_confidence_offset,
         Grp_Confidence.RT = grp_confidence_rt, Grp_Confidence.RESP = grp_confidence,
         Grp_Decision.OnsetTime = grp_decision_onset, Grp_Decision.OffsetTime = grp_decision_offset,
         Grp_Decision.RT = grp_decision_rt, Grp_Decision.RESP = grp_decision) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, CRTItemNum, Stimulus.CRESP, 
         Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime,
         Ind_Confidence.RESP, Ind_Confidence.RT, Ind_Confidence.OnsetTime, Ind_Confidence.OffsetTime,
         Ind_Decision.RESP, Ind_Decision.RT, Ind_Decision.OnsetTime, Ind_Decision.OffsetTime,
         Wait.OnsetTime, Wait.OffsetTime, Wait.RT, Grp_Stimulus.RESP, Grp_Stimulus.RT, 
         Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime, Grp_Confidence.RESP, Grp_Confidence.RT,
         Grp_Confidence.OnsetTime, Grp_Confidence.OffsetTime, Grp_Decision.RESP,
         Grp_Decision.RT, Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
  mutate(version = "b2")


# combine data from a1 and b1 into a single df 
# and rename first 10 columns labelled V1 - V10
crt <- rbind(surveys_qrt$CRT_A1, surveys_qrt$CRT_B1,
             surveys_java$CRT_A2, surveys_java$CRT_B2)

# Data Screening ----------------------------------------------------------

# Check participants appear correct number of times =======================
# ISSUE 1: Identified that the following uids appear twice
#   "18041210_1_d2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_1_g2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#
# SOLUTION 1: change uids for these Ps to reflect the correct start time
# new uid for ResponseID = R_2sKj3WhURuFY3Pf becomes 18041213_1_d2
# new uid for ResponseID = R_9Hzo2Sfvv4IS6QB becomes 18041213_1_g2
#
# ISSUE 2: Identified that uid == 18062615_1_e1 appears 3 times. 
# The browser crashed so I had to restart the survey. 
#
# SOLUTION 2. Remove ResponseID == "R_6lPgzrzx2p94I2V" and join responses for 
# Q1-6 from attempt 1 (ResponseID == "R_brOLA2jTCWJJdGJ") with Q7 from 
# attempt 3 (ResponseID == "R_29bx9o8A7LOakQt"). Due to the glitch
#  response for Q6 was not recorded so NA 

# crt %>% filter(., !str_detect(uid, "[0-9]_[0-9]_[d-g][1-2]")) %>% select(uid)
#
# x <- crt %>%
#   filter(uid == "17100609_2_g1" | uid == "18062615_1_e1") %>%
#   select(ResponseID, uid, StartDate:Finished, CRTItemNum, Ind_Stimulus.RESP, Grp_Stimulus.RESP,
#          Ind_Confidence.RESP, Grp_Confidence.RESP, Ind_Decision.RESP, Grp_Decision.RESP)
# 
# x <- crt %>% group_by(uid) %>%
#   summarise(StartDate = StartDate[1])

# Get Attempt 1 responses
x <- crt %>% filter(ResponseID == "R_brOLA2jTCWJJdGJ")

# Which variables will I replace?
vars <- c("Ind_Stimulus.RESP", "Ind_Stimulus.RT",
          "Ind_Stimulus.OnsetTime", "Ind_Stimulus.OffsetTime", "Ind_Confidence.RESP",
          "Ind_Confidence.OnsetTime", "Ind_Confidence.OffsetTime", "Ind_Decision.RESP",
          "Ind_Decision.RT", "Ind_Decision.OffsetTime", "Ind_Decision.OnsetTime",
          "Grp_Stimulus.RESP", "Grp_Stimulus.RT", "Grp_Stimulus.OnsetTime",
          "Grp_Stimulus.OffsetTime", "Grp_Confidence.RESP", "Grp_Confidence.OnsetTime",
          "Grp_Confidence.OffsetTime", "Grp_Decision.RESP", "Grp_Decision.RT",
          "Grp_Decision.OffsetTime", "Grp_Decision.OnsetTime")

x <- arrange(x, CRTItemNum)  # arrange by trial number

x <- x[1:5, vars]  # Take rows 1 to 5 (corresponding to answered questions)
# and variables of interest

# Insert data from x into relevant part of df
crt[crt$ResponseID == "R_29bx9o8A7LOakQt" & crt$CRTItemNum <= 5, vars] <- x


# Modify uids and remove duplicate attempts
crt <- crt %>% mutate(uid = ifelse(ResponseID == "R_2sKj3WhURuFY3Pf", "18041213_1_d2", uid),
                      uid = ifelse(ResponseID == "R_9Hzo2Sfvv4IS6QB", "18041213_1_g2", uid)) %>%
  filter(ResponseID != "R_eSefr7FC8RyG2m9" & ResponseID != "R_cGS3vokzoiOWpj7" & 
           ResponseID != "R_brOLA2jTCWJJdGJ" & ResponseID != "R_6lPgzrzx2p94I2V")

n_questions <- 7 # number of questions in this test

x <- table(crt$uid)

if (any(x != n_questions)) {
  print("Participants not appearing the correct number of times:")
  x[x != n_questions]
} else {
  print("All good! :)")
}

# Check for missing values ================================================
# identified that uid == 18062615_1_e1 had NAs for all Q6 responses
# change their teammate's responses to NA for same question
crt[crt$uid == "18062615_1_e2" & crt$CRTItemNum == "6", vars] <- NA

# Remove NA rows for both uids == 18062615_1_e1 & 18062615_1_e2 for Q6
crt <- crt[!is.na(crt$Ind_Stimulus.RESP), ]

# create vector to select only uid and participants responses
x <- crt %>%
  select(uid, CRTItemNum, Ind_Stimulus.RESP, Grp_Stimulus.RESP, Ind_Confidence.RESP, 
         Grp_Confidence.RESP, Ind_Decision.RESP, Grp_Decision.RESP)

# by column
col_miss <- sapply(x, function(col) {
  col %>%
    is.na() %>%
    sum()
})

# by row
row_miss <- apply(x, 1, function(row) {
  row %>%
    is.na() %>%
    any()
})

check <- if (any(row_miss)) {
  print("missing values for participant:")
  x[row_miss, ] # to print uid if there is a missing value
}  else {
  print("no missing values")
}


# visually check that there are no missing values per user per variable
x %>%
  group_by(uid) %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  gather(key, val, -uid) %>% 
  ggplot(aes(key, uid, fill = factor(val))) +
  geom_tile() +
  scale_fill_brewer(palette = "OrRd") +
  labs(title = "Number of missing values per user, per variable", x = NULL, y = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Check whether vector contains acceptable values () ------------------------
# identified that a number of responses contained spaces before the Ps answer
# so removed the space
# crt <- crt %>% mutate(Ind_Confidence.RESP = ifelse(Ind_Confidence.RESP == " 10", 
#                                                    "10", Ind_Confidence.RESP),
#                       Ind_Stimulus.RESP = ifelse(Ind_Stimulus.RESP == " 20", 
#                                                    "20", Ind_Confidence.RESP),
#                       Ind_Stimulus.RESP = ifelse(Ind_Stimulus.RESP == " 45", 
#                                                  "45", Ind_Confidence.RESP))

# identified that a number of Ps entered response to bat & ball problem in dollars
# only convert "0.05" to "5" cents. All others are already incorrect.
crt <- crt %>% mutate(Ind_Decision.RESP = tolower(Ind_Decision.RESP),
                      Grp_Decision.RESP = tolower(Grp_Decision.RESP),
                      Ind_Stimulus.RESP = as.numeric(Ind_Stimulus.RESP),
                      Grp_Stimulus.RESP = as.numeric(Grp_Stimulus.RESP),
                      Ind_Confidence.RESP = as.numeric(Ind_Confidence.RESP),
                      Grp_Confidence.RESP = as.numeric(Grp_Confidence.RESP),
                      Ind_Stimulus.RESP = ifelse(Ind_Stimulus.RESP == 0.05, 5, Ind_Stimulus.RESP),
                      Grp_Stimulus.RESP = ifelse(Grp_Stimulus.RESP == 0.05, 5, Grp_Stimulus.RESP))

check_values <- function(x, num_min, num_max = NULL) {
  if (is.null(num_max)) {
    any_wrong <- !all(x %in% num_min)
  } else {
    any_wrong <- any(x < num_min | x > num_max, na.rm = TRUE)
  }
  
  if (any_wrong) {
    print("Unacceptable value present in this variable.")
  } else {
    print("All good! :)")
  }
}

check_values(crt$Ind_Stimulus.RESP, 0, 6000)
check_values(crt$Ind_Confidence.RESP, 0, 100)
check_values(crt$Ind_Decision.RESP, c("y", "n"))

check_values(crt$Grp_Stimulus.RESP, 0, 15000)
check_values(crt$Grp_Confidence.RESP, 0, 100)
check_values(crt$Grp_Decision.RESP, c("y", "n"))

# add Stimulus.ACC column (Stimulus.RESP == Stimulus.CRESP)
crt <- crt %>%
  group_by(uid, CRTItemNum) %>%
  mutate(Ind_Stimulus.ACC = Ind_Stimulus.RESP == Stimulus.CRESP,
         Grp_Stimulus.ACC = Grp_Stimulus.RESP == Stimulus.CRESP,
         Ind_Stimulus.OffsetTime = as.numeric(Ind_Stimulus.OffsetTime),
         Ind_Stimulus.OnsetTime = as.numeric(Ind_Stimulus.OnsetTime),
         Ind_Confidence.OffsetTime = as.numeric(Ind_Confidence.OffsetTime),
         Ind_Confidence.OnsetTime = as.numeric(Ind_Confidence.OnsetTime),
         Ind_Decision.OffsetTime = as.numeric(Ind_Decision.OffsetTime),
         Ind_Decision.OnsetTime = as.numeric(Ind_Decision.OnsetTime),
         Ind_Stimulus.RT = Ind_Stimulus.OffsetTime - Ind_Stimulus.OnsetTime,
         Ind_Confidence.RT = Ind_Confidence.OffsetTime - Ind_Confidence.OnsetTime,
         Ind_Decision.RT = Ind_Decision.OffsetTime - Ind_Decision.OnsetTime) %>%
  ungroup()

         
# Check teams are giving same response and decision =======================
# Create grouping variables 
# group == date + group
# member == g == 1, d == 2
crt <- crt %>% mutate(member = ifelse(grepl("g[1-2]", uid), "1", 
                                      ifelse(grepl("d[1-2]", uid), "2", 
                                             ifelse(grepl("e1", uid), "1", "2"))),
                    recruit = ifelse(grepl("[dg]", uid), "s", "e"),
                    group = gsub("_[a-z][12]", "", uid))

# discovered that uid == "17100609_2_g1" does not have a teammate
# Not sure where their data went? All participants were in teams
# remove for now and investigate later
# NOTE: I think the other uids were teams that did not appear to follow instructions
# i.e., entered different reseponse for majority of team Qs

crt <- crt %>% filter(uid != "17100609_2_g1") 


x <- crt %>% 
  group_by(group) %>% 
  group_by(group, CRTItemNum) %>% 
  mutate(match = Grp_Stimulus.ACC[1] != Grp_Stimulus.ACC[2]) %>% 
  group_by(group) %>% 
  mutate(n = sum(match))

x %>% 
  select(group, n, version) %>% 
  group_by(group) %>% 
  summarise(n = n[1] / 2, 
            version = version[1]) %>% 
  arrange(desc(n)) # %>% 
# write_csv("output/number_diff_group_resp.csv")

# Decisions
# in cases where member's group decisions differ change to missing
# Justification: If decision is different for members it is impossible to
# determine which decision was agreed upon by members. As decision was provided last
# other data for same item remains valid, if matching.

x <- crt %>% select(group, member, CRTItemNum, Grp_Decision.RESP) %>% 
  spread(member, Grp_Decision.RESP) %>%
  group_by(group, CRTItemNum) %>%
  mutate(dif = `1` != `2`) %>%
  filter(dif == TRUE)

# prints out number of times that group decisions differed between members
cat("number of times that group decision differed between members:", nrow(x))

for (i in seq(nrow(x))){
  group <- x$group[i]
  CRTItemNum <- x$CRTItemNum[i]
  crt[crt$group == group & crt$CRTItemNum == CRTItemNum, 
      c("Grp_Decision.RESP", "Grp_Decision.OnsetTime", 
        "Grp_Decision.OffsetTime", "Grp_Decision.RT")] <- NA
}

# Accuracy
# in cases where either is different change to missing for all variables 
# (Grp_Stimulus.ACC, Grp_Confidence.RESP, Grp_Decision.RESP)
# Justification: If accuracy differs for members it is impossible to
# determine which response was agreed upon by members. Confidence and decision 
# were contingent upon accuracy, as it was provided first.

x <- crt %>% select(group, member, recruit, CRTItemNum, Grp_Stimulus.ACC) %>% 
  spread(member, Grp_Stimulus.ACC) %>%
  group_by(group, CRTItemNum) %>% 
  mutate(dif = `1` != `2`) %>%
  filter(dif == TRUE)

# prints out number of times that group accuracy differed between members
cat("number of times that group accuracy differed between members:", nrow(x))


for (i in seq(nrow(x))){
  group <- x$group[i]
  CRTItemNum <- x$CRTItemNum[i]
  crt[crt$group == group & crt$CRTItemNum == CRTItemNum, 
      c("Grp_Stimulus.ACC", "Grp_Stimulus.RESP", "Grp_Stimulus.OnsetTime", "Grp_Stimulus.OffsetTime",
        "Grp_Stimulus.RT", "Grp_Confidence.RESP", "Grp_Confidence.OnsetTime", 
        "Grp_Confidence.OffsetTime", "Grp_Confidence.RT", "Grp_Decision.RESP", 
        "Grp_Decision.OnsetTime", "Grp_Decision.OffsetTime", "Grp_Decision.RT")] <- NA
}



# Compute all variables ------------------------------------------------
# crt.uid <- crt %>%
#   group_by(uid) %>%
#   summarise(
#     group = group[1],
#     crt.ind.acc = mean(Ind_Stimulus.ACC) * 100,
#     crt.grp.acc = mean(Grp_Stimulus.ACC, na.rm = TRUE) * 100,
#     crt.ind.conf = mean(Ind_Confidence.RESP),
#     crt.grp.conf = mean(Grp_Confidence.RESP, na.rm = TRUE),
#     crt.ind.dec = mean(Ind_Decision.RESP == "y") * 100,
#     crt.grp.dec = mean(Grp_Decision.RESP == "y", na.rm = TRUE) * 100,
#     crt.ind.rec = 100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 0, na.rm = TRUE) 
#     / mean(Ind_Stimulus.ACC == 0, na.rm = TRUE),
#     crt.grp.rec = 100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 0, na.rm = TRUE) 
#     / mean(Grp_Stimulus.ACC == 0, na.rm = TRUE))
# crt.ind.comp = (100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 1, na.rm = TRUE)) + 
#  (100 * mean(Ind_Decision.RESP == "n" & Ind_Stimulus.ACC == 0, na.rm = TRUE)) / 30,
# crt.grp.comp = (100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 1, na.rm = TRUE)) + 
#  (100 * mean(Grp_Decision.RESP == "n" & Grp_Stimulus.ACC == 0, na.rm = TRUE)) / 30,
# crt.ind.optim = (100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 1, na.rm = TRUE)) 
# / 30,
# crt.grp.optim = (100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 1, na.rm = TRUE)) 
# / 30,
# crt.ind.hes = 100 * mean(Ind_Decision.RESP == "n" & Ind_Stimulus.ACC == 1, na.rm = TRUE) 
# / mean(Ind_Stimulus.ACC == 1, na.rm = TRUE),
# crt.grp.hes = 100 * mean(Grp_Decision.RESP == "n" & Grp_Stimulus.ACC == 1, na.rm = TRUE) 
# / mean(Grp_Stimulus.ACC == 1, na.rm = TRUE))



# Reliability ----------------------------------------------------
# # FOR INDIVIDUALS
# # Accuracy
# 
# x <- crt %>%
#   select(uid, CRTItemNum, Ind_Stimulus.ACC) %>%
#   mutate(Ind_Stimulus.ACC = ifelse(Ind_Stimulus.ACC == TRUE, 1, 0)) %>%
#   spread(CRTItemNum, Ind_Stimulus.ACC) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Confidence
# x <- crt %>%
#   select(uid, CRTItemNum, Ind_Confidence.RESP) %>%
#   spread(CRTItemNum, Ind_Confidence.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Decisiveness
# x <- crt %>%
#   select(uid, CRTItemNum, Ind_Decision.RESP)
# 
# x[x == "y"] <- 1
# x[x == "n"] <- 0
# 
# x <- transform(x, Ind_Decision.RESP = as.numeric(Ind_Decision.RESP))
# 
# x <- x %>%
#   spread(CRTItemNum, Ind_Decision.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Recklessness
# x <- crt %>%
#   select(uid, CRTItemNum, Ind_Stimulus.ACC, Ind_Decision.RESP) %>%
#   filter(Ind_Stimulus.ACC == 0) %>%
#   select(-Ind_Stimulus.ACC)
# 
# x[x == "y"] <- 1
# x[x == "n"] <- 0
# 
# x <- transform(x, Ind_Decision.RESP = as.numeric(Ind_Decision.RESP))
# 
# odd1 <- c(1, 3, 5, 7)
# 
# odd <- filter(x, CRTItemNum %in% odd1)
# 
# even1 <- c(2, 4, 6)
# 
# even <- filter(x, CRTItemNum %in% even1)
# 
# even <- even %>%
#   group_by(uid) %>%
#   summarise(even = sum(Ind_Decision.RESP, na.rm = TRUE))
# 
# odd <- odd %>%
#   group_by(uid) %>%
#   summarise(odd = sum(Ind_Decision.RESP, na.rm = TRUE))
# 
# x <- merge(odd, even) %>%
#   mutate(total = odd + even) %>%
#   ungroup(uid) %>%
#   select(-uid)
# 
# # write.csv(x, "RAPM_splithalf.csv")
# 
# cor.test(x$odd, x$even)
# 
# # use this website to correct using the spearman-brown prophecy formula
# # https://www.cedu.niu.edu/~walker/calculators/sbpf.asp


# # FOR TEAMS
# Accuracy
# 
# x <- crt %>%
#   select(uid, CRTItemNum, Grp_Stimulus.ACC) %>%
#   mutate(Grp_Stimulus.ACC = ifelse(Grp_Stimulus.ACC == TRUE, 1, 0)) %>%
#   spread(CRTItemNum, Grp_Stimulus.ACC) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Confidence
# x <- crt %>%
#   select(uid, CRTItemNum, Grp_Confidence.RESP) %>%
#   spread(CRTItemNum, Grp_Confidence.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Decisiveness
# x <- crt %>%
#   select(uid, CRTItemNum, Grp_Decision.RESP)
# 
# x[x == "y"] <- 1
# x[x == "n"] <- 0
# 
# x <- transform(x, Grp_Decision.RESP = as.numeric(Grp_Decision.RESP))
# 
# x <- x %>%
#   spread(CRTItemNum, Grp_Decision.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Recklessness
# x <- crt %>%
#   select(uid, CRTItemNum, Grp_Stimulus.ACC, Grp_Decision.RESP) %>%
#   filter(Grp_Stimulus.ACC == 0) %>%
#   select(-Grp_Stimulus.ACC)
# 
# x[x == "y"] <- 1
# x[x == "n"] <- 0
# 
# x <- transform(x, Grp_Decision.RESP = as.numeric(Grp_Decision.RESP))
# 
# odd1 <- c(1, 3, 5, 7)
# 
# odd <- filter(x, CRTItemNum %in% odd1)
# 
# even1 <- c(2, 4, 6)
# 
# even <- filter(x, CRTItemNum %in% even1)
# 
# even <- even %>%
#   group_by(uid) %>%
#   summarise(even = sum(Grp_Decision.RESP, na.rm = TRUE))
# 
# odd <- odd %>%
#   group_by(uid) %>%
#   summarise(odd = sum(Grp_Decision.RESP, na.rm = TRUE))
# 
# x <- merge(odd, even) %>%
#   mutate(total = odd + even) %>%
#   ungroup(uid) %>%
#   select(-uid)
# 
# # write.csv(x, "RAPM_splithalf.csv")
# 
# cor.test(x$odd, x$even)
# 
# use this website to correct using the spearman-brown prophecy formula
# https://www.cedu.niu.edu/~walker/calculators/sbpf.asp



# Composite_Emotions_Task -----------------------------------------------
surveys_qrt$Composite_Emotions_Task_A1 <- as.data.frame(surveys_qrt$Composite_Emotions_Task_A1 %>%
      select(ResponseID, StartDate:Finished, uid, CetItemNum, Stimulus.CRESP, 
             Stimulus.RESP, Stimulus.RT, Stimulus.OnsetTime, Stimulus.OffsetTime,
             Confidence.RESP, Confidence.OnsetTime, Confidence.OffsetTime,
             Decision.RESP, Decision.RT, Decision.OnsetTime, Decision.OffsetTime) %>%
      mutate(version = "a1") %>%
      group_by(uid, CetItemNum) %>%
      mutate(Confidence.OffsetTime = as.numeric(Confidence.OffsetTime),
             Confidence.OnsetTime = as.numeric(Confidence.OnsetTime),
             Confidence.OffsetTime = as.numeric(Confidence.OffsetTime),
             Confidence.OnsetTime = as.numeric(Confidence.OnsetTime),
             Confidence.RT = Confidence.OffsetTime - Confidence.OnsetTime,
             Confidence.RT = Confidence.OffsetTime - Confidence.OnsetTime))

surveys_qrt$Composite_Emotions_Task_B1 <- as.data.frame(surveys_qrt$Composite_Emotions_Task_B1 %>%
      select(ResponseID, StartDate:Finished, uid, CetItemNum, Stimulus.CRESP, 
             Stimulus.RESP, Stimulus.RT, Stimulus.OnsetTime, Stimulus.OffsetTime,
             Confidence.RESP, Confidence.OnsetTime, Confidence.OffsetTime,
             Decision.RESP, Decision.RT, Decision.OnsetTime, Decision.OffsetTime) %>%
      mutate(version = "b1") %>%
      group_by(uid, CetItemNum) %>%
      mutate(Confidence.OffsetTime = as.numeric(Confidence.OffsetTime),
             Confidence.OnsetTime = as.numeric(Confidence.OnsetTime),
             Confidence.OffsetTime = as.numeric(Confidence.OffsetTime),
             Confidence.OnsetTime = as.numeric(Confidence.OnsetTime),
             Confidence.RT = Confidence.OffsetTime - Confidence.OnsetTime,
             Confidence.RT = Confidence.OffsetTime - Confidence.OnsetTime))

surveys_java$Composite_Emotions_Task_A2 <- surveys_java$Composite_Emotions_Task_A2 %>%
  rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, CetItemNum = itemnum, 
         Stimulus.OnsetTime = stimulus_onset,
         Stimulus.OffsetTime = stimulus_offset, Stimulus.RT = stimulus_rt,
         Stimulus.RESP = response, Confidence.OnsetTime = confidence_onset,
         Confidence.OffsetTime = confidence_offset, Confidence.RT = confidence_rt,
         Confidence.RESP = confidence, Decision.OnsetTime = decision_onset,
         Decision.OffsetTime = decision_offset, Decision.RT = decision_rt,
         Decision.RESP = decision) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, CetItemNum, Stimulus.CRESP, 
         Stimulus.RESP, Stimulus.RT, Stimulus.OnsetTime, Stimulus.OffsetTime,
         Confidence.RESP, Confidence.RT, Confidence.OnsetTime, Confidence.OffsetTime,
         Decision.RESP, Decision.RT, Decision.OnsetTime, Decision.OffsetTime) %>%
  mutate(version = "a2")

surveys_java$Composite_Emotions_Task_B2 <- surveys_java$Composite_Emotions_Task_B2 %>%
  rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, CetItemNum = itemnum, 
         Stimulus.OnsetTime = stimulus_onset,
         Stimulus.OffsetTime = stimulus_offset, Stimulus.RT = stimulus_rt,
         Stimulus.RESP = response, Confidence.OnsetTime = confidence_onset,
         Confidence.OffsetTime = confidence_offset, Confidence.RT = confidence_rt,
         Confidence.RESP = confidence, Decision.OnsetTime = decision_onset,
         Decision.OffsetTime = decision_offset, Decision.RT = decision_rt,
         Decision.RESP = decision) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, CetItemNum, Stimulus.CRESP, 
         Stimulus.RESP, Stimulus.RT, Stimulus.OnsetTime, Stimulus.OffsetTime,
         Confidence.RESP, Confidence.RT, Confidence.OnsetTime, Confidence.OffsetTime,
         Decision.RESP, Decision.RT, Decision.OnsetTime, Decision.OffsetTime) %>%
  mutate(version = "b2")

# combine data from a1 and b1 into a single df 
# and rename first 10 columns labelled V1 - V10
cet <- rbind(surveys_qrt$Composite_Emotions_Task_A1, surveys_qrt$Composite_Emotions_Task_B1,
             surveys_java$Composite_Emotions_Task_A2, surveys_java$Composite_Emotions_Task_B2)

# Data Screening ----------------------------------------------------------

# Check participants appear correct number of times =======================
# ISSUE 1: Identified that the following uids appear twice
#   "18041210_1_d2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_1_g2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_2_d1"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_2_g1"   = all responses legit, P from 1pm session incorrectly assigned same uid
#
# SOLUTION 1: change uids for these Ps to reflect the correct start time
# new uid for ResponseID = R_4V1TDceif1NYMp7 becomes 18041213_1_d2
# new uid for ResponseID = R_ePx4GBGrM8iQzWd becomes 18041213_1_g2
# new uid for ResponseID = R_bIcjEckek10vtu5 becomes 18041213_1_d1
# new uid for ResponseID = R_1O06uBoWXTTiuPj becomes 18041213_1_g1

# Modify uids and remove duplicate attempts
cet <- cet %>% mutate(uid = ifelse(ResponseID == "R_4V1TDceif1NYMp7", "18041213_1_d2", uid),
                      uid = ifelse(ResponseID == "R_ePx4GBGrM8iQzWd", "18041213_1_g2", uid),
                      uid = ifelse(ResponseID == "R_bIcjEckek10vtu5", "18041213_1_d1", uid),
                      uid = ifelse(ResponseID == "R_1O06uBoWXTTiuPj", "18041213_1_g1", uid))  %>% 
  filter(!is.na(CetItemNum))


n_questions <- 36 # number of questions in this test

x <- table(cet$uid)

if (any(x != n_questions)) {
  print("Participants not appearing the correct number of times:")
  x[x != n_questions]
} else {
  print("All good! :)")
}

# Check for missing values ================================================
# create vector to select only uid and participants responses
x <- cet %>%
  select(uid, CetItemNum, Stimulus.RESP, Confidence.RESP, Decision.RESP)

# by column
col_miss <- sapply(x, function(col) {
  col %>%
    is.na() %>%
    sum()
})

# by row
row_miss <- apply(x, 1, function(row) {
  row %>%
    is.na() %>%
    any()
})

check <- if (any(row_miss)) {
  print("missing values for participant:")
  x[row_miss, ] # to print uid if there is a missing value
}  else {
  print("no missing values")
}

# visually check that there are no missing values per user per variable
x %>%
  group_by(uid) %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  gather(key, val, -uid) %>% 
  ggplot(aes(key, uid, fill = factor(val))) +
  geom_tile() +
  scale_fill_brewer(palette = "OrRd") +
  labs(title = "Number of missing values per user, per variable", x = NULL, y = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Check whether vector contains acceptable values ------------------------
# identified that a number of confidence responses contained spaces before the Ps answer
cet <- cet %>% mutate(Stimulus.RESP = as.numeric(Stimulus.RESP),
                      Confidence.RESP = as.numeric(Confidence.RESP),
                      Decision.RESP = tolower(Decision.RESP))

check_values <- function(x, num_min, num_max = NULL) {
  if (is.null(num_max)) {
    any_wrong <- !all(x %in% num_min)
  } else {
    any_wrong <- any(x < num_min | x > num_max, na.rm = TRUE)
  }
  
  if (any_wrong) {
    print("Unacceptable value present in this variable.")
  } else {
    print("All good! :)")
  }
}

check_values(cet$Stimulus.RESP, 0, 6)
check_values(cet$Confidence.RESP, 13, 100)
check_values(cet$Decision.RESP, c("y", "n"))


# calculate variables
# need to score CET using scoring method from Mattis Geiger.
# first create emotion categories
cet <- cet %>%
  mutate(CetItemNum = as.numeric(CetItemNum))

cet <- cet %>%
  mutate(Stimulus.ACC = Stimulus.RESP == Stimulus.CRESP,
    target_emotion = ifelse(CetItemNum %in% c(10, 13, 16, 28, 31, 34), "happy",
                          ifelse(CetItemNum %in% c(11, 14, 17, 29, 32, 35), "disgust",
                                 ifelse(CetItemNum %in% c(12, 15, 18, 30, 33, 36), "surprise",
                                        ifelse(CetItemNum %in% c(1, 2, 3, 19, 20, 21), "anger",
                                               ifelse(CetItemNum %in% c(4, 5, 6, 22, 23, 24), "fear",
                                                      ifelse(CetItemNum %in% c(7, 8, 9, 25, 26, 27), "sad", "unknown")))))),
    emotion.RESP = ifelse(Stimulus.RESP == 1, "happy",
                          ifelse(Stimulus.RESP == 2, "surprise",
                                 ifelse(Stimulus.RESP == 3, "anger",
                                        ifelse(Stimulus.RESP == 4, "fear",
                                               ifelse(Stimulus.RESP == 5, "sad",
                                                      ifelse(Stimulus.RESP == 6, "disgust", "unknown")))))))


x1 <- cet %>%
  group_by(uid, target_emotion) %>%
  summarise(freq_correct = sum(Stimulus.ACC == TRUE)^2,
            mean_correct = mean(Stimulus.ACC == TRUE))

x2 <- cet %>% 
  group_by(uid, emotion.RESP) %>%
  summarise(freq_resp = n()) %>%
  rename(target_emotion = emotion.RESP)

x <- x1 %>% left_join(x2)

cet.uid <- x %>%
  group_by(uid, target_emotion) %>%
  summarise(mean_correct = mean_correct,
    unbiased_hit_rate = freq_correct / (6 * freq_resp))

# check with mattis, is it ok to calculate overall accuracy score and unbiased hit rate?
cet.uid %>%
  group_by(uid) %>%
  summarise(mean_correct = mean(mean_correct),
            mean_ubhr = mean(unbiased_hit_rate, na.rm = T))


# Reliability ----------------------------------------------------
# Alpha for each emotion
# x <- cet %>%
#   filter(target_emotion == "surprise") %>% # calculate for each factor
#   select(uid, CetItemNum, Stimulus.RESP) %>%
#   spread(CetItemNum, Stimulus.RESP) %>%
#   select(-uid)
# 
# psych::alpha(as.data.frame(x))
# 
# # Alpha overall
# x <- cet %>%
#   select(uid, CetItemNum, Stimulus.RESP) %>%
#   spread(CetItemNum, Stimulus.RESP) %>%
#   select(-uid)
# 
# psych::alpha(as.data.frame(x))


# General_knowledge Test -----------------------------------------------
surveys_qrt$General_knowledge_A1 <- as.data.frame(surveys_qrt$General_knowledge_A1 %>%
  select(ResponseID, StartDate:Finished, uid, Itemnum, Question, Representativeness, Ind_Stimulus.CRESP, 
         Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, 
         Ind_Stimulus.OffsetTime, Ind_Confidence.RESP, Ind_Confidence.OnsetTime, 
         Ind_Confidence.OffsetTime, Ind_Decision.RESP, Ind_Decision.RT, 
         Ind_Decision.OnsetTime, Ind_Decision.OffsetTime, 
         Grp_Stimulus.RESP, Grp_Stimulus.RT, Grp_Stimulus.OnsetTime, 
         Grp_Stimulus.OffsetTime, Grp_Confidence.RESP, Grp_Confidence.OnsetTime, 
         Grp_Confidence.OffsetTime, Grp_Decision.RESP, Grp_Decision.RT, 
         Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
  rename(Stimulus.CRESP = Ind_Stimulus.CRESP) %>%
  mutate(version = "a1") %>%
  group_by(uid, Itemnum) %>%
  mutate(Ind_Confidence.OffsetTime = as.numeric(Ind_Confidence.OffsetTime),
         Ind_Confidence.OnsetTime = as.numeric(Ind_Confidence.OnsetTime),
         Grp_Confidence.OffsetTime = as.numeric(Grp_Confidence.OffsetTime),
         Grp_Confidence.OnsetTime = as.numeric(Grp_Confidence.OnsetTime),
         Ind_Confidence.RT = Ind_Confidence.OffsetTime - Ind_Confidence.OnsetTime,
         Grp_Confidence.RT = Grp_Confidence.OffsetTime - Grp_Confidence.OnsetTime))

surveys_qrt$General_knowledge_B1 <- as.data.frame(surveys_qrt$General_knowledge_B1 %>%
  select(ResponseID, StartDate:Finished, uid, Itemnum, Question, Representativeness, Ind_Stimulus.CRESP, 
         Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, 
         Ind_Stimulus.OffsetTime, Ind_Confidence.RESP, Ind_Confidence.OnsetTime, 
         Ind_Confidence.OffsetTime, Ind_Decision.RESP, Ind_Decision.RT, 
         Ind_Decision.OnsetTime, Ind_Decision.OffsetTime, 
         Grp_Stimulus.RESP, Grp_Stimulus.RT, Grp_Stimulus.OnsetTime, 
         Grp_Stimulus.OffsetTime, Grp_Confidence.RESP, Grp_Confidence.OnsetTime, 
         Grp_Confidence.OffsetTime, Grp_Decision.RESP, Grp_Decision.RT, 
         Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
  rename(Stimulus.CRESP = Ind_Stimulus.CRESP) %>%
  mutate(version = "b1") %>%
  group_by(uid, Itemnum) %>%
  mutate(Ind_Confidence.OffsetTime = as.numeric(Ind_Confidence.OffsetTime),
         Ind_Confidence.OnsetTime = as.numeric(Ind_Confidence.OnsetTime),
         Grp_Confidence.OffsetTime = as.numeric(Grp_Confidence.OffsetTime),
         Grp_Confidence.OnsetTime = as.numeric(Grp_Confidence.OnsetTime),
         Ind_Confidence.RT = Ind_Confidence.OffsetTime - Ind_Confidence.OnsetTime,
         Grp_Confidence.RT = Grp_Confidence.OffsetTime - Grp_Confidence.OnsetTime))

surveys_java$General_knowledge_A2 <- surveys_java$General_knowledge_A2 %>%
  rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, Itemnum = itemnum, 
         Ind_Stimulus.OnsetTime = ind_stimulus_onset,
         Ind_Stimulus.OffsetTime = ind_stimulus_offset, Ind_Stimulus.RT = ind_stimulus_rt,
         Ind_Stimulus.RESP = ind_response, Ind_Confidence.OnsetTime = ind_confidence_onset,
         Ind_Confidence.OffsetTime = ind_confidence_offset, Ind_Confidence.RT = ind_confidence_rt,
         Ind_Confidence.RESP = ind_confidence, Ind_Decision.OnsetTime = ind_decision_onset,
         Ind_Decision.OffsetTime = ind_decision_offset, Ind_Decision.RT = ind_decision_rt,
         Ind_Decision.RESP = ind_decision, Grp_Stimulus.OnsetTime = grp_stimulus_onset,
         Grp_Stimulus.OffsetTime = grp_stimulus_offset, Grp_Stimulus.RT = grp_stimulus_rt,
         Grp_Stimulus.RESP = grp_response, Grp_Confidence.OnsetTime = grp_confidence_onset,
         Grp_Confidence.OffsetTime = grp_confidence_offset, Grp_Confidence.RT = grp_confidence_rt,
         Grp_Confidence.RESP = grp_confidence, Grp_Decision.OnsetTime = grp_decision_onset,
         Grp_Decision.OffsetTime = grp_decision_offset, Grp_Decision.RT = grp_decision_rt,
         Grp_Decision.RESP = grp_decision, Question = question, Representativeness = representativeness) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, Itemnum, Question, Representativeness, Stimulus.CRESP, 
         Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime,
         Ind_Confidence.RESP, Ind_Confidence.RT, Ind_Confidence.OnsetTime, Ind_Confidence.OffsetTime,
         Ind_Decision.RESP, Ind_Decision.RT, Ind_Decision.OnsetTime, Ind_Decision.OffsetTime, 
         Grp_Stimulus.RESP, Grp_Stimulus.RT, Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime,
         Grp_Confidence.RESP, Grp_Confidence.RT, Grp_Confidence.OnsetTime, Grp_Confidence.OffsetTime,
         Grp_Decision.RESP, Grp_Decision.RT, Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
  mutate(version = "a2")

surveys_java$General_knowledge_B2 <- surveys_java$General_knowledge_B2 %>%
  rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, Itemnum = itemnum, 
         Ind_Stimulus.OnsetTime = ind_stimulus_onset,
         Ind_Stimulus.OffsetTime = ind_stimulus_offset, Ind_Stimulus.RT = ind_stimulus_rt,
         Ind_Stimulus.RESP = ind_response, Ind_Confidence.OnsetTime = ind_confidence_onset,
         Ind_Confidence.OffsetTime = ind_confidence_offset, Ind_Confidence.RT = ind_confidence_rt,
         Ind_Confidence.RESP = ind_confidence, Ind_Decision.OnsetTime = ind_decision_onset,
         Ind_Decision.OffsetTime = ind_decision_offset, Ind_Decision.RT = ind_decision_rt,
         Ind_Decision.RESP = ind_decision, Grp_Stimulus.OnsetTime = grp_stimulus_onset,
         Grp_Stimulus.OffsetTime = grp_stimulus_offset, Grp_Stimulus.RT = grp_stimulus_rt,
         Grp_Stimulus.RESP = grp_response, Grp_Confidence.OnsetTime = grp_confidence_onset,
         Grp_Confidence.OffsetTime = grp_confidence_offset, Grp_Confidence.RT = grp_confidence_rt,
         Grp_Confidence.RESP = grp_confidence, Grp_Decision.OnsetTime = grp_decision_onset,
         Grp_Decision.OffsetTime = grp_decision_offset, Grp_Decision.RT = grp_decision_rt,
         Grp_Decision.RESP = grp_decision, Question = question, Representativeness = representativeness) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, Itemnum, Question, Representativeness, Stimulus.CRESP, 
         Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime,
         Ind_Confidence.RESP, Ind_Confidence.RT, Ind_Confidence.OnsetTime, Ind_Confidence.OffsetTime,
         Ind_Decision.RESP, Ind_Decision.RT, Ind_Decision.OnsetTime, Ind_Decision.OffsetTime, 
         Grp_Stimulus.RESP, Grp_Stimulus.RT, Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime,
         Grp_Confidence.RESP, Grp_Confidence.RT, Grp_Confidence.OnsetTime, Grp_Confidence.OffsetTime,
         Grp_Decision.RESP, Grp_Decision.RT, Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
  mutate(version = "b2")

# combine data from a1 and b1 into a single df 
# and rename first 10 columns labelled V1 - V10
gk <- rbind(surveys_qrt$General_knowledge_A1, surveys_qrt$General_knowledge_B1,
            surveys_java$General_knowledge_A2, surveys_java$General_knowledge_B2)

# Data Screening ----------------------------------------------------------

# Check participants appear correct number of times =======================
# ISSUE 1: Identified that the following uids appear twice
#   "18041210_1_d2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_1_g2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#
# SOLUTION 1: change uids for these Ps to reflect the correct start time
# new uid for ResponseID = R_3n109pSpds4kmLb becomes 18041213_1_d2
# new uid for ResponseID = R_8qMLzAM3dx42ab3 becomes 18041213_1_g2
#
# ISSUE 2: Identified that uid == "18040910_2_g1" & "17102710_2_d1" appears twice each
# The browser crashed so I had to restart the survey. 
#
# SOLUTION 2. 
# For uid == "18040910_2_g1" take responses for Q1-6 from 1st attempt 
# ResponseID == "R_07nbXqOSwWXHx8p" and join with responses for Q12-20 
# from 2nd attemp ResponseID == "R_cuwZ6DNRTH1c5Bb" then remove first attempt

x <- gk %>%
  filter(uid == "18041210_1_g2") %>%
  select(ResponseID, uid, StartDate:Finished, Itemnum, Ind_Stimulus.RESP, Grp_Stimulus.RESP,
         Ind_Confidence.RESP, Grp_Confidence.RESP, Ind_Decision.RESP, Grp_Decision.RESP)

# Which variables will I replace?
vars <- c("Ind_Stimulus.RESP", "Ind_Stimulus.RT",
          "Ind_Stimulus.OnsetTime", "Ind_Stimulus.OffsetTime", "Ind_Confidence.RESP",
          "Ind_Confidence.OnsetTime", "Ind_Confidence.OffsetTime", "Ind_Decision.RESP",
          "Ind_Decision.RT", "Ind_Decision.OffsetTime", "Ind_Decision.OnsetTime",
          "Grp_Stimulus.RESP", "Grp_Stimulus.RT", "Grp_Stimulus.OnsetTime",
          "Grp_Stimulus.OffsetTime", "Grp_Confidence.RESP", "Grp_Confidence.OnsetTime",
          "Grp_Confidence.OffsetTime", "Grp_Decision.RESP", "Grp_Decision.RT",
          "Grp_Decision.OffsetTime", "Grp_Decision.OnsetTime")

# For uid == "18040910_2_g1" get Attempt 1 responses
x <- gk %>% filter(ResponseID == "R_07nbXqOSwWXHx8p")

# x <- arrange(x, Itemnum)  # arrange by trial number

x <- x[1:6, vars]  # Take rows 1 to 6 (corresponding to answered questions)
# and variables of interest

itemnum <- c("2", "31", "68", "87", "118", "15")

# Insert data from x into relevant part of df
gk[gk$ResponseID == "R_cuwZ6DNRTH1c5Bb" & gk$Itemnum %in% itemnum, vars] <- x

# For uid == "17102710_2_d1" get Attempt 1 responses
x <- gk %>% filter(ResponseID == "R_bIrElN26F0tB2Yt")

x <- x[1:11, vars]  # Take rows 1 to 11 (corresponding to answered questions)
# and variables of interest

itemnum <- c("2", "31", "68", "87", "118", "15", "25", "70", "97", "120", "19")

# Insert data from x into relevant part of df
gk[gk$ResponseID == "R_bEEKi8OqieuQKAB" & gk$Itemnum %in% itemnum, vars] <- x

# Modify uids and remove duplicate attempts
gk <- gk %>% mutate(uid = ifelse(ResponseID == "R_3n109pSpds4kmLb", "18041213_1_d2", uid),
                    uid = ifelse(ResponseID == "R_8qMLzAM3dx42ab3", "18041213_1_g2", uid)) %>%
  filter(ResponseID != "R_07nbXqOSwWXHx8p" & ResponseID != "R_bIrElN26F0tB2Yt")

n_questions <- 20 # number of questions in this test

x <- table(gk$uid)

if (any(x != n_questions)) {
  print("Participants not appearing the correct number of times:")
  x[x != n_questions]
} else {
  print("All good! :)")
}

# Check for missing values ================================================
# create vector to select only uid and participants responses
x <- gk %>%
  select(uid, Itemnum, Ind_Stimulus.RESP, Grp_Stimulus.RESP, Ind_Confidence.RESP, 
         Grp_Confidence.RESP, Ind_Decision.RESP, Grp_Decision.RESP)

# by column
col_miss <- sapply(x, function(col) {
  col %>%
    is.na() %>%
    sum()
})

# by row
row_miss <- apply(x, 1, function(row) {
  row %>%
    is.na() %>%
    any()
})

check <- if (any(row_miss)) {
  print("missing values for participant:")
  x[row_miss, ] # to print uid if there is a missing value
}  else {
  print("no missing values")
}

# visually check that there are no missing values per user per variable
x %>%
  group_by(uid) %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  gather(key, val, -uid) %>% 
  ggplot(aes(key, uid, fill = factor(val))) +
  geom_tile() +
  scale_fill_brewer(palette = "OrRd") +
  labs(title = "Number of missing values per user, per variable", x = NULL, y = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Check whether vector contains acceptable values () ------------------------
gk <- gk %>% mutate(Ind_Stimulus.RESP = as.numeric(Ind_Stimulus.RESP),
                    Grp_Stimulus.RESP = as.numeric(Grp_Stimulus.RESP),
                    Ind_Confidence.RESP = as.numeric(Ind_Confidence.RESP),
                    Grp_Confidence.RESP = as.numeric(Grp_Confidence.RESP),
                    Ind_Decision.RESP = tolower(Ind_Decision.RESP),
                    Grp_Decision.RESP = tolower(Grp_Decision.RESP))

check_values <- function(x, num_min, num_max = NULL) {
  if (is.null(num_max)) {
    any_wrong <- !all(x %in% num_min)
  } else {
    any_wrong <- any(x < num_min | x > num_max, na.rm = TRUE)
  }
  
  if (any_wrong) {
    print("Unacceptable value present in this variable.")
  } else {
    print("All good! :)")
  }
}

check_values(gk$Ind_Stimulus.RESP, 1, 2)
check_values(gk$Ind_Confidence.RESP, 50, 100)
check_values(gk$Ind_Decision.RESP, c("y", "n"))

check_values(gk$Grp_Stimulus.RESP, 1, 2)
check_values(gk$Grp_Confidence.RESP, 50, 100)
check_values(gk$Grp_Decision.RESP, c("y", "n"))

# Itemnum was recorded differently for versions a1 & b1 compared to a2 & b2
# SOLUTION: make itemnums for all versions range from 1-20
gk <- gk %>%
  group_by(uid) %>%
  mutate(Itemnum = 1:n()) %>%
  ungroup()

# # To check that Itemnum was correctly modified compare data for versions a1 & b1 to a2 & b2
# gk <- gk %>%
#   mutate(Question = ifelse(str_detect(Question, "Which state/territory has a larger area?"), "Which state has a larger area?",
#                            ifelse(str_detect(Question, "Which state/territory has a larger length of coastline?"), "Which state has a larger length of coastline?", 
#                                   ifelse(str_detect(Question, "Which state/territory has a larger area?"), "Which state has a larger area?", 
#                                          ifelse(str_detect(Question, "Which state/territory has a larger population?"), "Which state has a larger population?", Question)))))
# 
# x1 <- unique(gk %>% filter(version == "a1" | version == "b1") %>% select(Itemnum, Question, Representativeness, Stimulus.CRESP))
# x2 <- unique(gk %>%  filter(version == "a2" | version == "b2") %>% select(Itemnum, Question, Representativeness, Stimulus.CRESP))
# 
# identical(x1, x2)

# add Stimulus.ACC column (Stimulus.RESP == Stimulus.CRESP)
gk <- gk %>% 
  group_by(uid, Itemnum) %>%
  mutate(Ind_Stimulus.ACC = Ind_Stimulus.RESP == Stimulus.CRESP,
         Grp_Stimulus.ACC = Grp_Stimulus.RESP == Stimulus.CRESP) %>%
  ungroup()

# Check teams are giving same response and decision =======================
# Create grouping variables -----------------------------------------------
# group == date + group
# member == g == 1, d == 2
gk <- gk %>% mutate(member = ifelse(grepl("g[1-2]", uid), "1", 
                                    ifelse(grepl("d[1-2]", uid), "2", 
                                           ifelse(grepl("e1", uid), "1", "2"))),
                    recruit = ifelse(grepl("[dg]", uid), "s", "e"),
                    group = gsub("_[a-z][12]", "", uid))

# discovered that uid == "17100609_2_g1" does not have a teammate
# Not sure where their data went? All participants were in teams
# remove for now and investigate later
gk <- gk %>% filter(uid != "17100609_2_g1")

# Decisions
# in cases where member's group decisions differ change to missing
# Justification: If decision is different for members it is impossible to
# determine which decision was agreed upon by members. As decision was provided last
# other data for same item remains valid, if matching.

x <- gk %>% select(group, member, Itemnum, Grp_Decision.RESP) %>% 
  spread(member, Grp_Decision.RESP) %>%
  group_by(group, Itemnum) %>%
  mutate(dif = `1` != `2`) %>%
  filter(dif == TRUE)

# prints out number of times that group decisions differed between members
cat("number of times that group decision differed between members:", nrow(x))

for (i in seq(nrow(x))){
  group <- x$group[i]
  Itemnum <- x$Itemnum[i]
  gk[gk$group == group & gk$Itemnum == Itemnum, 
     c("Grp_Decision.RESP","Grp_Decision.OnsetTime", "Grp_Decision.OffsetTime",
       "Grp_Decision.RT")] <- NA
}

# Accuracy
# in cases where either is different change to missing for all variables 
# (Grp_Stimulus.ACC, Grp_Confidence.RESP, Grp_Decision.RESP)
# Justification: If accuracy differs for members it is impossible to
# determine which response was agreed upon by members. Confidence and decision 
# were contingent upon accuracy, as it was provided first.

x <- gk %>% select(group, member, Itemnum, Grp_Stimulus.RESP) %>% 
  spread(member, Grp_Stimulus.RESP) %>%
  group_by(group, Itemnum) %>% 
  mutate(dif = `1` != `2`) %>%
  filter(dif == TRUE)

# prints out number of times that group accuracy differed between members
cat("number of times that group accuracy differed between members:", nrow(x))

for (i in seq(nrow(x))){
  group <- x$group[i]
  Itemnum <- x$Itemnum[i]
  gk[gk$group == group & gk$Itemnum == Itemnum, 
     c("Grp_Stimulus.RESP", "Grp_Stimulus.OnsetTime", "Grp_Stimulus.OffsetTime",
       "Grp_Stimulus.RT", "Grp_Confidence.RESP", "Grp_Confidence.OnsetTime", 
       "Grp_Confidence.OffsetTime", "Grp_Confidence.RT", "Grp_Decision.RESP", 
       "Grp_Decision.OnsetTime", "Grp_Decision.OffsetTime", "Grp_Decision.RT")] <- NA
}
      


# Compute all variables ------------------------------------------------
gk.uid <- gk %>%
  group_by(uid) %>%
  summarise(
    group = group[1],
    gk.ind.acc = mean(Ind_Stimulus.ACC) * 100,
    gk.grp.acc = mean(Grp_Stimulus.ACC, na.rm = TRUE) * 100,
    gk.ind.conf = mean(Ind_Confidence.RESP),
    gk.grp.conf = mean(Grp_Confidence.RESP, na.rm = TRUE),
    gk.ind.dec = mean(Ind_Decision.RESP == "y") * 100,
    gk.grp.dec = mean(Grp_Decision.RESP == "y", na.rm = TRUE) * 100,
    gk.ind.rec = 100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 0, na.rm = TRUE) 
    / mean(Ind_Stimulus.ACC == 0, na.rm = TRUE),
    gk.grp.rec = 100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 0, na.rm = TRUE) 
    / mean(Grp_Stimulus.ACC == 0, na.rm = TRUE))
# gk.ind.comp = (100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 1, na.rm = TRUE)) + 
#  (100 * mean(Ind_Decision.RESP == "n" & Ind_Stimulus.ACC == 0, na.rm = TRUE)) / 30,
# gk.grp.comp = (100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 1, na.rm = TRUE)) + 
#  (100 * mean(Grp_Decision.RESP == "n" & Grp_Stimulus.ACC == 0, na.rm = TRUE)) / 30,
# gk.ind.optim = (100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 1, na.rm = TRUE)) 
# / 30,
# gk.grp.optim = (100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 1, na.rm = TRUE)) 
# / 30,
# gk.ind.hes = 100 * mean(Ind_Decision.RESP == "n" & Ind_Stimulus.ACC == 1, na.rm = TRUE) 
# / mean(Ind_Stimulus.ACC == 1, na.rm = TRUE),
# gk.grp.hes = 100 * mean(Grp_Decision.RESP == "n" & Grp_Stimulus.ACC == 1, na.rm = TRUE) 
# / mean(Grp_Stimulus.ACC == 1, na.rm = TRUE))

# Reliability ----------------------------------------------------
# # Accuracy
# # Individuals
# x <- gk %>%
#   select(uid, Itemnum, Ind_Stimulus.ACC) %>%
#   mutate(Ind_Stimulus.ACC = ifelse(Ind_Stimulus.ACC == TRUE, 1, 0)) %>%
#   spread(Itemnum, Ind_Stimulus.ACC) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# x <- gk %>%
#   filter(Representativeness == "UR") %>%
#   select(uid, Itemnum, Ind_Stimulus.ACC) %>%
#   mutate(Ind_Stimulus.ACC = ifelse(Ind_Stimulus.ACC == TRUE, 1, 0)) %>%
#   spread(Itemnum, Ind_Stimulus.ACC) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Teams
# x <- gk %>%
#   select(uid, Itemnum, Grp_Stimulus.ACC) %>%
#   mutate(Grp_Stimulus.ACC = ifelse(Grp_Stimulus.ACC == TRUE, 1, 0)) %>%
#   spread(Itemnum, Grp_Stimulus.ACC) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# x <- gk %>%
#   filter(Representativeness == "UR") %>%
#   select(uid, Itemnum, Grp_Stimulus.ACC) %>%
#   mutate(Grp_Stimulus.ACC = ifelse(Grp_Stimulus.ACC == TRUE, 1, 0)) %>%
#   spread(Itemnum, Grp_Stimulus.ACC) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
#
#
# # Confidence
# # Individuals
# x <- gk %>%
#   select(uid, Itemnum, Ind_Confidence.RESP) %>%
#   spread(Itemnum, Ind_Confidence.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# 
# x <- gk %>%
#   filter(Representativeness == "R") %>%
#   select(uid, Itemnum, Ind_Confidence.RESP) %>%
#   spread(Itemnum, Ind_Confidence.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Teams
# x <- gk %>%
#   select(uid, Itemnum, Grp_Confidence.RESP) %>%
#   spread(Itemnum, Grp_Confidence.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# 
# x <- gk %>%
#   filter(Representativeness == "R") %>%
#   select(uid, Itemnum, Grp_Confidence.RESP) %>%
#   spread(Itemnum, Grp_Confidence.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
#
# # Individual Decisiveness
# x <- gk %>%
#   select(uid, Itemnum, Ind_Decision.RESP)
# 
# x[x == "y"] <- 1
# x[x == "n"] <- 0
# 
# x <- transform(x, Ind_Decision.RESP = as.numeric(Ind_Decision.RESP))
# 
# x <- x %>%
#   spread(Itemnum, Ind_Decision.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# 
# x <- gk %>%
#   select(uid, Itemnum, Representativeness, Ind_Decision.RESP) %>%
#   filter(Representativeness == "UR")
# 
# x[x == "y"] <- 1
# x[x == "n"] <- 0
# 
# x <- transform(x, Ind_Decision.RESP = as.numeric(Ind_Decision.RESP))
# 
# x <- x %>%
#   spread(Itemnum, Ind_Decision.RESP) %>%
#   select(-uid, -Representativeness)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Team Decisiveness
# x <- gk %>%
#   select(uid, Itemnum, Grp_Decision.RESP)
# 
# x[x == "y"] <- 1
# x[x == "n"] <- 0
# 
# x <- transform(x, Grp_Decision.RESP = as.numeric(Grp_Decision.RESP))
# 
# x <- x %>%
#   spread(Itemnum, Grp_Decision.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# 
# x <- gk %>%
#   select(uid, Itemnum, Representativeness, Grp_Decision.RESP) %>%
#   filter(Representativeness == "R")
# 
# x[x == "y"] <- 1
# x[x == "n"] <- 0
# 
# x <- transform(x, Grp_Decision.RESP = as.numeric(Grp_Decision.RESP))
# 
# x <- x %>%
#   spread(Itemnum, Grp_Decision.RESP) %>%
#   select(-uid, -Representativeness)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # recklessness for indivdiuals overall
# x <- gk %>%
#   select(uid, Itemnum, Ind_Stimulus.ACC, Ind_Decision.RESP) %>%
#   filter(Ind_Stimulus.ACC == 0) %>%
#   select(-Ind_Stimulus.ACC)
# 
# x[x == "y"] <- 1
# x[x == "n"] <- 0
# 
# x <- transform(x, Ind_Decision.RESP = as.numeric(Ind_Decision.RESP))
# 
# odd1 <- c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)
# 
# odd <- filter(x, Itemnum %in% odd1) %>%
#   mutate(class = "odd")
# 
# even1 <- c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)
# 
# even <- filter(x, Itemnum %in% even1) %>%
#   mutate(class = "even")
# 
# even <- even %>%
#   group_by(uid) %>%
#   summarise(even = sum(Ind_Decision.RESP))
# 
# odd <- odd %>%
#   group_by(uid) %>%
#   summarise(odd = sum(Ind_Decision.RESP))
# 
# 
# x <- merge(odd, even) %>%
#   mutate(total = odd + even) %>%
#   ungroup(uid) %>%
#   select(-uid)
# 
# cor.test(x$odd, x$even)
# 
# sd(x$odd)
# 
# use this website to correct using the spearman-brown prophecy formula
# https://www.cedu.niu.edu/~walker/calculators/sbpf.asp
# 
# # recklessness for individuals for CC questions
# x <- gk %>%
#   select(uid, Itemnum, Representativeness, Ind_Stimulus.ACC, Ind_Decision.RESP) %>%
#   filter(Representativeness == "R" & Ind_Stimulus.ACC == 0) %>%
#   select(-Ind_Stimulus.ACC, -Representativeness)
# 
# R.item <- gk %>%
#   filter(Representativeness == "R") %>%
#   select(Itemnum)
# 
# table(R.item)
# 
# x[x == "y"] <- 1
# x[x == "n"] <- 0
# 
# x <- transform(x, Ind_Decision.RESP = as.numeric(Ind_Decision.RESP))
# 
# odd1 <- c(1, 5, 10, 13, 17)
# 
# odd <- filter(x, Itemnum %in% odd1) %>%
#   mutate(class = "odd")
# 
# even1 <- c(4, 7, 11, 16, 19)
# 
# even <- filter(x, Itemnum %in% even1) %>%
#   mutate(class = "even")
# 
# even <- even %>%
#   group_by(uid) %>%
#   summarise(even = sum(Ind_Decision.RESP))
# 
# odd <- odd %>%
#   group_by(uid) %>%
#   summarise(odd = sum(Ind_Decision.RESP))
# 
# x <- merge(odd, even) %>%
#   mutate(total = odd + even) %>%
#   ungroup(uid) %>%
#   select(-uid)
# 
# cor.test(x$odd, x$even)
# 
# sd(x$odd)
# 
# use this website to correct using the spearman-brown prophecy formula
# https://www.cedu.niu.edu/~walker/calculators/sbpf.asp
# 
# 
# # # recklessness for individuals for CW questions
# x <- gk %>%
#   select(uid, Itemnum, Representativeness, Ind_Stimulus.ACC, Ind_Decision.RESP) %>%
#   filter(Representativeness == "UR" & Ind_Stimulus.ACC == 0) %>%
#   select(-Ind_Stimulus.ACC, -Representativeness)
# 
# R.item <- gk %>%
#   filter(Representativeness == "UR") %>%
#   select(Itemnum)
# 
# table(R.item)
# 
# x[x == "y"] <- 1
# x[x == "n"] <- 0
# 
# x <- transform(x, Ind_Decision.RESP = as.numeric(Ind_Decision.RESP))
# 
# odd1 <- c(2, 6, 9, 14, 18)
# 
# odd <- filter(x, Itemnum %in% odd1) %>%
#   mutate(class = "odd")
# 
# even1 <- c(3, 8, 12, 15, 20)
# 
# even <- filter(x, Itemnum %in% even1) %>%
#   mutate(class = "even")
# 
# even <- even %>%
#   group_by(uid) %>%
#   summarise(even = sum(Ind_Decision.RESP))
# 
# odd <- odd %>%
#   group_by(uid) %>%
#   summarise(odd = sum(Ind_Decision.RESP))
# 
# x <- merge(odd, even) %>%
#   mutate(total = odd + even) %>%
#   ungroup(uid) %>%
#   select(-uid)
# 
# cor.test(x$odd, x$even)
# 
# sd(x$odd)
# 
# # use this website to correct using the spearman-brown prophecy formula
# # https://www.cedu.niu.edu/~walker/calculators/sbpf.asp
# 
# # recklessness for teams overall
# x <- gk %>%
#   select(uid, Itemnum, Grp_Stimulus.ACC, Grp_Decision.RESP) %>%
#   filter(Grp_Stimulus.ACC == 0) %>%
#   select(-Grp_Stimulus.ACC)
# 
# x[x == "y"] <- 1
# x[x == "n"] <- 0
# 
# x <- transform(x, Grp_Decision.RESP = as.numeric(Grp_Decision.RESP))
# 
# odd1 <- c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)
# 
# odd <- filter(x, Itemnum %in% odd1) %>%
#   mutate(class = "odd")
# 
# even1 <- c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)
# 
# even <- filter(x, Itemnum %in% even1) %>%
#   mutate(class = "even")
# 
# even <- even %>%
#   group_by(uid) %>%
#   summarise(even = sum(Grp_Decision.RESP))
# 
# odd <- odd %>%
#   group_by(uid) %>%
#   summarise(odd = sum(Grp_Decision.RESP))
# 
# x <- merge(odd, even) %>%
#   mutate(total = odd + even) %>%
#   ungroup(uid) %>%
#   select(-uid)
# 
# cor.test(x$odd, x$even)
# 
# sd(x$odd, na.rm = T)
# 
# # use this website to correct using the spearman-brown prophecy formula
# # https://www.cedu.niu.edu/~walker/calculators/sbpf.asp
# 
# # recklessness for teams for CC questions
# x <- gk %>%
#   select(uid, Itemnum, Representativeness, Grp_Stimulus.ACC, Grp_Decision.RESP) %>%
#   filter(Representativeness == "R" & Grp_Stimulus.ACC == 0) %>%
#   select(-Grp_Stimulus.ACC, -Representativeness)
# 
# R.item <- gk %>%
#   filter(Representativeness == "R") %>%
#   select(Itemnum)
# 
# table(R.item)
# 
# x[x == "y"] <- 1
# x[x == "n"] <- 0
# 
# x <- transform(x, Grp_Decision.RESP = as.numeric(Grp_Decision.RESP))
# 
# odd1 <- c(1, 5, 10, 13, 17)
# 
# odd <- filter(x, Itemnum %in% odd1) %>%
#   mutate(class = "odd")
# 
# even1 <- c(4, 7, 11, 16, 19)
# 
# even <- filter(x, Itemnum %in% even1) %>%
#   mutate(class = "even")
# 
# even <- even %>%
#   group_by(uid) %>%
#   summarise(even = sum(Grp_Decision.RESP))
# 
# odd <- odd %>%
#   group_by(uid) %>%
#   summarise(odd = sum(Grp_Decision.RESP))
# 
# x <- merge(odd, even) %>%
#   mutate(total = odd + even) %>%
#   ungroup(uid) %>%
#   select(-uid)
# 
# cor.test(x$odd, x$even)
# 
# sd(x$odd, na.rm = T)
# 
# # use this website to correct using the spearman-brown prophecy formula
# # https://www.cedu.niu.edu/~walker/calculators/sbpf.asp
# 
# 
# # # recklessness for teams for CW questions
# x <- gk %>%
#   select(uid, Itemnum, Representativeness, Grp_Stimulus.ACC, Grp_Decision.RESP) %>%
#   filter(Representativeness == "UR" & Grp_Stimulus.ACC == 0) %>%
#   select(-Grp_Stimulus.ACC, -Representativeness)
# 
# R.item <- gk %>%
#   filter(Representativeness == "UR") %>%
#   select(Itemnum)
# 
# table(R.item)
# 
# x[x == "y"] <- 1
# x[x == "n"] <- 0
# 
# x <- transform(x, Grp_Decision.RESP = as.numeric(Grp_Decision.RESP))
# 
# odd1 <- c(2, 6, 9, 14, 18)
# 
# odd <- filter(x, Itemnum %in% odd1) %>%
#   mutate(class = "odd")
# 
# even1 <- c(3, 8, 12, 15, 20)
# 
# even <- filter(x, Itemnum %in% even1) %>%
#   mutate(class = "even")
# 
# even <- even %>%
#   group_by(uid) %>%
#   summarise(even = sum(Grp_Decision.RESP))
# 
# odd <- odd %>%
#   group_by(uid) %>%
#   summarise(odd = sum(Grp_Decision.RESP))
# 
# x <- merge(odd, even) %>%
#   mutate(total = odd + even) %>%
#   ungroup(uid) %>%
#   select(-uid)
# 
# cor.test(x$odd, x$even)
# 
# sd(x$odd, na.rm = T)
# 
# # use this website to correct using the spearman-brown prophecy formula
# # https://www.cedu.niu.edu/~walker/calculators/sbpf.asp




# MDMT_individual -----------------------------------------------
surveys_qrt$MDMT_individual_A1 <- as.data.frame(surveys_qrt$MDMT_individual_A1 %>%
                                                  select(ResponseID, StartDate:Finished, uid, MdmtItemNum, Ind_Stimulus.CRESP, 
                                                         Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, 
                                                         Ind_Stimulus.OffsetTime, Ind_Confidence.RESP, Ind_Confidence.OnsetTime, 
                                                         Ind_Confidence.OffsetTime, Ind_Decision.RESP, Ind_Decision.RT, 
                                                         Ind_Decision.OnsetTime, Ind_Decision.OffsetTime) %>%
                                                  rename(Stimulus.CRESP = Ind_Stimulus.CRESP) %>%
                                                  mutate(version = "a1") %>%
                                                  group_by(uid, MdmtItemNum) %>%
                                                  mutate(Ind_Confidence.OffsetTime = as.numeric(Ind_Confidence.OffsetTime),
                                                         Ind_Confidence.OnsetTime = as.numeric(Ind_Confidence.OnsetTime),
                                                         Ind_Confidence.RT = Ind_Confidence.OffsetTime - Ind_Confidence.OnsetTime))

surveys_qrt$MDMT_individual_B1 <- as.data.frame(surveys_qrt$MDMT_individual_B1 %>%
                                                  select(ResponseID, StartDate:Finished, uid, MdmtItemNum, Ind_Stimulus.CRESP, 
                                                         Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, 
                                                         Ind_Stimulus.OffsetTime, Ind_Confidence.RESP, Ind_Confidence.OnsetTime, 
                                                         Ind_Confidence.OffsetTime, Ind_Decision.RESP, Ind_Decision.RT, 
                                                         Ind_Decision.OnsetTime, Ind_Decision.OffsetTime) %>%
                                                  rename(Stimulus.CRESP = Ind_Stimulus.CRESP) %>%
                                                  mutate(version = "b1") %>%
                                                  group_by(uid, MdmtItemNum) %>%
                                                  mutate(Ind_Confidence.OffsetTime = as.numeric(Ind_Confidence.OffsetTime),
                                                         Ind_Confidence.OnsetTime = as.numeric(Ind_Confidence.OnsetTime),
                                                         Ind_Confidence.RT = Ind_Confidence.OffsetTime - Ind_Confidence.OnsetTime))

surveys_java$MDMT_individual_A2 <- surveys_java$MDMT_individual_A2 %>%
  rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, MdmtItemNum = itemnum, 
         Ind_Stimulus.OnsetTime = ind_stimulus_onset,
         Ind_Stimulus.OffsetTime = ind_stimulus_offset, Ind_Stimulus.RT = ind_stimulus_rt,
         Ind_Stimulus.RESP = ind_response, Ind_Confidence.OnsetTime = ind_confidence_onset,
         Ind_Confidence.OffsetTime = ind_confidence_offset, Ind_Confidence.RT = ind_confidence_rt,
         Ind_Confidence.RESP = ind_confidence, Ind_Decision.OnsetTime = ind_decision_onset,
         Ind_Decision.OffsetTime = ind_decision_offset, Ind_Decision.RT = ind_decision_rt,
         Ind_Decision.RESP = ind_decision) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, MdmtItemNum, Stimulus.CRESP, 
         Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime,
         Ind_Confidence.RESP, Ind_Confidence.RT, Ind_Confidence.OnsetTime, Ind_Confidence.OffsetTime,
         Ind_Decision.RESP, Ind_Decision.RT, Ind_Decision.OnsetTime, Ind_Decision.OffsetTime) %>%
  mutate(version = "a2")

surveys_java$MDMT_individual_B2 <- surveys_java$MDMT_individual_B2 %>%
  rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, MdmtItemNum = itemnum, 
         Ind_Stimulus.OnsetTime = ind_stimulus_onset,
         Ind_Stimulus.OffsetTime = ind_stimulus_offset, Ind_Stimulus.RT = ind_stimulus_rt,
         Ind_Stimulus.RESP = ind_response, Ind_Confidence.OnsetTime = ind_confidence_onset,
         Ind_Confidence.OffsetTime = ind_confidence_offset, Ind_Confidence.RT = ind_confidence_rt,
         Ind_Confidence.RESP = ind_confidence, Ind_Decision.OnsetTime = ind_decision_onset,
         Ind_Decision.OffsetTime = ind_decision_offset, Ind_Decision.RT = ind_decision_rt,
         Ind_Decision.RESP = ind_decision) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, MdmtItemNum, Stimulus.CRESP, 
         Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime,
         Ind_Confidence.RESP, Ind_Confidence.RT, Ind_Confidence.OnsetTime, Ind_Confidence.OffsetTime,
         Ind_Decision.RESP, Ind_Decision.RT, Ind_Decision.OnsetTime, Ind_Decision.OffsetTime) %>%
  mutate(version = "b2")

# combine data from a1 and b1 into a single df 
mdmt_ind <- rbind(surveys_qrt$MDMT_individual_A1, surveys_qrt$MDMT_individual_B1,
                  surveys_java$MDMT_individual_A2, surveys_java$MDMT_individual_B2)

# Data Screening ----------------------------------------------------------
# identified duplicated records for most participants created when parsing

mdmt_ind <- mdmt_ind %>% filter(!is.na(MdmtItemNum))


# Check participants appear correct number of times =======================
# ISSUE 1: Identified that the following uids appear twice
#   "18041210_1_d2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_1_g2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_2_d1"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_2_g1"   = all responses legit, P from 1pm session incorrectly assigned same uid

# SOLUTION 1: change uids for these Ps to reflect the correct start time
# new uid for ResponseID = R_afVdc7n6UjyZKS1 becomes 18041213_1_d2
# new uid for ResponseID = R_4Nuw8JPU5ExWxUh becomes 18041213_1_g2
# new uid for ResponseID = R_6yUMj3s9d3otiGV becomes 18041213_1_d1
# new uid for ResponseID = R_9snmMnu9ZKkZJn7 becomes 18041213_1_g1

# ISSUE 2: Identified that uid == "17103012_2_g1" appears twice
# The browser crashed so I had to restart the survey
#
# SOLUTION 2. 
# For uid == "17103012_2_g1" take responses for Q1-3 from 1st attempt 
# ResponseID == "R_dnWhJuwpMkBcVQF" and join with responses for Q4-16 
# from 2nd attemp ResponseID == "R_eRnsJy1NFudWPnT" then remove first attempt

# Which variables will I replace?
vars <- c("Ind_Stimulus.RESP", "Ind_Stimulus.RT", "Ind_Stimulus.OnsetTime", 
          "Ind_Stimulus.OffsetTime", "Ind_Confidence.RESP", "Ind_Confidence.RT", 
          "Ind_Confidence.OnsetTime", "Ind_Confidence.OffsetTime", "Ind_Decision.RESP",
          "Ind_Decision.RT", "Ind_Decision.OffsetTime", "Ind_Decision.OnsetTime")

# For uid == "18040910_2_g1" get Attempt 1 responses
x <- mdmt_ind %>% filter(ResponseID == "R_dnWhJuwpMkBcVQF")

# x <- arrange(x, Itemnum)  # arrange by trial number

x <- x[1:3, vars]  # Take rows 1 to 3 (corresponding to answered questions)
# and variables of interest

itemnum <- c("1", "2", "26")

# Insert data from x into relevant part of df
mdmt_ind[mdmt_ind$ResponseID == "R_eRnsJy1NFudWPnT" & mdmt_ind$MdmtItemNum %in% itemnum, vars] <- x

# Modify uids and remove duplicate attempts
mdmt_ind <- mdmt_ind %>% mutate(uid = ifelse(ResponseID == "R_afVdc7n6UjyZKS1", "18041213_1_d2", uid),
                                uid = ifelse(ResponseID == "R_4Nuw8JPU5ExWxUh", "18041213_1_g2", uid),
                                uid = ifelse(ResponseID == "R_6yUMj3s9d3otiGV", "18041213_2_d1", uid),
                                uid = ifelse(ResponseID == "R_9snmMnu9ZKkZJn7", "18041213_2_g1", uid)) %>%
  filter(ResponseID != "R_dnWhJuwpMkBcVQF")

n_questions <- 16 # number of questions in this test

x <- table(mdmt_ind$uid)

if (any(x != n_questions)) {
  print("Participants not appearing the correct number of times:")
  x[x != n_questions]
} else {
  print("All good! :)")
}


# Check for missing values ================================================
# create vector to select only uid and participants responses
x <- mdmt_ind %>%
  select(uid, MdmtItemNum, Ind_Stimulus.RESP, Ind_Confidence.RESP, Ind_Decision.RESP)

# by column
col_miss <- sapply(x, function(col) {
  col %>%
    is.na() %>%
    sum()
})

# by row
row_miss <- apply(x, 1, function(row) {
  row %>%
    is.na() %>%
    any()
})

check <- if (any(row_miss)) {
  print("missing values for participant:")
  x[row_miss, ] # to print uid if there is a missing value
}  else {
  print("no missing values")
}

# visually check that there are no missing values per user per variable
x %>%
  group_by(uid) %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  gather(key, val, -uid) %>% 
  ggplot(aes(key, uid, fill = factor(val))) +
  geom_tile() +
  scale_fill_brewer(palette = "OrRd") +
  labs(title = "Number of missing values per user, per variable", x = NULL, y = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Check whether vector contains acceptable values () ------------------------
mdmt_ind <- mdmt_ind %>% mutate(Ind_Stimulus.RESP = as.numeric(Ind_Stimulus.RESP),
                                Ind_Confidence.RESP = as.numeric(Ind_Confidence.RESP),
                                Ind_Decision.RESP = tolower(Ind_Decision.RESP))

check_values <- function(x, num_min, num_max = NULL) {
  if (is.null(num_max)) {
    any_wrong <- !all(x %in% num_min)
  } else {
    any_wrong <- any(x < num_min | x > num_max, na.rm = TRUE)
  }
  
  if (any_wrong) {
    print("Unacceptable value present in this variable.")
  } else {
    print("All good! :)")
  }
}

check_values(mdmt_ind$Ind_Stimulus.RESP, 0, 3)
check_values(mdmt_ind$Ind_Confidence.RESP, 25, 100)
check_values(mdmt_ind$Ind_Decision.RESP, c("a", "b"))

# add Stimulus.ACC column (Stimulus.RESP == Stimulus.CRESP)
mdmt_ind <- mdmt_ind %>% 
  group_by(uid, MdmtItemNum) %>%
  mutate(Ind_Stimulus.ACC = Ind_Stimulus.RESP == Stimulus.CRESP) %>%
  ungroup()

# Create grouping variables -----------------------------------------------
# group == date + group
# member == g == 1, d == 2
mdmt_ind <- mdmt_ind %>% mutate(member = ifelse(grepl("g[1-2]", uid), "1", 
                                         ifelse(grepl("d[1-2]", uid), "2", 
                                         ifelse(grepl("e1", uid), "1", "2"))),
                                recruit = ifelse(grepl("[dg]", uid), "s", "e"),
                                group = gsub("_[a-z][12]", "", uid))

# discovered that uid == "17100609_2_g1" does not have a teammate
# Not sure where their data went? All participants were in teams
# remove for now and investigate later
mdmt_ind <- mdmt_ind %>% filter(uid != "17100609_2_g1")

# calculate data for control analyses (accuracy, confidence, decision and RTs)
mdmt_ind <- mdmt_ind %>%
  mutate(Ind_Confidence.RESP = as.numeric(Ind_Confidence.RESP),
         Ind_Stimulus.OffsetTime = as.numeric(Ind_Stimulus.OffsetTime),
         Ind_Stimulus.OnsetTime = as.numeric(Ind_Stimulus.OnsetTime),
         Ind_Confidence.OffsetTime = as.numeric(Ind_Confidence.OffsetTime),
         Ind_Confidence.OnsetTime = as.numeric(Ind_Confidence.OnsetTime),
         Ind_Decision.OffsetTime = as.numeric(Ind_Decision.OffsetTime),
         Ind_Decision.OnsetTime = as.numeric(Ind_Decision.OnsetTime),
         Ind_Decision.RESP = tolower(Ind_Decision.RESP),
         Ind_Stimulus.RT = as.numeric(Ind_Stimulus.RT),
         Ind_Confidence.RT = as.numeric(Ind_Confidence.RT),
         Ind_Decision.RT = as.numeric(Ind_Decision.RT))
# write_csv("data/control/mdmt_ind.csv")


# Calculate average time on task
# add 3 mins for memorising stimulus
# mdmt_ind %>% 
#   select(uid, contains(".RT")) %>% 
#   gather(var, val, -uid) %>% 
#   group_by(uid) %>% 
#   summarise(time = sum(val)/1000/60) %>% 
#   ungroup() %>% 
#   summarise(time = mean(time))
  


# Compute all variables ------------------------------------------------
mdmt_ind.uid <- mdmt_ind %>%
  group_by(uid) %>%
  summarise(
    group = group[1],
    mdmt_ind.acc = mean(Ind_Stimulus.ACC) * 100,
    mdmt_ind.conf = mean(Ind_Confidence.RESP),
    mdmt_ind.dec = mean(Ind_Decision.RESP == "a") * 100,
    mdmt_ind.rec = 100 * mean(Ind_Decision.RESP == "a" & Ind_Stimulus.ACC == 0, na.rm = TRUE) 
    / mean(Ind_Stimulus.ACC == 0, na.rm = TRUE))
# mdmt_ind.ind.comp = (100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 1, na.rm = TRUE)) + 
#  (100 * mean(Ind_Decision.RESP == "n" & Ind_Stimulus.ACC == 0, na.rm = TRUE)) / 30,
# mdmt_ind.ind.optim = (100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 1, na.rm = TRUE)) 
# / 30,
# mdmt_ind.ind.hes = 100 * mean(Ind_Decision.RESP == "n" & Ind_Stimulus.ACC == 1, na.rm = TRUE) 
# / mean(Ind_Stimulus.ACC == 1, na.rm = TRUE))


# Reliability ----------------------------------------------------
# # Accuracy 
# x <-  mdmt_ind %>%
#   select(uid, MdmtItemNum, Ind_Stimulus.ACC) %>%
#   mutate(Ind_Stimulus.ACC = ifelse(Ind_Stimulus.ACC == TRUE, 1, 0)) %>%
#   spread(MdmtItemNum, Ind_Stimulus.ACC) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Confidence
# x <- mdmt_ind %>%
#   select(uid, MdmtItemNum, Ind_Confidence.RESP) %>%
#   spread(MdmtItemNum, Ind_Confidence.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Decisiveness
# x <- mdmt_ind %>%
#   select(uid, MdmtItemNum, Ind_Decision.RESP)
# 
# x[x == "a"] <- 1
# x[x == "b"] <- 0
# 
# x <- transform(x, Ind_Decision.RESP = as.numeric(Ind_Decision.RESP))
# 
# x <- x %>%
#   spread(MdmtItemNum, Ind_Decision.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Recklessness
# x <- mdmt_ind %>%
#   select(uid, MdmtItemNum, Ind_Stimulus.ACC, Ind_Decision.RESP) %>%
#   filter(Ind_Stimulus.ACC == 0) %>%
#   select(-Ind_Stimulus.ACC)
# 
# x[x == "a"] <- 1
# x[x == "b"] <- 0
# 
# x <- transform(x, Ind_Decision.RESP = as.numeric(Ind_Decision.RESP))
# 
# odd1 <- c(1, 26, 15, 14, 5, 29, 17, 31)
# 
# odd <- filter(x, MdmtItemNum %in% odd1)
# 
# even1 <- c(2, 23, 33, 24, 11, 8, 20, 12)
# 
# even <- filter(x, MdmtItemNum %in% even1)
# 
# even <- even %>%
#   group_by(uid) %>%
#   summarise(even = sum(Ind_Decision.RESP, na.rm = TRUE))
# 
# odd <- odd %>%
#   group_by(uid) %>%
#   summarise(odd = sum(Ind_Decision.RESP, na.rm = TRUE))
# 
# x <- merge(odd, even) %>%
#   mutate(total = odd + even) %>%
#   ungroup(uid) %>%
#   select(-uid)
# 
# cor.test(x$odd, x$even)
#
# use this website to correct using the spearman-brown prophecy formula
# https://www.cedu.niu.edu/~walker/calculators/sbpf.asp




# MDMT_team -----------------------------------------------
surveys_qrt$MDMT_team_A1 <- as.data.frame(surveys_qrt$MDMT_team_A1 %>%
  select(ResponseID, StartDate:Finished, uid, MdmtItemNum, Grp_Stimulus.CRESP, 
         Grp_Stimulus.RESP, Grp_Stimulus.RT, Grp_Stimulus.OnsetTime, 
         Grp_Stimulus.OffsetTime, Grp_Confidence.RESP, Grp_Confidence.OnsetTime, 
         Grp_Confidence.OffsetTime, Grp_Decision.RESP, Grp_Decision.RT, 
         Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
  rename(Stimulus.CRESP = Grp_Stimulus.CRESP) %>%
  mutate(version = "a1") %>%
  group_by(uid, MdmtItemNum) %>%
  mutate(Grp_Confidence.OffsetTime = as.numeric(Grp_Confidence.OffsetTime),
         Grp_Confidence.OnsetTime = as.numeric(Grp_Confidence.OnsetTime),
         Grp_Confidence.RT = Grp_Confidence.OffsetTime - Grp_Confidence.OnsetTime))

surveys_qrt$MDMT_team_B1 <- as.data.frame(surveys_qrt$MDMT_team_B1 %>%
  select(ResponseID, StartDate:Finished, uid, MdmtItemNum, Grp_Stimulus.CRESP, 
         Grp_Stimulus.RESP, Grp_Stimulus.RT, Grp_Stimulus.OnsetTime, 
         Grp_Stimulus.OffsetTime, Grp_Confidence.RESP, Grp_Confidence.OnsetTime, 
         Grp_Confidence.OffsetTime, Grp_Decision.RESP, Grp_Decision.RT, 
         Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
  rename(Stimulus.CRESP = Grp_Stimulus.CRESP) %>%
  mutate(version = "b1") %>%
  group_by(uid, MdmtItemNum) %>%
  mutate(Grp_Confidence.OffsetTime = as.numeric(Grp_Confidence.OffsetTime),
         Grp_Confidence.OnsetTime = as.numeric(Grp_Confidence.OnsetTime),
         Grp_Confidence.RT = Grp_Confidence.OffsetTime - Grp_Confidence.OnsetTime))

surveys_java$MDMT_team_A2 <- surveys_java$MDMT_team_A2 %>%
  rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, MdmtItemNum = itemnum, 
         Grp_Stimulus.OnsetTime = grp_stimulus_onset,
         Grp_Stimulus.OffsetTime = grp_stimulus_offset, Grp_Stimulus.RT = grp_stimulus_rt,
         Grp_Stimulus.RESP = grp_response, Grp_Confidence.OnsetTime = grp_confidence_onset,
         Grp_Confidence.OffsetTime = grp_confidence_offset, Grp_Confidence.RT = grp_confidence_rt,
         Grp_Confidence.RESP = grp_confidence, Grp_Decision.OnsetTime = grp_decision_onset,
         Grp_Decision.OffsetTime = grp_decision_offset, Grp_Decision.RT = grp_decision_rt,
         Grp_Decision.RESP = grp_decision) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, MdmtItemNum, Stimulus.CRESP, 
         Grp_Stimulus.RESP, Grp_Stimulus.RT, Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime,
         Grp_Confidence.RESP, Grp_Confidence.RT, Grp_Confidence.OnsetTime, Grp_Confidence.OffsetTime,
         Grp_Decision.RESP, Grp_Decision.RT, Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
  mutate(version = "a2")

surveys_java$MDMT_team_B2 <- surveys_java$MDMT_team_B2 %>%
  rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, MdmtItemNum = itemnum, 
         Grp_Stimulus.OnsetTime = grp_stimulus_onset,
         Grp_Stimulus.OffsetTime = grp_stimulus_offset, Grp_Stimulus.RT = grp_stimulus_rt,
         Grp_Stimulus.RESP = grp_response, Grp_Confidence.OnsetTime = grp_confidence_onset,
         Grp_Confidence.OffsetTime = grp_confidence_offset, Grp_Confidence.RT = grp_confidence_rt,
         Grp_Confidence.RESP = grp_confidence, Grp_Decision.OnsetTime = grp_decision_onset,
         Grp_Decision.OffsetTime = grp_decision_offset, Grp_Decision.RT = grp_decision_rt,
         Grp_Decision.RESP = grp_decision) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, MdmtItemNum, Stimulus.CRESP, 
         Grp_Stimulus.RESP, Grp_Stimulus.RT, Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime,
         Grp_Confidence.RESP, Grp_Confidence.RT, Grp_Confidence.OnsetTime, Grp_Confidence.OffsetTime,
         Grp_Decision.RESP, Grp_Decision.RT, Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
  mutate(version = "b2")

# combine data from a1 and b1 into a single df 
mdmt_team <- rbind(surveys_qrt$MDMT_team_A1, surveys_qrt$MDMT_team_B1,
                   surveys_java$MDMT_team_A2, surveys_java$MDMT_team_B2)

# Data Screening ----------------------------------------------------------
# identified duplicated records for most participants created when parsing

mdmt_team <- mdmt_team %>% filter(!is.na(MdmtItemNum))


# Check participants appear correct number of times =======================
# ISSUE 1: Identified that the following uids appear twice
#   "18041210_1_d2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_1_g2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_2_d1"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_2_g1"   = all responses legit, P from 1pm session incorrectly assigned same uid

# SOLUTION 1: change uids for these Ps to reflect the correct start time
# new uid for ResponseID = R_7O5Z8EYXfOdaKuF becomes 18041213_1_d2
# new uid for ResponseID = R_1B35tJ8AUKVbV1b becomes 18041213_1_g2
# new uid for ResponseID = R_9pojwpBJgp6FKqF becomes 18041213_2_d1
# new uid for ResponseID = R_cT0ougvPqsWbmTP becomes 18041213_2_g1

# Modify uids
mdmt_team <- mdmt_team %>% mutate(uid = ifelse(ResponseID == "R_7O5Z8EYXfOdaKuF", "18041213_1_d2", uid),
                                  uid = ifelse(ResponseID == "R_1B35tJ8AUKVbV1b", "18041213_1_g2", uid),
                                  uid = ifelse(ResponseID == "R_9pojwpBJgp6FKqF", "18041213_2_d1", uid),
                                  uid = ifelse(ResponseID == "R_cT0ougvPqsWbmTP", "18041213_2_g1", uid))

n_questions <- 16 # number of questions in this test

x <- table(mdmt_team$uid)

if (any(x != n_questions)) {
  print("Participants not appearing the correct number of times:")
  x[x != n_questions]
} else {
  print("All good! :)")
}


# Check for missing values ================================================
# create vector to select only uid and participants responses
x <- mdmt_team %>%
  select(uid, MdmtItemNum, Grp_Stimulus.RESP, Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime,
         Grp_Confidence.RESP, Grp_Decision.RESP)

# by column
col_miss <- sapply(x, function(col) {
  col %>%
    is.na() %>%
    sum()
})

# by row
row_miss <- apply(x, 1, function(row) {
  row %>%
    is.na() %>%
    any()
})

check <- if (any(row_miss)) {
  print("missing values for participant:")
  x[row_miss, ] # to print uid if there is a missing value
}  else {
  print("no missing values")
}

# visually check that there are no missing values per user per variable
x %>%
  group_by(uid) %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  gather(key, val, -uid) %>% 
  ggplot(aes(key, uid, fill = factor(val))) +
  geom_tile() +
  scale_fill_brewer(palette = "OrRd") +
  labs(title = "Number of missing values per user, per variable", x = NULL, y = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Check whether vector contains acceptable values () ------------------------
mdmt_team <- mdmt_team %>% mutate(Grp_Stimulus.RESP = as.numeric(Grp_Stimulus.RESP),
                                  Grp_Confidence.RESP = as.numeric(Grp_Confidence.RESP),
                                  Grp_Decision.RESP = tolower(Grp_Decision.RESP))

check_values <- function(x, num_min, num_max = NULL) {
  if (is.null(num_max)) {
    any_wrong <- !all(x %in% num_min)
  } else {
    any_wrong <- any(x < num_min | x > num_max, na.rm = TRUE)
  }
  
  if (any_wrong) {
    print("Unacceptable value present in this variable.")
  } else {
    print("All good! :)")
  }
}

check_values(mdmt_team$Grp_Stimulus.RESP, 0, 3)
check_values(mdmt_team$Grp_Confidence.RESP, 25, 100)
check_values(mdmt_team$Grp_Decision.RESP, c("a", "b"))


# add Stimulus.ACC column (Stimulus.RESP == Stimulus.CRESP)
check <- mdmt_team %>% 
  group_by(uid, MdmtItemNum) %>%
  mutate(Grp_Stimulus.ACC = Grp_Stimulus.RESP == Stimulus.CRESP) %>%
  ungroup()


# Check teams are giving same response and decision =======================
# Create grouping variables -----------------------------------------------
# group == date + group
# member == g == 1, d == 2
mdmt_team <- mdmt_team %>% mutate(member = ifelse(grepl("g[1-2]", uid), "1", 
                                           ifelse(grepl("d[1-2]", uid), "2", 
                                           ifelse(grepl("e1", uid), "1", "2"))),
                                  recruit = ifelse(grepl("[dg]", uid), "s", "e"),
                                  group = gsub("_[a-z][12]", "", uid))

# discovered that uid == "17100609_2_g1" does not have a teammate
# Not sure where their data went? All participants were in teams
# remove for now and investigate later
mdmt_team <- mdmt_team %>% filter(uid != "17100609_2_g1")

# Decisions
# in cases where member's group decisions differ change to missing
# Justification: If decision is different for members it is impossible to
# determine which decision was agreed upon by members. As decision was provided last
# other data for same item remains valid, if matching.

x <- mdmt_team %>% select(group, member, MdmtItemNum, Grp_Decision.RESP) %>% 
  spread(member, Grp_Decision.RESP) %>%
  group_by(group, MdmtItemNum) %>%
  mutate(dif = `1` != `2`) %>%
  filter(dif == TRUE)

# prints out number of times that group decisions differed between members
cat("number of times that group decision differed between members:", nrow(x))

for (i in seq(nrow(x))){
  group <- x$group[i]
  MdmtItemNum <- x$MdmtItemNum[i]
  mdmt_team[mdmt_team$group == group & mdmt_team$MdmtItemNum == MdmtItemNum, 
            c("Grp_Decision.RESP", "Grp_Decision.OnsetTime", 
              "Grp_Decision.OffsetTime", "Grp_Decision.RT")] <- NA
}

# Accuracy
# in cases where either is different change to missing for all variables 
# (Grp_Stimulus.ACC, Grp_Confidence.RESP, Grp_Decision.RESP)
# Justification: If accuracy differs for members it is impossible to
# determine which response was agreed upon by members. Confidence and decision 
# were contingent upon accuracy, as it was provided first.

x <- mdmt_team %>% select(group, member, MdmtItemNum, Grp_Stimulus.RESP) %>% 
  spread(member, Grp_Stimulus.RESP) %>%
  group_by(group, MdmtItemNum) %>% 
  mutate(dif = `1` != `2`) %>%
  filter(dif == TRUE)

# prints out number of times that group accuracy differed between members
cat("number of times that group accuracy differed between members:", nrow(x))

for (i in seq(nrow(x))){
  group <- x$group[i]
  MdmtItemNum <- x$MdmtItemNum[i]
  mdmt_team[mdmt_team$group == group & mdmt_team$MdmtItemNum == MdmtItemNum, 
            c("Grp_Stimulus.RESP", "Grp_Stimulus.OnsetTime", "Grp_Stimulus.OffsetTime",
              "Grp_Stimulus.RT", "Grp_Confidence.RESP", "Grp_Confidence.OnsetTime", 
              "Grp_Confidence.OffsetTime", "Grp_Confidence.RT", "Grp_Decision.RESP", 
              "Grp_Decision.OnsetTime", "Grp_Decision.OffsetTime", "Grp_Decision.RT")] <- NA
}


# Compute all variables ------------------------------------------------
# calculate data for control analyses (accuracy, confidence, decision and RTs)
mdmt_team <- mdmt_team %>%
  mutate(Grp_Stimulus.ACC = Grp_Stimulus.RESP == Stimulus.CRESP,
         Grp_Confidence.RESP = as.numeric(Grp_Confidence.RESP),
         Grp_Stimulus.OffsetTime = as.numeric(Grp_Stimulus.OffsetTime),
         Grp_Stimulus.OnsetTime = as.numeric(Grp_Stimulus.OnsetTime),
         Grp_Confidence.OffsetTime = as.numeric(Grp_Confidence.OffsetTime),
         Grp_Confidence.OnsetTime = as.numeric(Grp_Confidence.OnsetTime),
         Grp_Decision.OffsetTime = as.numeric(Grp_Decision.OffsetTime),
         Grp_Decision.OnsetTime = as.numeric(Grp_Decision.OnsetTime),
         Grp_Decision.RESP = tolower(Grp_Decision.RESP))


mdmt_team.uid <- mdmt_team %>%
  group_by(uid) %>%
  summarise(
    group = group[1],
    mdmt_grp.acc = mean(Grp_Stimulus.ACC, na.rm = TRUE) * 100,
    mdmt_grp.conf = mean(Grp_Confidence.RESP, na.rm = TRUE),
    mdmt_grp.dec = mean(Grp_Decision.RESP == "a", na.rm = TRUE) * 100,
    mdmt_grp.rec = 100 * mean(Grp_Decision.RESP == "a" & Grp_Stimulus.ACC == 0, na.rm = TRUE) 
    / mean(Grp_Stimulus.ACC == 0, na.rm = TRUE))
# mdmt_grp.grp.comp = (100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 1, na.rm = TRUE)) + 
#  (100 * mean(Grp_Decision.RESP == "n" & Grp_Stimulus.ACC == 0, na.rm = TRUE)) / 30,
# mdmt_grp.grp.optim = (100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 1, na.rm = TRUE)) 
# / 30,
# mdmt_grp.grp.hes = 100 * mean(Grp_Decision.RESP == "n" & Grp_Stimulus.ACC == 1, na.rm = TRUE) 
# / mean(Grp_Stimulus.ACC == 1, na.rm = TRUE))


# calculate group variables for control paper
uid <- mdmt_team %>%
  group_by(group, MdmtItemNum) %>%
  mutate(Grp_Stimulus.ACC = Grp_Stimulus.RESP == Stimulus.CRESP,
         Grp_Stimulus.RT = min(Grp_Stimulus.OffsetTime) - min(Grp_Stimulus.OnsetTime),
         Grp_Confidence.RESP = mean(Grp_Confidence.RESP, na.rm = TRUE),
         Grp_Confidence.RT = min(Grp_Confidence.OffsetTime) - min(Grp_Confidence.OnsetTime),
         Grp_Decision.RT = min(Grp_Decision.OffsetTime) - min(Grp_Decision.OnsetTime)) %>% ungroup()


# remove RTs that are negative or too fast (e.g., less than 3 seconds)
# only 3 RTs < 3 seconds. Inspected them and they look like normal responses. Retain.
# uid %>% filter(Grp_Stimulus.RT <= 3000) %>% select(group, Grp_Stimulus.RT)

 # modify or remove RTs that are unusual
# look for very slow RTs (e.g., > 3 SDs)
# library(lubridate)
# 
# uid %>% filter(group == "18081711_1" & MdmtItemNum == 20) %>% 
#   select(uid, MdmtItemNum, Grp_Stimulus.RESP, Grp_Stimulus.RT, 
#          Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime) %>%
#   group_by(uid) %>%
#   mutate(rt = Grp_Stimulus.OffsetTime - Grp_Stimulus.OnsetTime) %>%
#   ungroup() %>%
#   mutate(
#          Grp_Stimulus.OnsetTime = Grp_Stimulus.OnsetTime / 1000,
#          Grp_Stimulus.OffsetTime = Grp_Stimulus.OffsetTime / 1000,
#          Grp_Stimulus.OnsetTime = as.POSIXct(Grp_Stimulus.OnsetTime, origin = "1970-01-01 00:00.00 UTC", tz = "Etc/GMT-11"),
#          Grp_Stimulus.OffsetTime = as.POSIXct(Grp_Stimulus.OffsetTime, origin = "1970-01-01 00:00.00 UTC", tz = "Etc/GMT-11"))


# groups that have more than 4 very fast RTs
# none to worry about in for this task
# uid %>% filter(Grp_Stimulus.RT < 3000) %>%
#   group_by(group) %>%
#   mutate(n = n()) %>%
#   select(MdmtItemNum, n, Grp_Stimulus.ACC, Grp_Stimulus.RT, Grp_Confidence.RESP)

d_mdmt_grp <- uid %>%
  group_by(group, MdmtItemNum) %>%
  summarise(Grp_Stimulus.ACC = Grp_Stimulus.ACC[1],
            Grp_Stimulus.RESP = Grp_Stimulus.RESP[1],
            Grp_Stimulus.RT = Grp_Stimulus.RT[1],
            Grp_Confidence.RESP = Grp_Confidence.RESP[1],
            #        Grp_Confidence.RT = Grp_Confidence.RT[1],
            Grp_Decision.RESP = Grp_Decision.RESP[1]
            #        Grp_Decision.RT = Grp_Decision.RT[1]
  ) 
 # write_csv("data/control/mdmt_team.csv")


# Reliability ----------------------------------------------------
# #  Accuracy
#   x <-  rapm_team %>%
#     select(uid, ApmItemNum, Grp_Stimulus.ACC) %>%
#     mutate(Grp_Stimulus.ACC = ifelse(Grp_Stimulus.ACC == TRUE, 1, 0)) %>%
#     spread(ApmItemNum, Grp_Stimulus.ACC) %>%
#     select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
#   # Confidence
#   x <- rapm_team %>%
#     select(uid, ApmItemNum, Grp_Confidence.RESP) %>%
#     spread(ApmItemNum, Grp_Confidence.RESP) %>%
#     select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
#   # Decisiveness
#   x <- rapm_team %>%
#     select(uid, ApmItemNum, Grp_Decision.RESP)
# 
#   x[x == "y"] <- 1
#   x[x == "n"] <- 0
# 
#   x <- transform(x, Grp_Decision.RESP = as.numeric(Grp_Decision.RESP))
# 
#   x <- x %>%
#     spread(ApmItemNum, Grp_Decision.RESP) %>%
#     select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
#   # Recklessness
#   x <- rapm_team %>%
#     select(uid, ApmItemNum, Grp_Stimulus.ACC, Grp_Decision.RESP) %>%
#     filter(Grp_Stimulus.ACC == 0) %>%
#     select(-Grp_Stimulus.ACC)
# 
#   x[x == "y"] <- 1
#   x[x == "n"] <- 0
# 
#   x <- transform(x, Grp_Decision.RESP = as.numeric(Grp_Decision.RESP))
# 
#   odd1 <- c(2, 6, 10, 14, 18, 22, 26, 30, 34)
# 
#   odd <- filter(x, ApmItemNum %in% odd1)
# 
#   even1 <- c(4, 8, 12, 16, 20, 24, 28, 32, 36)
# 
#   even <- filter(x, ApmItemNum %in% even1)
# 
#   even <- even %>%
#     group_by(uid) %>%
#     summarise(even = sum(Grp_Decision.RESP, na.rm = TRUE))
# 
#   odd <- odd %>%
#     group_by(uid) %>%
#     summarise(odd = sum(Grp_Decision.RESP, na.rm = TRUE))
# 
#   x <- merge(odd, even) %>%
#     mutate(total = odd + even) %>%
#     ungroup(uid) %>%
#     select(-uid)
# 
#   # write.csv(x, "RAPM_splithalf.csv")
# 
#   cor.test(x$odd, x$even)
#
# use this website to correct using the spearman-brown prophecy formula
# https://www.cedu.niu.edu/~walker/calculators/sbpf.asp



# MiniIPIP ----------------------------------------------------------------
surveys_qrt$MiniIPIP_A1 <- surveys_qrt$MiniIPIP_A1 %>% 
  select(ResponseID, StartDate:Finished, uid, # MINIIPIPtest.TrialNr,
         factor, key, Stimulus.RESP, Stimulus.RT, Stimulus.OnsetTime, 
         Stimulus.OffsetTime) %>%
  #  rename(Itemnum = MINIIPIPtest.TrialNr) %>%
  mutate(version = "a1")

surveys_qrt$MiniIPIP_B1 <- surveys_qrt$MiniIPIP_B1 %>% 
  select(ResponseID, StartDate:Finished, uid, # MINIIPIPtest.TrialNr,
         factor, key, Stimulus.RESP, Stimulus.RT, Stimulus.OnsetTime, 
         Stimulus.OffsetTime) %>%
  #  rename(Itemnum = MINIIPIPtest.TrialNr) %>%
  mutate(version = "b1")

# surveys_java$MiniIPIP_A2 %>%
#   group_by(uid) %>%
#   mutate(Itemnum = n()) %>%
#   select(uid, item, Itemnum)

surveys_java$MiniIPIP_A2 <- surveys_java$MiniIPIP_A2 %>%
  rename(ResponseID = ResponseId, Stimulus.RESP = response, 
         Stimulus.OnsetTime = onset, Stimulus.OffsetTime = offset, 
         Stimulus.RT = rt, factor = facet) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid,
         factor, key, Stimulus.RESP, Stimulus.RT, Stimulus.OnsetTime, 
         Stimulus.OffsetTime) %>%
  mutate(version = "a2")

surveys_java$MiniIPIP_B2 <- surveys_java$MiniIPIP_B2 %>%
  rename(ResponseID = ResponseId, Stimulus.RESP = response, 
         Stimulus.OnsetTime = onset, Stimulus.OffsetTime = offset, 
         Stimulus.RT = rt, factor = facet) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid,
         factor, key, Stimulus.RESP, Stimulus.RT, Stimulus.OnsetTime, 
         Stimulus.OffsetTime) %>%
  mutate(version = "b2")

# combine data from a1, b1, a2 and b2 into a single df 
ipip <- rbind(surveys_qrt$MiniIPIP_A1, surveys_qrt$MiniIPIP_B1, 
              surveys_java$MiniIPIP_A2, surveys_java$MiniIPIP_B2)

# Data Screening ----------------------------------------------------------

# Check participants appear correct number of times =======================
# ISSUE 1: 
# identified that the following uids appear twice
# 1.    "18041210_1_d2"   = all responses legit, P from 1pm session assigned incorrect uid
# 1.    "18041210_1_g2"   = all responses legit, P from 1pm session assigned incorrect uid
# 1.    "18041210_2_d1"   = all responses legit, P from 1pm session assigned incorrect uid
# 1.    "18041210_2_g1"   = all responses legit, P from 1pm session assigned incorrect uid
# SOLUTION: 
# change uids for Ps in 1pm session to reflect the correct start time
# new uid for ResponseID = R_5zh4gP0x8XGWWs5 becomes 18041213_1_d2
# new uid for ResponseID = R_d7tWP92nnNjv6C1 becomes 18041213_1_g2
# new uid for ResponseID = R_9skqG9xRVOs6Xrv becomes 18041213_2_d1
# new uid for ResponseID = R_82CDpPsNNqz6c85 becomes 18041213_2_g1
#
# ISSUE 2:
# Identified that uid == "18081416_2_e1" completed the mini-ipip twice
# SOLUTION:
# Remove second attempt from data, ResponseID == R_2eaPPKJnrYg2zaI 

ipip <- ipip %>% mutate(uid = ifelse(ResponseID == "R_5zh4gP0x8XGWWs5", "18041213_1_d2", uid),
                        uid = ifelse(ResponseID == "R_d7tWP92nnNjv6C1", "18041213_1_g2", uid),
                        uid = ifelse(ResponseID == "R_9skqG9xRVOs6Xrv", "18041213_2_d1", uid),
                        uid = ifelse(ResponseID == "R_82CDpPsNNqz6c85", "18041213_2_g1", uid)) %>%
  filter(ResponseID != "R_2eaPPKJnrYg2zaI")

n_questions <- 20 # number of questions in this test

x <- table(ipip$uid)

if (any(x != n_questions)) {
  print("Participants not appearing the correct number of times:")
  x[x != n_questions]
} else {
  print("All good! :)")
}

# Check for missing values ================================================
# create vector to select only uid and participants responses
x <- ipip %>%
  select(uid, Stimulus.RESP)

# by column
col_miss <- sapply(x, function(col) {
  col %>%
    is.na() %>%
    sum()
})

# by row
row_miss <- apply(x, 1, function(row) {
  row %>%
    is.na() %>%
    any()
})

if (any(row_miss)) {
  print("missing values for participant:")
  x[row_miss, ] # to print uid if there is a missing value
}  else {
  print("no missing values")
}


# visually check that there are no missing values per user per variable
x %>%
  group_by(uid) %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  gather(key, val, -uid) %>% 
  ggplot(aes(key, uid, fill = factor(val))) +
  geom_tile() +
  scale_fill_brewer(palette = "OrRd") +
  labs(title = "Number of missing values per user, per variable", x = NULL, y = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Check whether vector contains acceptable values (range == 1 - 7) ------------------------
check_values <- function(x, num_min, num_max = NULL) {
  if (is.null(num_max)) {
    any_wrong <- !all(x %in% num_min)
  } else {
    any_wrong <- any(x < num_min | x > num_max, na.rm = TRUE)
  }
  
  if (any_wrong) {
    print("Unacceptable value present in this variable.")
  } else {
    print("All good! :)")
  }
}

ipip <- ipip %>% 
  mutate(Stimulus.RESP = as.numeric(Stimulus.RESP))

check_values(ipip$Stimulus.RESP, 1, 5)


# Compute Variables -------------------------------------------------------
# score personality factors

ipip %>%
  mutate(Stimulus.RESP = ifelse(key == 1, Stimulus.RESP, 6 - Stimulus.RESP)) %>%  # Reverse score negative items
  group_by(uid, factor) %>%
  summarise(
    score = mean(Stimulus.RESP, na.rm = TRUE)
  ) %>%
  spread(factor, score)


# Reliability ----------------------------------------------------
# x <- ipip %>%
#   group_by(uid) %>%
#   mutate(itemnum = 1:n()) %>%
#   ungroup() %>%
#   mutate(Stimulus.RESP = ifelse(key == 1, Stimulus.RESP,
#                          6 - Stimulus.RESP))  %>% # Reverse score negative items
#   filter(factor == "intellect") %>% # calculate for each trait
#   select(uid, itemnum, Stimulus.RESP) %>%
#   spread(itemnum, Stimulus.RESP) %>%
#   select(-uid)
# 
# names(x) <- paste0("a", 1:4)
# 
# psych::alpha(as.data.frame(x))


# RAPM_individual -----------------------------------------------
surveys_qrt$RAPM_individual_A1 <- as.data.frame(surveys_qrt$RAPM_individual_A1 %>%
      select(ResponseID, StartDate:Finished, uid, ApmItemNum, Ind_Stimulus.CRESP, 
             Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, 
             Ind_Stimulus.OffsetTime, Ind_Confidence.RESP, Ind_Confidence.OnsetTime, 
             Ind_Confidence.OffsetTime, Ind_Decision.RESP, Ind_Decision.RT, 
             Ind_Decision.OnsetTime, Ind_Decision.OffsetTime) %>%
      rename(Stimulus.CRESP = Ind_Stimulus.CRESP) %>%
      mutate(version = "a1") %>%
      group_by(uid, ApmItemNum) %>%
      mutate(Ind_Confidence.OffsetTime = as.numeric(Ind_Confidence.OffsetTime),
             Ind_Confidence.OnsetTime = as.numeric(Ind_Confidence.OnsetTime),
             Ind_Confidence.RT = Ind_Confidence.OffsetTime - Ind_Confidence.OnsetTime))

surveys_qrt$RAPM_individual_B1 <- as.data.frame(surveys_qrt$RAPM_individual_B1 %>%
      select(ResponseID, StartDate:Finished, uid, ApmItemNum, Ind_Stimulus.CRESP, 
             Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, 
             Ind_Stimulus.OffsetTime, Ind_Confidence.RESP, Ind_Confidence.OnsetTime, 
             Ind_Confidence.OffsetTime, Ind_Decision.RESP, Ind_Decision.RT, 
             Ind_Decision.OnsetTime, Ind_Decision.OffsetTime) %>%
      rename(Stimulus.CRESP = Ind_Stimulus.CRESP) %>%
      mutate(version = "b1") %>%
      group_by(uid, ApmItemNum) %>%
      mutate(Ind_Confidence.OffsetTime = as.numeric(Ind_Confidence.OffsetTime),
             Ind_Confidence.OnsetTime = as.numeric(Ind_Confidence.OnsetTime),
             Ind_Confidence.RT = Ind_Confidence.OffsetTime - Ind_Confidence.OnsetTime))

surveys_java$RAPM_individual_A2 <- surveys_java$RAPM_individual_A2 %>%
  rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, ApmItemNum = itemnum, 
         Ind_Stimulus.OnsetTime = ind_stimulus_onset,
         Ind_Stimulus.OffsetTime = ind_stimulus_offset, Ind_Stimulus.RT = ind_stimulus_rt,
         Ind_Stimulus.RESP = ind_response, Ind_Confidence.OnsetTime = ind_confidence_onset,
         Ind_Confidence.OffsetTime = ind_confidence_offset, Ind_Confidence.RT = ind_confidence_rt,
         Ind_Confidence.RESP = ind_confidence, Ind_Decision.OnsetTime = ind_decision_onset,
         Ind_Decision.OffsetTime = ind_decision_offset, Ind_Decision.RT = ind_decision_rt,
         Ind_Decision.RESP = ind_decision) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, ApmItemNum, Stimulus.CRESP, 
         Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime,
         Ind_Confidence.RESP, Ind_Confidence.RT, Ind_Confidence.OnsetTime, Ind_Confidence.OffsetTime,
         Ind_Decision.RESP, Ind_Decision.RT, Ind_Decision.OnsetTime, Ind_Decision.OffsetTime) %>%
  mutate(version = "a2")

surveys_java$RAPM_individual_B2 <- surveys_java$RAPM_individual_B2 %>%
  rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, ApmItemNum = itemnum, 
         Ind_Stimulus.OnsetTime = ind_stimulus_onset,
         Ind_Stimulus.OffsetTime = ind_stimulus_offset, Ind_Stimulus.RT = ind_stimulus_rt,
         Ind_Stimulus.RESP = ind_response, Ind_Confidence.OnsetTime = ind_confidence_onset,
         Ind_Confidence.OffsetTime = ind_confidence_offset, Ind_Confidence.RT = ind_confidence_rt,
         Ind_Confidence.RESP = ind_confidence, Ind_Decision.OnsetTime = ind_decision_onset,
         Ind_Decision.OffsetTime = ind_decision_offset, Ind_Decision.RT = ind_decision_rt,
         Ind_Decision.RESP = ind_decision) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, ApmItemNum, Stimulus.CRESP, 
         Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime,
         Ind_Confidence.RESP, Ind_Confidence.RT, Ind_Confidence.OnsetTime, Ind_Confidence.OffsetTime,
         Ind_Decision.RESP, Ind_Decision.RT, Ind_Decision.OnsetTime, Ind_Decision.OffsetTime) %>%
  mutate(version = "b2")

# combine data from a1 and b1 into a single df 
rapm_ind <- rbind(surveys_qrt$RAPM_individual_A1, surveys_qrt$RAPM_individual_B1,
                  surveys_java$RAPM_individual_A2, surveys_java$RAPM_individual_B2)

# Data Screening ----------------------------------------------------------
# identified duplicated records for most participants created when parsing

rapm_ind <- rapm_ind %>% filter(!is.na(ApmItemNum))


# Check participants appear correct number of times =======================
# ISSUE 1: Identified that the following uids appear twice
#   "18041210_1_d2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_1_g2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_2_d1"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_2_g1"   = all responses legit, P from 1pm session incorrectly assigned same uid

# SOLUTION 1: change uids for these Ps to reflect the correct start time
# new uid for ResponseID = R_0JwbMy4HDQljoQR becomes 18041213_1_d2
# new uid for ResponseID = R_1MPeDnhE9yES2IB becomes 18041213_1_g2
# new uid for ResponseID = R_4UPYMp1ohvpPNPL becomes 18041213_1_d1
# new uid for ResponseID = R_7R4dstNm0GW6oxn becomes 18041213_1_g1

# ISSUE 2: Identified that uid == "18042010_1_d2" has all NAs for Q6 (Itemnum == 11)
# SOLUTION 2. 
# Remove this Q from their responses so they have responses for 17 Qs only

rapm_ind <- rapm_ind %>% mutate(uid = ifelse(ResponseID == "R_0JwbMy4HDQljoQR", "18041213_1_d2", uid),
                                uid = ifelse(ResponseID == "R_1MPeDnhE9yES2IB", "18041213_1_g2", uid),
                                uid = ifelse(ResponseID == "R_4UPYMp1ohvpPNPL", "18041213_2_d1", uid),
                                uid = ifelse(ResponseID == "R_7R4dstNm0GW6oxn", "18041213_2_g1", uid))

n_questions <- 18 # number of questions in this test

x <- table(rapm_ind$uid)

if (any(x != n_questions)) {
  print("Participants not appearing the correct number of times:")
  x[x != n_questions]
} else {
  print("All good! :)")
}


# Check for missing values ================================================
# create vector to select only uid and participants responses
x <- rapm_ind %>%
  select(uid, ApmItemNum, Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Confidence.RESP, Ind_Decision.RESP)

# by column
col_miss <- sapply(x, function(col) {
  col %>%
    is.na() %>%
    sum()
})

# by row
row_miss <- apply(x, 1, function(row) {
  row %>%
    is.na() %>%
    any()
})

check <- if (any(row_miss)) {
  print("missing values for participant:")
  x[row_miss, ] # to print uid if there is a missing value
}  else {
  print("no missing values")
}

# visually check that there are no missing values per user per variable
x %>%
  group_by(uid) %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  gather(key, val, -uid) %>% 
  ggplot(aes(key, uid, fill = factor(val))) +
  geom_tile() +
  scale_fill_brewer(palette = "OrRd") +
  labs(title = "Number of missing values per user, per variable", x = NULL, y = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Check whether vector contains acceptable values () ------------------------
# uid == "17102710_1_d2" entered "70y" for one confidence rating. Change to "70"
rapm_ind <- rapm_ind %>% mutate(Ind_Confidence.RESP = ifelse(uid == "17102710_1_d2" & ApmItemNum == "7", "60", Ind_Confidence.RESP))

rapm_ind <- rapm_ind %>% mutate(Ind_Stimulus.RESP = as.numeric(Ind_Stimulus.RESP),
                                Ind_Confidence.RESP = as.numeric(Ind_Confidence.RESP),
                                Ind_Decision.RESP = tolower(Ind_Decision.RESP))

check_values <- function(x, num_min, num_max = NULL) {
  if (is.null(num_max)) {
    any_wrong <- !all(x %in% num_min)
  } else {
    any_wrong <- any(x < num_min | x > num_max, na.rm = TRUE)
  }
  
  if (any_wrong) {
    print("Unacceptable value present in this variable.")
  } else {
    print("All good! :)")
  }
}

check_values(rapm_ind$Ind_Stimulus.RESP, 1, 8)
check_values(rapm_ind$Ind_Confidence.RESP, 0, 100)
check_values(rapm_ind$Ind_Decision.RESP, c("y", "n"))

# Calculate variables
# some Stimulus.CRESP values were missing
rapm_ind <- rapm_ind %>%  
  mutate(Stimulus.CRESP = ifelse(ApmItemNum %in% c("13", "15"), "2", 
                          ifelse(ApmItemNum %in% c("5", "19", "35"), "3",
                          ifelse(ApmItemNum %in% c("31"), "4",
                          ifelse(ApmItemNum %in% c("1", "11", "33"), "5",
                          ifelse(ApmItemNum %in% c("7", "17", "23", "29"), "6",
                          ifelse(ApmItemNum %in% c("3", "25", "27"), "7",
                          ifelse(ApmItemNum %in% c("9", "21"), "8", Stimulus.CRESP))))))))


# add Stimulus.ACC column (Stimulus.RESP == Stimulus.CRESP)
rapm_ind <- rapm_ind %>%
  group_by(uid, ApmItemNum) %>%
  mutate(Ind_Stimulus.ACC = Ind_Stimulus.RESP == Stimulus.CRESP) %>%
  ungroup()

# Create grouping variables -----------------------------------------------
# group == date + group
# member == g == 1, d == 2
rapm_ind <- rapm_ind %>% mutate(member = ifelse(grepl("g[1-2]", uid), "1", 
                                         ifelse(grepl("d[1-2]", uid), "2", 
                                         ifelse(grepl("e1", uid), "1", "2"))),
                                recruit = ifelse(grepl("[dg]", uid), "s", "e"),
                                group = gsub("_[a-z][12]", "", uid))



# discovered that uid == "17100609_2_g1" does not have a teammate
# Not sure where their data went? All participants were in teams
# remove for now and investigate later
check <- rapm_ind %>% filter(uid == "17100609_2_g1")


# Compute all variables ------------------------------------------------
# calculate data for control analyses (accuracy, confidence, decision and RTs)
rapm_ind <- rapm_ind %>%
  mutate(
    Ind_Confidence.RESP = as.numeric(Ind_Confidence.RESP),
         Ind_Stimulus.OffsetTime = as.numeric(Ind_Stimulus.OffsetTime),
         Ind_Stimulus.OnsetTime = as.numeric(Ind_Stimulus.OnsetTime),
         Ind_Confidence.OffsetTime = as.numeric(Ind_Confidence.OffsetTime),
         Ind_Confidence.OnsetTime = as.numeric(Ind_Confidence.OnsetTime),
         Ind_Decision.OffsetTime = as.numeric(Ind_Decision.OffsetTime),
         Ind_Decision.OnsetTime = as.numeric(Ind_Decision.OnsetTime),
         Ind_Decision.RESP = tolower(Ind_Decision.RESP))


rapm_ind.uid <- rapm_ind %>%
  group_by(uid) %>%
  summarise(
    group = group[1],
    rapm_ind.acc = mean(Ind_Stimulus.ACC) * 100,
    rapm_ind.conf = mean(Ind_Confidence.RESP),
    rapm_ind.dec = mean(Ind_Decision.RESP == "y") * 100,
    rapm_ind.rec = 100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 0, na.rm = TRUE) 
    / mean(Ind_Stimulus.ACC == 0, na.rm = TRUE))
# rapm_ind.comp = (100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 1, na.rm = TRUE)) + 
#  (100 * mean(Ind_Decision.RESP == "n" & Ind_Stimulus.ACC == 0, na.rm = TRUE)) / 30,
# rapm_ind.optim = (100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 1, na.rm = TRUE)) 
# / 30,
# rapm_ind.hes = 100 * mean(Ind_Decision.RESP == "n" & Ind_Stimulus.ACC == 1, na.rm = TRUE) 
# / mean(Ind_Stimulus.ACC == 1, na.rm = TRUE))



# Reliability ----------------------------------------------------
# # Accuracy 
# x <-  rapm_ind %>%
#   select(uid, ApmItemNum, Ind_Stimulus.ACC) %>%
#   mutate(Ind_Stimulus.ACC = ifelse(Ind_Stimulus.ACC == TRUE, 1, 0)) %>%
#   spread(ApmItemNum, Ind_Stimulus.ACC) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Confidence
# x <- rapm_ind %>%
#   select(uid, ApmItemNum, Ind_Confidence.RESP) %>%
#   spread(ApmItemNum, Ind_Confidence.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Decisiveness
# x <- rapm_ind %>%
#   select(uid, ApmItemNum, Ind_Decision.RESP)
# 
# x[x == "y"] <- 1
# x[x == "n"] <- 0
# 
# x <- transform(x, Ind_Decision.RESP = as.numeric(Ind_Decision.RESP))
# 
# x <- x %>%
#   spread(ApmItemNum, Ind_Decision.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Recklessness
# x <- rapm_ind %>%
#   select(uid, ApmItemNum, Ind_Stimulus.ACC, Ind_Decision.RESP) %>%
#   filter(Ind_Stimulus.ACC == 0) %>%
#   select(-Ind_Stimulus.ACC)
# 
# x[x == "y"] <- 1
# x[x == "n"] <- 0
# 
# x <- transform(x, Ind_Decision.RESP = as.numeric(Ind_Decision.RESP))
# 
# odd1 <- c(1, 5, 9, 13, 17, 21, 25, 29, 33)
# 
# odd <- filter(x, ApmItemNum %in% odd1)
# 
# even1 <- c(3, 7, 11, 15, 19, 23, 27, 31, 35)
# 
# even <- filter(x, ApmItemNum %in% even1)
# 
# even <- even %>%
#   group_by(uid) %>%
#   summarise(even = sum(Ind_Decision.RESP, na.rm = TRUE))
# 
# odd <- odd %>%
#   group_by(uid) %>%
#   summarise(odd = sum(Ind_Decision.RESP, na.rm = TRUE))
# 
# x <- merge(odd, even) %>%
#   mutate(total = odd + even) %>%
#   ungroup(uid) %>%
#   select(-uid)
# 
# # write.csv(x, "RAPM_splithalf.csv")
# 
# cor.test(x$odd, x$even)
# #
# # use this website to correct using the spearman-brown prophecy formula
# # https://www.cedu.niu.edu/~walker/calculators/sbpf.asp



# RAPM_team -----------------------------------------------
surveys_qrt$RAPM_team_A1 <- as.data.frame(surveys_qrt$RAPM_team_A1 %>%
  select(ResponseID, StartDate:Finished, uid, ApmItemNum, Grp_Stimulus.CRESP, 
         Grp_Stimulus.RESP, Grp_Stimulus.RT, Grp_Stimulus.OnsetTime, 
         Grp_Stimulus.OffsetTime, Grp_Confidence.RESP, Grp_Confidence.OnsetTime, 
         Grp_Confidence.OffsetTime, Grp_Decision.RESP, Grp_Decision.RT, 
         Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
  rename(Stimulus.CRESP = Grp_Stimulus.CRESP) %>%
  mutate(version = "a1") %>%
  group_by(uid, ApmItemNum) %>%
  mutate(Grp_Confidence.OffsetTime = as.numeric(Grp_Confidence.OffsetTime),
         Grp_Confidence.OnsetTime = as.numeric(Grp_Confidence.OnsetTime),
         Grp_Confidence.RT = Grp_Confidence.OffsetTime - Grp_Confidence.OnsetTime))

surveys_qrt$RAPM_team_B1 <- as.data.frame(surveys_qrt$RAPM_team_B1 %>%
  select(ResponseID, StartDate:Finished, uid, ApmItemNum, Grp_Stimulus.CRESP, 
         Grp_Stimulus.RESP, Grp_Stimulus.RT, Grp_Stimulus.OnsetTime, 
         Grp_Stimulus.OffsetTime, Grp_Confidence.RESP, Grp_Confidence.OnsetTime, 
         Grp_Confidence.OffsetTime, Grp_Decision.RESP, Grp_Decision.RT, 
         Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
  rename(Stimulus.CRESP = Grp_Stimulus.CRESP) %>%
  mutate(version = "b1") %>%
  group_by(uid, ApmItemNum) %>%
  mutate(Grp_Confidence.OffsetTime = as.numeric(Grp_Confidence.OffsetTime),
         Grp_Confidence.OnsetTime = as.numeric(Grp_Confidence.OnsetTime),
         Grp_Confidence.RT = Grp_Confidence.OffsetTime - Grp_Confidence.OnsetTime))

surveys_java$RAPM_team_A2 <- surveys_java$RAPM_team_A2 %>%
  rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, ApmItemNum = itemnum, 
         Grp_Stimulus.OnsetTime = grp_stimulus_onset,
         Grp_Stimulus.OffsetTime = grp_stimulus_offset, Grp_Stimulus.RT = grp_stimulus_rt,
         Grp_Stimulus.RESP = grp_response, Grp_Confidence.OnsetTime = grp_confidence_onset,
         Grp_Confidence.OffsetTime = grp_confidence_offset, Grp_Confidence.RT = grp_confidence_rt,
         Grp_Confidence.RESP = grp_confidence, Grp_Decision.OnsetTime = grp_decision_onset,
         Grp_Decision.OffsetTime = grp_decision_offset, Grp_Decision.RT = grp_decision_rt,
         Grp_Decision.RESP = grp_decision) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, ApmItemNum, Stimulus.CRESP, 
         Grp_Stimulus.RESP, Grp_Stimulus.RT, Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime,
         Grp_Confidence.RESP, Grp_Confidence.RT, Grp_Confidence.OnsetTime, Grp_Confidence.OffsetTime,
         Grp_Decision.RESP, Grp_Decision.RT, Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
  mutate(version = "a2")

surveys_java$RAPM_team_B2 <- surveys_java$RAPM_team_B2 %>%
  rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, ApmItemNum = itemnum, 
         Grp_Stimulus.OnsetTime = grp_stimulus_onset,
         Grp_Stimulus.OffsetTime = grp_stimulus_offset, Grp_Stimulus.RT = grp_stimulus_rt,
         Grp_Stimulus.RESP = grp_response, Grp_Confidence.OnsetTime = grp_confidence_onset,
         Grp_Confidence.OffsetTime = grp_confidence_offset, Grp_Confidence.RT = grp_confidence_rt,
         Grp_Confidence.RESP = grp_confidence, Grp_Decision.OnsetTime = grp_decision_onset,
         Grp_Decision.OffsetTime = grp_decision_offset, Grp_Decision.RT = grp_decision_rt,
         Grp_Decision.RESP = grp_decision) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, ApmItemNum, Stimulus.CRESP, 
         Grp_Stimulus.RESP, Grp_Stimulus.RT, Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime,
         Grp_Confidence.RESP, Grp_Confidence.RT, Grp_Confidence.OnsetTime, Grp_Confidence.OffsetTime,
         Grp_Decision.RESP, Grp_Decision.RT, Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
  mutate(version = "b2")

# combine data from a1 and b1 into a single df 
rapm_team <- rbind(surveys_qrt$RAPM_team_A1, surveys_qrt$RAPM_team_B1,
                   surveys_java$RAPM_team_A2, surveys_java$RAPM_team_B2)

# Data Screening ----------------------------------------------------------
# identified duplicated records for most participants created when parsing
rapm_team <- rapm_team %>% filter(!is.na(ApmItemNum))

# Check participants appear correct number of times =======================
# ISSUE 1: Identified that the following uids appear twice
#   "18041210_1_d2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_1_g2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_2_d1"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_2_g1"   = all responses legit, P from 1pm session incorrectly assigned same uid

# SOLUTION 1: change uids for these Ps to reflect the correct start time
# new uid for ResponseID = R_aXXnQzecGqoincN becomes 18041213_1_d2
# new uid for ResponseID = R_bNORotyubVHr8Bn becomes 18041213_1_g2
# new uid for ResponseID = R_1SxJ0lXtVMVE91P becomes 18041213_1_d1
# new uid for ResponseID = R_eX2DQL2fHvclLil becomes 18041213_1_g1

# ISSUE 2: 
# Identified that the browser crashed for uid == "18041210_2_d1" 
# they completed Q1-5 in attempt 1 ResponseID == "R_eD38KVfogEUZR0V"
# and Q6-18 in attempt 2 ResponseID == "R_bJFkt0OJntHDJk1"

# Identified that the browser also crashed for uid == "18041210_2_g1" 
# they completed Q1-9 in attempt 1 ResponseID == "R_cMAxMyLfBXNKQbH"
# and Q10-18 in attempt 2 ResponseID == "R_eE6yXvI2qkHfSU5"

# Identified that the browser also crashed for uid == "18062010_1_e2" 
# they completed Q1-11 in attempt 1 ResponseID == "R_9G0HlLYN0Fc1Bcx"
# and Q12-18 in attempt 2 ResponseID == "R_d4jydZbeCaiAYex"

# Identified that the browser also crashed for uid == "18062010_2_e2" 
# they completed Q1-15 in attempt 1 ResponseID == "R_004LR4oxNC3xDgh"
# and Q16-18 in attempt 2 ResponseID == "R_2oeQFyAdtWYkk7j"

# SOLUTION 2. 
# join attempt 1 responses with attempt 2 responses 

# Which variables will I replace?
vars <- c("Grp_Stimulus.RESP", "Grp_Stimulus.RT", "Grp_Stimulus.OnsetTime", 
          "Grp_Stimulus.OffsetTime", "Grp_Confidence.RESP", "Grp_Confidence.RT", 
          "Grp_Confidence.OnsetTime", "Grp_Confidence.OffsetTime", "Grp_Decision.RESP",
          "Grp_Decision.RT", "Grp_Decision.OffsetTime", "Grp_Decision.OnsetTime")

# For uid == "18041210_2_d1" get Attempt 1 responses
x <- rapm_team %>% filter(ResponseID == "R_eD38KVfogEUZR0V")

# x <- arrange(x, Itemnum)  # arrange by trial number

x <- x[1:5, vars]  # Take rows 1 to 3 (corresponding to answered questions)
# and variables of interest

# if item numbers are not ordered (e.g., 1-18) need to specify
itemnum <- c("2", "4", "6", "8", "10")

# Insert data from x into relevant part of df
rapm_team[rapm_team$ResponseID == "R_bJFkt0OJntHDJk1" & rapm_team$ApmItemNum %in% itemnum, vars] <- x


# For uid == "18041210_2_g1" get Attempt 1 responses
x <- rapm_team %>% filter(ResponseID == "R_cMAxMyLfBXNKQbH")

# x <- arrange(x, Itemnum)  # arrange by trial number

x <- x[1:9, vars]  # Take rows 1 to 3 (corresponding to answered questions)
# and variables of interest

# if item numbers are not ordered (e.g., 1-18) need to specify
itemnum <- c("2", "4", "6", "8", "10", "12", "14", "16", "18")

# Insert data from x into relevant part of df
rapm_team[rapm_team$ResponseID == "R_eE6yXvI2qkHfSU5" & rapm_team$ApmItemNum %in% itemnum, vars] <- x


# For uid == "18062010_1_e2" get Attempt 1 responses
x <- rapm_team %>% filter(ResponseID == "R_9G0HlLYN0Fc1Bcx")

# x <- arrange(x, Itemnum)  # arrange by trial number

x <- x[1:11, vars]  # Take rows 1 to 3 (corresponding to answered questions)
# and variables of interest

# if item numbers are not ordered (e.g., 1-18) need to specify
itemnum <- c("2", "4", "6", "8", "10", "12", "14", "16", "18", "20", "22")

# Insert data from x into relevant part of df
rapm_team[rapm_team$ResponseID == "R_d4jydZbeCaiAYex" & rapm_team$ApmItemNum %in% itemnum, vars] <- x


# For uid == "18062010_2_e2" get Attempt 1 responses
x <- rapm_team %>% filter(ResponseID == "R_004LR4oxNC3xDgh")

# x <- arrange(x, Itemnum)  # arrange by trial number

x <- x[1:15, vars]  # Take rows 1 to 3 (corresponding to answered questions)
# and variables of interest

# if item numbers are not ordered (e.g., 1-18) need to specify
itemnum <- c("2", "4", "6", "8", "10", "12", "14", "16", "18", "20", "22", "24", "26", "28", "30")

# Insert data from x into relevant part of df
rapm_team[rapm_team$ResponseID == "R_2oeQFyAdtWYkk7j" & rapm_team$ApmItemNum %in% itemnum, vars] <- x


rapm_team <- rapm_team %>% mutate(uid = ifelse(ResponseID == "R_aXXnQzecGqoincN", "18041213_1_d2", uid),
                                  uid = ifelse(ResponseID == "R_bNORotyubVHr8Bn", "18041213_1_g2", uid),
                                  uid = ifelse(ResponseID == "R_1SxJ0lXtVMVE91P", "18041213_2_d1", uid),
                                  uid = ifelse(ResponseID == "R_eX2DQL2fHvclLil", "18041213_2_g1", uid)) %>%
  filter(ResponseID != "R_eD38KVfogEUZR0V", ResponseID != "R_cMAxMyLfBXNKQbH", 
         ResponseID != "R_9G0HlLYN0Fc1Bcx", ResponseID != "R_004LR4oxNC3xDgh")

n_questions <- 18 # number of questions in this test

x <- table(rapm_team$uid)

if (any(x != n_questions)) {
  print("Participants not appearing the correct number of times:")
  x[x != n_questions]
} else {
  print("All good! :)")
}

# uid == 18041210_2_g1 and 18041210_2_d1 completed RAPM_team individually
# remove from data
rapm_team <- rapm_team %>% filter(uid != "18041210_2_g1", uid != "18041210_2_d1")


# Check for missing values ================================================
# create vector to select only uid and participants responses
x <- rapm_team %>%
  select(uid, ApmItemNum, Grp_Stimulus.RESP, Grp_Stimulus.RT, Grp_Stimulus.OnsetTime, 
         Grp_Stimulus.OffsetTime, Grp_Confidence.RESP, Grp_Decision.RESP)

# by column
col_miss <- sapply(x, function(col) {
  col %>%
    is.na() %>%
    sum()
})

# by row
row_miss <- apply(x, 1, function(row) {
  row %>%
    is.na() %>%
    any()
})

check <- if (any(row_miss)) {
  print("missing values for participant:")
  x[row_miss, ] # to print uid if there is a missing value
}  else {
  print("no missing values")
}

# visually check that there are no missing values per user per variable
x %>%
  group_by(uid) %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  gather(key, val, -uid) %>% 
  ggplot(aes(key, uid, fill = factor(val))) +
  geom_tile() +
  scale_fill_brewer(palette = "OrRd") +
  labs(title = "Number of missing values per user, per variable", x = NULL, y = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Check whether vector contains acceptable values () ------------------------
x <- rapm_team %>% mutate(Grp_Stimulus.RESP = as.numeric(Grp_Stimulus.RESP),
                                 Grp_Confidence.RESP = as.numeric(Grp_Confidence.RESP),
                                 Grp_Decision.RESP = tolower(Grp_Decision.RESP))

check_values <- function(x, num_min, num_max = NULL) {
  if (is.null(num_max)) {
    any_wrong <- !all(x %in% num_min)
  } else {
    any_wrong <- any(x < num_min | x > num_max, na.rm = TRUE)
  }
  
  if (any_wrong) {
    print("Unacceptable value present in this variable.")
  } else {
    print("All good! :)")
  }
}

check_values(x$Grp_Stimulus.RESP, 1, 8)
check_values(x$Grp_Confidence.RESP, 0, 100)
check_values(x$Grp_Decision.RESP, c("y", "n"))

# Check teams are giving same response and decision =======================
# Create grouping variables -----------------------------------------------
# group == date + group
# member == g == 1, d == 2
rapm_team <- rapm_team %>% mutate(member = ifelse(grepl("g[1-2]", uid), "1", 
                                           ifelse(grepl("d[1-2]", uid), "2", 
                                           ifelse(grepl("e1", uid), "1", "2"))),
                                  recruit = ifelse(grepl("[dg]", uid), "s", "e"),
                                  group = gsub("_[a-z][12]", "", uid))

# discovered that uid == "17100609_2_g1" does not have a teammate
# Not sure where their data went? All participants were in teams
# remove for now and investigate later
rapm_team <- rapm_team %>% filter(uid != "17100609_2_g1")

# Decisions
# in cases where member's group decisions differ change to missing
# Justification: If decision is different for members it is impossible to
# determine which decision was agreed upon by members. As decision was provided last
# other data for same item remains valid, if matching.

x <- rapm_team %>% select(group, member, ApmItemNum, Grp_Decision.RESP) %>% 
  spread(member, Grp_Decision.RESP) %>%
  group_by(group, ApmItemNum) %>%
  mutate(dif = `1` != `2`) %>%
  filter(dif == TRUE)

# prints out number of times that group decisions differed between members
cat("number of times that group decision differed between members:", nrow(x))

for (i in seq(nrow(x))){
  group <- x$group[i]
  ApmItemNum <- x$ApmItemNum[i]
  rapm_team[rapm_team$group == group & rapm_team$ApmItemNum == ApmItemNum, 
            c("Grp_Decision.RESP", "Grp_Decision.OnsetTime", 
              "Grp_Decision.OffsetTime", "Grp_Decision.RT")] <- NA
}

# Accuracy
# in cases where either is different change to missing for all variables 
# (Grp_Stimulus.ACC, Grp_Confidence.RESP, Grp_Decision.RESP)
# Justification: If accuracy differs for members it is impossible to
# determine which response was agreed upon by members. Confidence and decision 
# were contingent upon accuracy, as it was provided first.

x <- rapm_team %>% select(group, member, ApmItemNum, Grp_Stimulus.RESP) %>% 
  spread(member, Grp_Stimulus.RESP) %>%
  group_by(group, ApmItemNum) %>% 
  mutate(dif = `1` != `2`) %>%
  filter(dif == TRUE)

# prints out number of times that group accuracy differed between members
cat("number of times that group accuracy differed between members:", nrow(x))

for (i in seq(nrow(x))){
  group <- x$group[i]
  ApmItemNum <- x$ApmItemNum[i]
  rapm_team[rapm_team$group == group & rapm_team$ApmItemNum == ApmItemNum, 
            c("Grp_Stimulus.RESP", "Grp_Stimulus.OnsetTime", "Grp_Stimulus.OffsetTime",
              "Grp_Stimulus.RT", "Grp_Confidence.RESP", "Grp_Confidence.OnsetTime", 
              "Grp_Confidence.OffsetTime", "Grp_Confidence.RT", "Grp_Decision.RESP", 
              "Grp_Decision.OnsetTime", "Grp_Decision.OffsetTime", "Grp_Decision.RT")] <- NA
}

# Calculate variables
# some Stimulus.CRESP values were missing
rapm_team <- rapm_team %>%  
  mutate(Stimulus.CRESP = ifelse(ApmItemNum %in% c("2", "6", "8", "14", "34"), "1", 
                          ifelse(ApmItemNum %in% c("26", "36"), "2",
                          ifelse(ApmItemNum == "24", "3",
                          ifelse(ApmItemNum %in% c("4", "10", "16"), "4",
                          ifelse(ApmItemNum %in% c("28", "30"), "5",
                          ifelse(ApmItemNum == "12", "6",
                          ifelse(ApmItemNum %in% c("18", "22"), "7",
                          ifelse(ApmItemNum %in% c("20", "32"), "8", Stimulus.CRESP)))))))))



# Compute all variables ------------------------------------------------
# add Stimulus.ACC column (Stimulus.RESP == Stimulus.CRESP)
rapm_team <- rapm_team %>% 
  group_by(uid, ApmItemNum) %>%
  mutate(Grp_Stimulus.ACC = Grp_Stimulus.RESP == Stimulus.CRESP) %>%
  ungroup()


# prepare data for variable calculation
rapm_team <- rapm_team %>%
  mutate(Grp_Confidence.RESP = as.numeric(Grp_Confidence.RESP),
         Grp_Stimulus.OffsetTime = as.numeric(Grp_Stimulus.OffsetTime),
         Grp_Stimulus.OnsetTime = as.numeric(Grp_Stimulus.OnsetTime),
         Grp_Confidence.OffsetTime = as.numeric(Grp_Confidence.OffsetTime),
         Grp_Confidence.OnsetTime = as.numeric(Grp_Confidence.OnsetTime),
         Grp_Decision.OffsetTime = as.numeric(Grp_Decision.OffsetTime),
         Grp_Decision.OnsetTime = as.numeric(Grp_Decision.OnsetTime),
         Grp_Decision.RESP = tolower(Grp_Decision.RESP))


rapm_team.uid <- rapm_team %>%
  group_by(uid) %>%
  summarise(
    group = group[1],
    rapm_grp.acc = mean(Grp_Stimulus.ACC, na.rm = TRUE) * 100,
    rapm_grp.conf = mean(Grp_Confidence.RESP, na.rm = TRUE),
    rapm_grp.dec = mean(Grp_Decision.RESP == "y", na.rm = TRUE) * 100,
    rapm_grp.rec = 100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 0, na.rm = TRUE) 
    / mean(Grp_Stimulus.ACC == 0, na.rm = TRUE))
# rapm_grp.comp = (100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 1, na.rm = TRUE)) + 
#  (100 * mean(Grp_Decision.RESP == "n" & Grp_Stimulus.ACC == 0, na.rm = TRUE)) / 30,
# rapm_grp.optim = (100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 1, na.rm = TRUE)) 
# / 30,
# rapm_grp.hes = 100 * mean(Grp_Decision.RESP == "n" & Grp_Stimulus.ACC == 1, na.rm = TRUE) 
# / mean(Grp_Stimulus.ACC == 1, na.rm = TRUE))


# # data for control analyses (accuracy, confidence, decision and RTs)
# uid <- rapm_team %>%
#   group_by(group, ApmItemNum) %>%
#   mutate(Grp_Stimulus.RT = min(Grp_Stimulus.OffsetTime) - max(Grp_Stimulus.OnsetTime),
#          Grp_Confidence.RESP = mean(Grp_Confidence.RESP, na.rm = TRUE),
#          Grp_Confidence.RT = min(Grp_Confidence.OffsetTime) - max(Grp_Confidence.OnsetTime),
#          Grp_Decision.RT = min(Grp_Decision.OffsetTime) - max(Grp_Decision.OnsetTime))
# 
# d_rapm_grp <- uid %>%
#   group_by(group, ApmItemNum)
#   summarise(Grp_Stimulus.ACC = Grp_Stimulus.ACC[1],
#             Grp_Stimulus.RESP = Grp_Stimulus.RESP[1],
#             Grp_Stimulus.RT = Grp_Stimulus.RT[1],
#             Grp_Confidence.RESP = Grp_Confidence.RESP[1],
#             Grp_Confidence.RT = Grp_Confidence.RT[1],
#             Grp_Decision.RESP = Grp_Decision.RESP[1],
#             Grp_Decision.RT = Grp_Decision.RT[1]
#     ) 
# write_csv("data/control/rapm_team.csv")

  

# Reliability ----------------------------------------------------
# #  Accuracy
#   x <-  rapm_team %>%
#     select(uid, ApmItemNum, Grp_Stimulus.ACC) %>%
#     mutate(Grp_Stimulus.ACC = ifelse(Grp_Stimulus.ACC == TRUE, 1, 0)) %>%
#     spread(ApmItemNum, Grp_Stimulus.ACC) %>%
#     select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
#   # Confidence
#   x <- rapm_team %>%
#     select(uid, ApmItemNum, Grp_Confidence.RESP) %>%
#     spread(ApmItemNum, Grp_Confidence.RESP) %>%
#     select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
#   # Decisiveness
#   x <- rapm_team %>%
#     select(uid, ApmItemNum, Grp_Decision.RESP)
# 
#   x[x == "y"] <- 1
#   x[x == "n"] <- 0
# 
#   x <- transform(x, Grp_Decision.RESP = as.numeric(Grp_Decision.RESP))
# 
#   x <- x %>%
#     spread(ApmItemNum, Grp_Decision.RESP) %>%
#     select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
#   # Recklessness
#   x <- rapm_team %>%
#     select(uid, ApmItemNum, Grp_Stimulus.ACC, Grp_Decision.RESP) %>%
#     filter(Grp_Stimulus.ACC == 0) %>%
#     select(-Grp_Stimulus.ACC)
# 
#   x[x == "y"] <- 1
#   x[x == "n"] <- 0
# 
#   x <- transform(x, Grp_Decision.RESP = as.numeric(Grp_Decision.RESP))
# 
#   odd1 <- c(2, 6, 10, 14, 18, 22, 26, 30, 34)
# 
#   odd <- filter(x, ApmItemNum %in% odd1)
# 
#   even1 <- c(4, 8, 12, 16, 20, 24, 28, 32, 36)
# 
#   even <- filter(x, ApmItemNum %in% even1)
# 
#   even <- even %>%
#     group_by(uid) %>%
#     summarise(even = sum(Grp_Decision.RESP, na.rm = TRUE))
# 
#   odd <- odd %>%
#     group_by(uid) %>%
#     summarise(odd = sum(Grp_Decision.RESP, na.rm = TRUE))
# 
#   x <- merge(odd, even) %>%
#     mutate(total = odd + even) %>%
#     ungroup(uid) %>%
#     select(-uid)
# 
# cor.test(x$odd, x$even)
# 

