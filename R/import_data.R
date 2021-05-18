#' *********************************************************************************
#' ************************************* Script 1 **********************************
#' *********************************************************************************
#'  
#'  This script imports the raw survey data for Matt Blanchard's 2nd PhD study
#'  investigating collective intelligence and cleans uids in surveys_qrt
#'  and surveys_java

if (file.exists("data/rds/surveys_qrt_clean.rds")) {
  
  library(tidyverse)
  surveys_qrt <- readRDS("data/rds/surveys_qrt_clean.rds")
  surveys_java <- readRDS("data/rds/surveys_java_clean.rds")
  surveys_multi <- readRDS("data/rds/surveys_multi_raw.rds")
  surveys_r2f <- readRDS("data/rds/surveys_r2f_clean.rds")
  surveys_read <- readRDS("data/rds/surveys_read_clean.rds")
  
} else {

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

# Load qrt survey data (A1 and B1)
  if (file.exists("data/rds/surveys_qrt_raw.rds")) {
    surveys_qrt <- readRDS("data/rds/surveys_qrt_raw.rds")

      } else {
    # if using office computer
    # survey_dir <- "/Users/Matty/Dropbox (Sydney Uni)/R_projects/S2_CI/data/qrt" # dir for office computer
    # survey_paths <- file.path(survey_dir, list.files(survey_dir))
    # names(survey_paths) <- survey_paths %>% str_replace("/Users/Matty/Dropbox \\(Sydney Uni\\)/R_projects/S2_CI/data/qrt/", "") %>% str_replace("\\.csv", "")
    
    # if using laptop
    survey_dir <- "/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/data/qrt"
    survey_paths <- file.path(survey_dir, list.files(survey_dir))
    names(survey_paths) <- survey_paths %>% str_replace("/Users/blattymanchard/Dropbox \\(Sydney Uni\\)/R_projects/S2_CI/data/qrt/", "") %>% str_replace("\\.csv", "")
    # try using q_read instead of parse_qrt_csv
    surveys_qrt <- map(survey_paths, parse_qrt_csv)
    saveRDS(surveys_qrt, "data/rds/surveys_qrt_raw.rds")
}
  
# Load java survey data (A2 and B2)
  if (file.exists("data/rds/surveys_java_raw.rds")) {
    surveys_java <- readRDS("data/rds/surveys_java_raw.rds")
    
  } else {
    # If using office computer
    # survey_dir <- "/Users/Matty/Dropbox (Sydney Uni)/R_projects/S2_CI/data/java"
    # survey_paths <- file.path(survey_dir, list.files(survey_dir))
    # names(survey_paths) <- survey_paths %>% str_replace("/Users/Matty/Dropbox \\(Sydney Uni\\)/R_projects/S2_CI/data/java/", "") %>% str_replace("\\.csv", "")
    
    # If using laptop
    survey_dir <- "/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/data/java"
    survey_paths <- file.path(survey_dir, list.files(survey_dir))
    names(survey_paths) <- survey_paths %>% str_replace("/Users/blattymanchard/Dropbox \\(Sydney Uni\\)/R_projects/S2_CI/data/java/", "") %>% str_replace("\\.csv", "")
# try using q_read instead of parse_qrt_csv
surveys_java <- map(survey_paths, parse_csv)
saveRDS(surveys_java, "data/rds/surveys_java_raw.rds")
  }
  
# Load R2F java data
  if (file.exists("data/rds/surveys_r2f_raw.rds")) {
    surveys_r2f <- readRDS("data/rds/surveys_r2f_raw.rds")
    
  } else {
survey_dir <- "data/java_R2F"
survey_paths <- file.path(survey_dir, list.files(survey_dir))
names(survey_paths) <- survey_paths %>% str_replace("data/java_R2F/", "") %>% str_replace("\\.csv", "")
# try using q_read instead of parse_qrt_csv
surveys_r2f <- map(survey_paths, parse_r2f)
saveRDS(surveys_r2f, "data/rds/surveys_r2f_raw.rds")
  }

    
# Load java data with multiple JSON columns (8 participants)
  if (file.exists("data/rds/surveys_multi_raw.rds")) {
    surveys_multi <- readRDS("data/rds/surveys_multi_raw.rds")
  } else {
source("R/read_multi.R")
  }

  # Load java data with multiple JSON columns (8 participants)
  if (file.exists("data/rds/surveys_read_raw.rds")) {
    surveys_read <- readRDS("data/rds/surveys_read_raw.rds")
  } else {
    survey_dir <- "data/read_as_is"
    survey_paths <- file.path(survey_dir, list.files(survey_dir))
    names(survey_paths) <- survey_paths %>% str_replace("data/read_as_is/", "") %>% str_replace("\\.csv", "")
    # try using q_read instead of parse_qrt_csv
    surveys_read <- map(survey_paths, read_csv)
    saveRDS(surveys_read, "data/rds/surveys_read_raw.rds")
  }
  
  
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

# surveys_qrt -------------------------------------------------------------
# Function to count frequency of `uid` in each test
uid_freqs <- function() {
  map(surveys_qrt, select, uid) %>% 
    map(count, uid) %>% 
    map2(names(surveys_qrt), ~ mutate(.x , test = .y)) %>% 
    reduce(full_join)  
}

# Function to plot these frequencies
plot_freqs <- function() {
  library(RColorBrewer)
  colourCount = length(unique(surveys_qrt))
  getPalette = colorRampPalette(brewer.pal(9, "OrRd"))
  
  ggplot(uid_freqs(), aes(x = uid, y = test, fill = factor(n))) +
    geom_tile() +
    scale_fill_manual(values = getPalette(colourCount)) +
    # scale_fill_brewer(palette = "OrRd") +
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

# Identify and remove multiple attempts
uid_freqs() %>% filter(n > 1) %>% arrange(uid)

# check <- uid_freqs() %>% group_by(test) %>% 
#   summarise(max_items = max(n),
#          min_items = min(n),
#          same = min_items == max_items) %>% 
#   filter(same == TRUE)
  

# ... a number of cases of multiple attemps

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

# identified that group == 18081613_2 did not make a genuine attempt so remove all data
# and uid == 17100609_2_g1 does not have a teammate (no idea why their data wasn't recorded)

# SOLUTION: 
# Remove various TEST uids, non-genuine attempt and modify incorrect uids
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
  map(filter, uid != "17100609_2_g1") %>%
  map(filter, uid != "12345") %>%
  map(filter, uid != "999") %>%
  map(filter, uid != "343") %>%
  map(filter, uid != "431") %>%
  map(filter, uid != "") %>% 
  map(filter, uid != "18081613_2_e1") %>% 
  map(filter, uid != "18081613_2_e2")

plot_freqs()

# surveys_java -------------------------------------------------------------
# Function to count frequency of `uid` in each test
uid_freqs <- function() {
  map(surveys_java, select, uid) %>% 
    map(count, uid) %>% 
    map2(names(surveys_java), ~ mutate(.x , test = .y)) %>% 
    reduce(full_join)  
}

# Function to plot these frequencies
plot_freqs <- function() {
  library(RColorBrewer)
  colourCount = length(unique(surveys_java))
  getPalette = colorRampPalette(brewer.pal(9, "OrRd"))
  
  ggplot(uid_freqs(), aes(x = uid, y = test, fill = factor(n))) +
    geom_tile() +
    scale_fill_manual(values = getPalette(colourCount)) +
    # scale_fill_brewer(palette = "OrRd") +
    labs(title = "uid frequency in each survey", x = NULL, y = NULL, fill = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
plot_freqs()

# Initial plot reveals `uid`s that...
# 
#   were not used by participants:
#   - "test[1-4]"
#   - NA
#
# The following will fix these issues.

# Drop `uid` missing values (so other functions can be run)
surveys_java <- map(surveys_java, drop_na, uid)
plot_freqs()

# Remove rows with `uid` containing "TEST"
surveys_java <- map(surveys_java, ~ filter(., !str_detect(uid, "test")))
surveys_java <- map(surveys_java, ~ filter(., !str_detect(uid, "TEST")))
plot_freqs()

# identified some uids were incorrectly recorded across all tasks:
#     "18073109_1_g2"   - should be  "18073109_1_e1"
#     "18073109_1_d2"   - should be  "18073109_1_e2"
#     "18073110_2_g1"   - should be  "18073110_2_e1"
#     "18073110_2_d1"   - should be  "18073110_2_e2"
# 
# Will modify within the scripts that clean each survey separately
# surveys_java <- surveys_java %>%
#   map(mutate, uid = ifelse(uid == "18073109_1_g2", "18073109_1_e1", uid)) %>%
#   map(mutate, uid = ifelse(uid == "18073109_1_d2", "18073109_1_e2", uid)) %>%
#   map(mutate, uid = ifelse(uid == "18073110_2_g1", "18073110_2_e1", uid)) %>%
#   map(mutate, uid = ifelse(uid == "18073110_2_d1", "18073110_2_e2", uid))

# Remove team that made non-genuine attempt
surveys_java <- surveys_java %>% 
map(filter, uid != "18081613_2_e1") %>% 
map(filter, uid != "18081613_2_e2")

# It looks like a few participants made two attempts on running_letters and mini_IPIP
# uid_freqs() %>% filter(str_detect(test, "running_letters")) %>%  filter(n > 15) %>% arrange(uid)
# uid_freqs() %>% filter(str_detect(test, "IPIP")) %>%  filter(n > 20) %>% arrange(uid)

# surveys_multi -------------------------------------------------------------
# for some reason map() is not working on surveys_multi so will need to clean uids manually
# surveys 3, 6:10 work but the others dont
# map(surveys_java, drop_na, uid)
# map(surveys_multi[1], drop_na, uid)
# map(surveys_multi, ~ filter(., !str_detect(uid, "test")))

# surveys_r2f -------------------------------------------------------------
# Function to count frequency of `uid` in each test
uid_freqs <- function() {
  map(surveys_r2f, select, uid) %>% 
    map(count, uid) %>% 
    map2(names(surveys_r2f), ~ mutate(.x , test = .y)) %>% 
    reduce(full_join)  
}

# Function to plot these frequencies
plot_freqs <- function() {
  library(RColorBrewer)
  colourCount = length(unique(surveys_r2f))
  getPalette = colorRampPalette(brewer.pal(9, "OrRd"))
  
  ggplot(uid_freqs(), aes(x = uid, y = test, fill = factor(n))) +
    geom_tile() +
    scale_fill_manual(values = getPalette(colourCount)) +
    # scale_fill_brewer(palette = "OrRd") +
    labs(title = "uid frequency in each survey", x = NULL, y = NULL, fill = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
plot_freqs()

# Initial plot reveals `uid`s that...
# 
#   were not used by participants:
#   - "test"
#   - NA
#
# The following will fix these issues.

# Drop `uid` missing values (so other functions can be run)
surveys_r2f <- map(surveys_r2f, drop_na, uid)
plot_freqs()

# Remove rows with `uid` containing "TEST"
surveys_r2f <- map(surveys_r2f, ~ filter(., !str_detect(uid, "TEST")))
plot_freqs()

# identified some uids were incorrectly recorded across all tasks:
#     "18073109_1_g2"   - should be  "18073109_1_e1"
#     "18073109_1_d2"   - should be  "18073109_1_e2"
#     "18073110_2_g1"   - should be  "18073110_2_e1"
#     "18073110_2_d1"   - should be  "18073110_2_e2"

# will modify within the scripts that clean each survey separately
# surveys_r2f <- surveys_r2f %>%
#   map(mutate, uid = ifelse(uid == "18073109_1_g2", "18073109_1_e1", uid)) %>%
#   map(mutate, uid = ifelse(uid == "18073109_1_d2", "18073109_1_e2", uid)) %>%
#   map(mutate, uid = ifelse(uid == "18073110_2_g1", "18073110_2_e1", uid)) %>%
#   map(mutate, uid = ifelse(uid == "18073110_2_d1", "18073110_2_e2", uid))

# Remove team that made non-genuine attempt
surveys_r2f <- surveys_r2f %>% 
  map(filter, uid != "18081613_2_e1") %>% 
  map(filter, uid != "18081613_2_e2")


# surveys_read - consent and demographics ---------------------------------
# Function to count frequency of `uid` in each test
uid_freqs <- function() {
  map(surveys_read, select, uid) %>% 
    map(count, uid) %>% 
    map2(names(surveys_read), ~ mutate(.x , test = .y)) %>% 
    reduce(full_join)  
}

# Function to plot these frequencies
plot_freqs <- function() {
  library(RColorBrewer)
  colourCount = length(unique(surveys_read))
  getPalette = colorRampPalette(brewer.pal(9, "OrRd"))
  
  ggplot(uid_freqs(), aes(x = uid, y = test, fill = factor(n))) +
    geom_tile() +
    scale_fill_manual(values = getPalette(colourCount)) +
    # scale_fill_brewer(palette = "OrRd") +
    labs(title = "uid frequency in each survey", x = NULL, y = NULL, fill = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
plot_freqs()

# Initial plot reveals `uid`s that...
# 
#   were not used by participants:
#   - "test[1-4]"
#   - "{\"ImportId\":\"uid\"}"
#   - "unique_ID
#
#   appear more than once on same test
#
#   were entered incorrectly (18051410_2_d2 instead of 18051410_2-d2)
#
#
# The following will fix these issues.

# Drop `uid` missing values (so other functions can be run)
# surveys_read <- map(surveys_read, drop_na, uid)
# plot_freqs()

# remove first row containing column names
surveys_read <- map(surveys_read, ~ filter(., !str_detect(uid, "ImportId")))
plot_freqs()

# Remove rows with `uid` containing "TEST"
surveys_read <- map(surveys_read, ~ filter(., !str_detect(uid, "TEST")))
plot_freqs()

# Remove rows with `uid` containing "Unique ID"
surveys_read <- map(surveys_read, ~ filter(., !str_detect(uid, "Unique ID")))
plot_freqs()

# Remove rows with `uid` containing "Unique ID"
surveys_read <- map(surveys_read, ~ filter(., !str_detect(uid, "uid")))
plot_freqs()


# Identify and remove multiple attempts
uid_freqs() %>% filter(n > 1) %>% arrange(uid)

# ... a number of cases of multiple attempts these will be cleaned in a separate script for each measure

# identified that the following uids were incorrectly recorded across surveys:
# surveys_read$demographics_B %>% filter(!str_detect(uid, "[0-9]_[1-2]_[dge]")) %>% select(uid)
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
#     "18073109_1_g2"   - should be  "18073109_1_e1"
#     "18073109_1_d2"   - should be  "18073109_1_e2"
#     "18073110_2_g1"   - should be  "18073110_2_e1"
#     "18073110_2_d1"   - should be  "18073110_2_e2"
#
# also identified that group == 18081613_2 did not make a genuine attempt so remove all data

# SOLUTION: 
# Remove various TEST uids and non-genuine attempt for group == 18081613_2 and modify incorrect uids
surveys_read <- surveys_read %>% 
  map(mutate, uid = ifelse(uid == "18050509_1-e", "18050509_1_e1", uid)) %>%
  map(mutate, uid = ifelse(uid == "18050509_1-e2", "18050509_1_e2", uid)) %>%
  map(mutate, uid = ifelse(uid == "17100609_g1", "17100609_2_g1", uid)) %>%
  map(mutate, uid = ifelse(uid == "17100609_d2", "17100609_1_d2", uid)) %>%
  map(mutate, uid = ifelse(uid == "17100609_g2", "17100609_1_g2", uid)) %>%
  map(mutate, uid = ifelse(uid == "17110112_1_d2", "17110110_1_d2", uid)) %>%
  map(mutate, uid = ifelse(uid == "17040910_2_d1", "18040910_2_d1", uid)) %>%
  map(mutate, uid = ifelse(uid == "17040910_2_g1", "18040910_2_g1", uid)) %>%
  map(mutate, uid = ifelse(uid == "17040910_1_d2", "18040910_1_d2", uid)) %>%
  map(mutate, uid = ifelse(uid == "17040910_1_g2", "18040910_1_g2", uid)) %>%
  map(mutate, uid = ifelse(uid == "18073109_1_g2", "18073109_1_e1", uid)) %>%
  map(mutate, uid = ifelse(uid == "18073109_1_d2", "18073109_1_e2", uid)) %>%
  map(mutate, uid = ifelse(uid == "18073110_2_g1", "18073110_2_e1", uid)) %>%
  map(mutate, uid = ifelse(uid == "18073110_2_d1", "18073110_2_e2", uid)) %>%
  map(filter, uid != "Unique_ID") %>% 
  map(filter, uid != "18081613_2_e1") %>% 
  map(filter, uid != "18081613_2_e2")

plot_freqs()


# Save uid cleaned data as RDS for quick load -----------------------------

saveRDS(surveys_qrt, "data/rds/surveys_qrt_clean.rds")
saveRDS(surveys_java, "data/rds/surveys_java_clean.rds")
# saveRDS(surveys_multi, "data/surveys_multi_raw.rds")
saveRDS(surveys_r2f, "data/rds/surveys_r2f_clean.rds")
saveRDS(surveys_read, "data/rds/surveys_read_clean.rds")

# clear redundant objects from the environment
rm(list = setdiff(ls(), c("surveys_qrt", "surveys_java", "surveys_multi", "surveys_r2f", "surveys_read")))

}
