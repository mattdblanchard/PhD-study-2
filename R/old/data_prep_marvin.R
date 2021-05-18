# ********************************************************
# ******************* DATA PREPARATION *******************
# ********************************************************
# ******************* MATT BLANCHARD *********************
# ********************************************************

# the purpose of this script is to prepare the data for all 
# tasks for analyses. This includes reading in the csv files 
# as tibbles, cleaning, calculating variables and merging
# the data into a single dataframe.

source("R/parse_qrtengine.R")
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

survey_dir <- "data/java"
survey_paths <- file.path(survey_dir, list.files(survey_dir))
names(survey_paths) <- survey_paths %>% str_replace(stringr::str_c(survey_dir, "/"), "") %>% str_replace("\\.csv", "")

surveys_java <- map(survey_paths, parse_csv)


# ISSUE: identified some TEST uids
# SOLUTION: Remove TEST uids
surveys_java <- surveys_java %>% map(~ filter(., !stringr::str_detect(uid, "TEST"))) %>%
                                 map(~ filter(., !stringr::str_detect(uid, "test")))


# create group variable across all tasks
# surveys <- surveys %>% map(mutate, id = uid) %>%
#             map(separate, id, into = c("date", "group", "member"), sep = "_")


# Applying_Decision_Rules ---------------------------------------------------------------------
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

# function to sort character strings
string_sort <- function(x) paste0(sort(unlist(strsplit(x, "")), decreasing = F), collapse = "")


adr <- adr %>% mutate(Ind_Stimulus.RESP = tolower(Ind_Stimulus.RESP),
                      Grp_Stimulus.RESP = tolower(Grp_Stimulus.RESP),
                      Ind_Decision.RESP = tolower(Ind_Decision.RESP),
                      Grp_Decision.RESP = tolower(Grp_Decision.RESP),
                      Ind_Stimulus.RESP = ifelse(Ind_Stimulus.RESP != "none", sapply(adr$Ind_Stimulus.RESP, string_sort), Ind_Stimulus.RESP),
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
# psych::alpha(x)$total$raw_alpha
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
#   mutate(Ind_Decision.RESP = ifelse(Ind_Decision.RESP == "y", 1, 0)) %>%
#   spread(CRTItemNum, Ind_Decision.RESP) %>%
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



# Cognitive_Reflection_Test -----------------------------------------------
# combine data from a1 and b1 into a single df 
# and rename first 10 columns labelled V1 - V10
crt <- rbind(surveys_qrt$Cognitive_Reflection_Test_A1, surveys_qrt$Cognitive_Reflection_Test_B1,
             surveys_java$Cognitive_Reflection_Test_A2, surveys_java$Cognitive_Reflection_Test_B2)

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

x <- x[1:6, vars]  # Take rows 1 to 6 (corresponding to answered questions)
# and variables of interest

# Insert data from x into relevant part of df
crt[crt$ResponseID == "R_29bx9o8A7LOakQt" & crt$CRTItemNum <= 6, vars] <- x


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
# identified that a number of Ps entered response to bat & ball problem in dollars
# only convert "0.05" to "5" cents. All others are already incorrect.
crt <- crt %>% mutate(Ind_Decision.RESP = tolower(Ind_Decision.RESP),
                      Grp_Decision.RESP = tolower(Grp_Decision.RESP),
                      Ind_Stimulus.RESP = as.numeric(Ind_Stimulus.RESP),
                      Grp_Stimulus.RESP = as.numeric(Grp_Stimulus.RESP),
                      Ind_Confidence.RESP = as.numeric(Ind_Confidence.RESP),
                      Grp_Confidence.RESP = as.numeric(Grp_Confidence.RESP),
                      Ind_Stimulus.RESP = ifelse(Ind_Stimulus.RESP == 0.05, 5, Ind_Stimulus.RESP),
                      Grp_Stimulus.RESP = ifelse(Grp_Stimulus.RESP == 0.05, 5, Ind_Stimulus.RESP))

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

check_values(crt$Grp_Stimulus.RESP, 0, 6000)
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

# Decision
# in cases where member's group decisions differ, change to NA
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
      c("Grp_Stimulus.RESP", "Grp_Stimulus.OnsetTime", "Grp_Stimulus.OffsetTime",
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
#   select(uid, CRTitemnum, Ind_Decision.RESP) %>%
#   mutate(Ind_Decision.RESP = ifelse(Ind_Decision.RESP == "y", 1, 0)) %>%
#   spread(CRTItemNum, Ind_Decision.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Recklessness
# # Using split-half reliability adjusted using the Spearman-Brown prophecy formula
# x <- crt %>%
#   select(uid, CRTitemnum, Ind_Stimulus.ACC, Ind_Decision.RESP) %>%
#   filter(Ind_Stimulus.ACC == 0) %>%
#   select(-Ind_Stimulus.ACC) %>% 
#   mutate(Ind_Decision.RESP = ifelse(Ind_Decision.RESP == "y", 1, 0))
# 
# # odd1 <- c(1, 3, 5, 7)
# 
# odd <- filter(x, CRTitemnum %in% odd1) %>% 
#   group_by(uid) %>%
#   summarise(odd = sum(Ind_Decision.RESP))
# 
# # even1 <- c(2, 4, 6)
# 
# even <- filter(x, CRTitemnum %in% even1) %>% 
#   group_by(uid) %>%
#   summarise(even = sum(Ind_Decision.RESP))
# 
# x <- merge(odd, even) %>%
#   ungroup(uid) %>%
#   select(-uid)
# 
# cor.test(x$odd, x$even)
# 
# # # Adjusting with the Spearman-Brown prophecy formula = (2*r) / (1+r)
# (2*0.4328238) / (1+0.4328238)




# General_knowledge Test -----------------------------------------------
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
#
# # Individual confidence
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
#
# # Individual Decisiveness
# x <- gk %>%
  #   select(uid, Itemnum, Ind_Decision.RESP) %>%
#   mutate(Ind_Decision.RESP = ifelse(Ind_Decision.RESP == "y", 1, 0)) %>%
#   spread(Itemnum, Ind_Decision.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# 
# x <- gk %>%
#   select(uid, Itemnum, Representativeness, Ind_Decision.RESP) %>%
#   filter(Representativeness == "UR")
#   mutate(Ind_Decision.RESP = ifelse(Ind_Decision.RESP == "y", 1, 0)) %>% 
#   spread(Itemnum, Ind_Decision.RESP) %>%
#   select(-uid, -Representativeness)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # recklessness for indivdiuals overall
# # Using split-half reliability adjusted using the Spearman-Brown prophecy formula
# x <- gk %>%
#   select(uid, Itemnum, Ind_Stimulus.ACC, Ind_Decision.RESP) %>%
#   filter(Ind_Stimulus.ACC == 0) %>%
#   select(-Ind_Stimulus.ACC) %>% 
#   mutate(Ind_Decision.RESP = ifelse(Ind_Decision.RESP == "y", 1, 0))
# 
# # odd1 <- c(1, 3, 5, 7)
# 
# odd <- filter(x, Itemnum %in% odd1) %>% 
#   group_by(uid) %>%
#   summarise(odd = sum(Ind_Decision.RESP))
# 
# # even1 <- c(2, 4, 6)
# 
# even <- filter(x, Itemnum %in% even1) %>% 
#   group_by(uid) %>%
#   summarise(even = sum(Ind_Decision.RESP))
# 
# x <- merge(odd, even) %>%
#   ungroup(uid) %>%
#   select(-uid)
# 
# cor.test(x$odd, x$even)
# 
# # # Adjusting with the Spearman-Brown prophecy formula = (2*r) / (1+r)
# (2*0.4328238) / (1+0.4328238)

# MDMT_individual -----------------------------------------------
# combine data from a1, b1, a2 and b2 into a single df 
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
#   select(uid, MdmtItemNum, Ind_Decision.RESP) %>%
#   mutate(Ind_Decision.RESP = ifelse(Ind_Decision.RESP == "a", 1, 0)) %>%
#   spread(MdmtItemNum, Ind_Decision.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Recklessness
# # Using split-half reliability adjusted using the Spearman-Brown prophecy formula
# x <- mdmt_ind %>%
#   select(uid, MdmtItemNum, Ind_Stimulus.ACC, Ind_Decision.RESP) %>%
#   filter(Ind_Stimulus.ACC == 0) %>%
#   select(-Ind_Stimulus.ACC) %>% 
#   mutate(Ind_Decision.RESP = ifelse(Ind_Decision.RESP == "a", 1, 0))
# 
# odd1 <- c(1, 26, 15, 14, 5, 29, 17, 31)
# 
# odd <- filter(x, MdmtItemNum %in% odd1) %>% 
#   group_by(uid) %>%
#   summarise(odd = sum(Ind_Decision.RESP))
# 
# 
# even1 <- c(2, 23, 33, 24, 11, 8, 20, 12)
# 
# even <- filter(x, MdmtItemNum %in% even1) %>% 
#   group_by(uid) %>%
#   summarise(even = sum(Ind_Decision.RESP))
#
# x <- merge(odd, even) %>%
#   ungroup(uid) %>%
#   select(-uid)
# 
# cor.test(x$odd, x$even)
# 
# # # Adjusting with the Spearman-Brown prophecy formula = (2*r) / (1+r)
# (2*0.4328238) / (1+0.4328238)




# RAPM_individual ----------------------------------------------
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


# Compute all variables ------------------------------------------------
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
#   select(uid, ApmItemNum, Ind_Decision.RESP) %>%
#   mutate(Ind_Decision.RESP = ifelse(Ind_Decision.RESP == "y", 1, 0)) %>%
#   spread(ApmItemNum, Ind_Decision.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Recklessness
# x <- rapm_ind %>%
#   select(uid, ApmItemNum, Ind_Stimulus.ACC, Ind_Decision.RESP) %>%
#   filter(Ind_Stimulus.ACC == 0) %>%
#   select(-Ind_Stimulus.ACC) %>% 
#   mutate(Ind_Decision.RESP = ifelse(Ind_Decision.RESP == "y", 1, 0))
# 
# odd1 <- c(1, 5, 9, 13, 17, 21, 25, 29, 33)
# 
# odd <- filter(x, ApmItemNum %in% odd1) %>% 
#   group_by(uid) %>%
#   summarise(odd = sum(Ind_Decision.RESP))
# 
# even1 <- c(3, 7, 11, 15, 19, 23, 27, 31, 35)
# 
# even <- filter(x, ApmItemNum %in% even1) %>% 
#   group_by(uid) %>%
#   summarise(even = sum(Ind_Decision.RESP))
# 
# x <- merge(odd, even) %>%
#   mutate(total = odd + even) %>%
#   ungroup(uid) %>%
#   select(-uid)
# 
# cor.test(x$odd, x$even)
# 
# # # Adjusting with the Spearman-Brown prophecy formula = (2*r) / (1+r)
# (2*0.4328238) / (1+0.4328238)

