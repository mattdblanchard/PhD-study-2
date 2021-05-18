# Cognitive_Reflection_Test -----------------------------------------------
tmp1 <- as.data.frame(surveys_qrt$CRT_A1 %>% 
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

tmp2 <- as.data.frame(surveys_qrt$CRT_B1 %>%
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

tmp3 <- surveys_java$CRT_A2 %>%
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

tmp4 <- surveys_java$CRT_B2 %>%
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


tmp5 <- surveys_multi$crt_multi[, c(-39, -40, -41)] %>% # remove duplicated columns for itemnum, question and cresp
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
         Grp_Stimulus.RT = grp_stimulus_rt, Grp_Stimulus.RESP = grp_response, 
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
  mutate(version = "b_multi")

# uids that completed surveys with multiple JSON columns are included in this data
# the parsing function does not work so all values are NA need to remove and read in separately
uids <- c("18073109_1_d2", "18073109_1_g2", "18073110_2_d1", "18073110_2_g1", "18073114_1_d2",
          "18073114_1_g2", "18080114_1_e1", "18080114_1_e2")

tmp4 <- tmp4 %>% filter(!uid %in% uids)

# combine data into a single df 
crt <- rbind(tmp1, tmp2, tmp3, tmp4, tmp5)

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
# ISSUE 2:
#  identified that the following uids were recorded incorrectly
# 1.    "18041210_2_g1"   session started at 1pm so should be 18041213_2_g1
# 2.    "18041210_2_d1"   session started at 1pm so should be 18041213_2_d1
#
# SOLUTION 2:
# new uid for ResponseID == R_6gSwmIC5h2iZX49 is 18041213_2_g1
# new uid for ResponseID == R_bqO3Mxg9As4UsId is 18041213_2_d1
#
# ISSUE 3: Identified that uid == 18062615_1_e1 appears 3 times. 
# The browser crashed so I had to restart the survey. 
#
# SOLUTION 3. Remove ResponseID == "R_6lPgzrzx2p94I2V" and join responses for 
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
                      uid = ifelse(ResponseID == "R_9Hzo2Sfvv4IS6QB", "18041213_1_g2", uid),
                      uid = ifelse(ResponseID == "R_bqO3Mxg9As4UsId", "18041213_2_d1", uid),
                      uid = ifelse(ResponseID == "R_6gSwmIC5h2iZX49", "18041213_2_g1", uid)) %>%
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
# listen to audio to attempt to recover their response in the meantime
# change their's and their teammate's responses to NA for Q6
crt[crt$uid == "18062615_1_e2" & crt$CRTItemNum == "6", vars] <- NA
crt[crt$uid == "18062615_1_e1" & crt$CRTItemNum == "6", vars] <- NA

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
# also identified that one participant entered "4-0" for an individual confidence rating
# it seems highly likely they were trying to enter "40" so will change
crt <- crt %>% mutate(Ind_Confidence.RESP = ifelse(Ind_Confidence.RESP == "4-0", "40", Ind_Confidence.RESP),
                      Ind_Decision.RESP = tolower(Ind_Decision.RESP),
                      Grp_Decision.RESP = tolower(Grp_Decision.RESP),
                      Ind_Stimulus.RESP = as.numeric(Ind_Stimulus.RESP),
                      Grp_Stimulus.RESP = as.numeric(Grp_Stimulus.RESP),
                      Ind_Confidence.RESP = as.numeric(Ind_Confidence.RESP),
                      Grp_Confidence.RESP = as.numeric(Grp_Confidence.RESP),
                      # convert all responses recorded in dollars to cents
                      Ind_Stimulus.RESP = ifelse(Ind_Stimulus.RESP < 2.3, Ind_Stimulus.RESP * 100, Ind_Stimulus.RESP),
                      # convert all responses recorded in dollars to cents
                      Grp_Stimulus.RESP = ifelse(Grp_Stimulus.RESP < 1.1,Grp_Stimulus.RESP * 100, Grp_Stimulus.RESP))


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

check_values(crt$Ind_Stimulus.RESP, 0, 10000)
check_values(crt$Ind_Confidence.RESP, 0, 100)
check_values(crt$Ind_Decision.RESP, c("y", "n"))

check_values(crt$Grp_Stimulus.RESP, 0, 15000)
check_values(crt$Grp_Confidence.RESP, 0, 100)
check_values(crt$Grp_Decision.RESP, c("y", "n"))

# add Stimulus.ACC column (Stimulus.RESP == Stimulus.CRESP)
crt <- crt %>%
  group_by(uid, CRTItemNum) %>%
  mutate(    
    # convert decisions to lower case for consistency
    Ind_Decision.RESP = tolower(Ind_Decision.RESP),
    Grp_Decision.RESP = tolower(Grp_Decision.RESP),
    # compute accuracy
    Ind_Stimulus.ACC = Ind_Stimulus.RESP == Stimulus.CRESP,
    Grp_Stimulus.ACC = Grp_Stimulus.RESP == Stimulus.CRESP) %>%
  ungroup()

# Check teams are giving same response and decision =======================
# Create grouping variables 
# group == date + group
# member == g == 1, d == 2
crt <- crt %>% mutate(member = ifelse(grepl("g[1-2]", uid), "1", 
                                      ifelse(grepl("d[1-2]", uid), "2", 
                                             ifelse(grepl("e1", uid), "1", "2"))),
                      recruit = ifelse(grepl("[dg]", uid), "sona", "ext"),
                      group = gsub("_[a-z][12]", "", uid))

# session notes reveal a number of issues:
# uid == "17110114_2_g1" mistyped their response to Q1 - should have entered 10
# uid == 18080913_1_g2 entered 10 after the team had agreed upon 20
# uid == 18080913_1_g2 accidentally entered 49 after the team had agreed upon 29
# uid == 18101713_2_g2 entered 10 for Q1 and 5 for confidence. They told me response should be 5 and 100 confidence
# uid == 18080615_1_e1 said 10 cents aloud for Q1 individual and their teammate gave them the correct answer. Change response to 10
crt <- crt %>% 
  mutate(Grp_Stimulus.RESP = ifelse(group == "17110114_2" & CRTItemNum == "1", 10, 
                                    ifelse(group == "18080913_1" & CRTItemNum == "6", 20,
                                           ifelse(group == "18080913_1" & CRTItemNum == "5", 29,
                                                  ifelse(group == "18101713_2" & CRTItemNum == "1", 5, Grp_Stimulus.RESP)))),
         Grp_Stimulus.ACC = ifelse(group == "18080913_1" & CRTItemNum == "6", TRUE,
                                   ifelse(group == "18080913_1" & CRTItemNum == "5", TRUE,
                                          ifelse(group == "18101713_2" & CRTItemNum == "1", TRUE, Grp_Stimulus.ACC))),
         Grp_Confidence.RESP = ifelse(uid == "18101713_2_g2" & CRTItemNum == "1", 100, Grp_Confidence.RESP),
         Ind_Stimulus.RESP = ifelse(uid == "18080615_1_e1" & CRTItemNum == "1", 10, Ind_Stimulus.RESP),
         Ind_Stimulus.ACC = ifelse(uid == "18080615_1_e1" & CRTItemNum == "1", FALSE, Ind_Stimulus.ACC))
         

x <- crt %>% 
  group_by(group) %>% 
  group_by(group, CRTItemNum) %>% 
  mutate(match = Grp_Stimulus.ACC[1] != Grp_Stimulus.ACC[2]) %>% 
  group_by(group) %>% 
  mutate(n = sum(match))

# # to see how many times each team entered different group responses
# x %>% 
#   select(group, n, version) %>% 
#   group_by(group) %>% 
#   summarise(n = n[1] / 2, 
#             version = version[1]) %>% 
#   arrange(desc(n))

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


# convert RT vars to numeric and responses to lower case for scoring
crt <- crt %>%
  group_by(uid, CRTItemNum) %>% 
  mutate(
    # convert confidence responses to numeric
    Ind_Confidence.RESP = as.numeric(Ind_Confidence.RESP),
    Grp_Confidence.RESP = as.numeric(Grp_Confidence.RESP),
    # convert RT variables to numeric
    Ind_Stimulus.OffsetTime = as.numeric(Ind_Stimulus.OffsetTime),
    Ind_Stimulus.OnsetTime = as.numeric(Ind_Stimulus.OnsetTime),
    Ind_Confidence.OffsetTime = as.numeric(Ind_Confidence.OffsetTime),
    Ind_Confidence.OnsetTime = as.numeric(Ind_Confidence.OnsetTime),
    Ind_Decision.OffsetTime = as.numeric(Ind_Decision.OffsetTime),
    Ind_Decision.OnsetTime = as.numeric(Ind_Decision.OnsetTime),
    Grp_Stimulus.OffsetTime = as.numeric(Grp_Stimulus.OffsetTime),
    Grp_Stimulus.OnsetTime = as.numeric(Grp_Stimulus.OnsetTime),
    Grp_Confidence.OffsetTime = as.numeric(Grp_Confidence.OffsetTime),
    Grp_Confidence.OnsetTime = as.numeric(Grp_Confidence.OnsetTime),
    Grp_Decision.OffsetTime = as.numeric(Grp_Decision.OffsetTime),
    Grp_Decision.OnsetTime = as.numeric(Grp_Decision.OnsetTime),
    # calculate individual RTs: offset-onset
    Ind_Stimulus.RT = Ind_Stimulus.OffsetTime - Ind_Stimulus.OnsetTime,
    Ind_Confidence.RT = Ind_Confidence.OffsetTime - Ind_Confidence.OnsetTime,
    Ind_Decision.RT = Ind_Decision.OffsetTime - Ind_Decision.OnsetTime) %>% 
  group_by(group, CRTItemNum) %>% 
    mutate(
      # calculate team RTs: min(offset) - min(onset)
      Grp_Stimulus.RT = min(Grp_Stimulus.OffsetTime) - min(Grp_Stimulus.OnsetTime),
      Grp_Confidence.RT = min(Grp_Confidence.OffsetTime) - min(Grp_Confidence.OnsetTime),
      Grp_Decision.RT = min(Grp_Decision.OffsetTime) - min(Grp_Decision.OnsetTime))

# visualise RTs for each participant
# very slow RTs are not an issue
# RTs look reasonable
# crt %>%
#   ggplot(aes(Ind_Stimulus.RT, colour = CRTItemNum)) +
#   geom_density() +
#   theme_minimal()
# 
# ggplot(crt) +
#   geom_point(aes(x = Ind_Stimulus.RT, y = CRTItemNum))
# 
# crt %>%
#   filter(Ind_Stimulus.RT < 5000) %>%
#   group_by(uid) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# crt %>%
#   filter(Ind_Stimulus.RT < 5000) %>%
#   ggplot() +
#   geom_point(aes(x = CRTItemNum, y = Ind_Stimulus.RT)) +
#   facet_wrap(~ uid)
# 
# crt %>%
#   ggplot(aes(Grp_Stimulus.RT, colour = CRTItemNum)) +
#   geom_density() +
#   theme_minimal()
# 
# ggplot(crt) +
#   geom_point(aes(x = Grp_Stimulus.RT, y = CRTItemNum))
# 
# crt %>%
#   filter(Grp_Stimulus.RT < 1000) %>%
#   group_by(uid) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# crt %>%
#   filter(Grp_Stimulus.RT < 1000) %>%
#   ggplot() +
#   geom_point(aes(x = CRTItemNum, y = Grp_Stimulus.RT)) +
#   facet_wrap(~ uid)


# integrity check: calculate the proportion of times each team entered the 
# same response individually. If very high (>=.80) then need to investigate further 
# may need to check audio to see if they were discussing individual responses
# the following code is to investigate pattern of responses and timing of each item attempted
# check <- crt %>%
#   filter(group == "19020812_1") %>%
#   select(uid, CRTItemNum, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime) %>%
#   mutate(Ind_Stimulus.OnsetTime = Ind_Stimulus.OnsetTime / 1000,
#          Ind_Stimulus.OffsetTime = Ind_Stimulus.OffsetTime / 1000,
#          Ind_Stimulus.OnsetTime = as.POSIXct(Ind_Stimulus.OnsetTime, origin = "1970-01-01 00:00.00 UTC", tz = "Etc/GMT-11"),
#          Ind_Stimulus.OffsetTime = as.POSIXct(Ind_Stimulus.OffsetTime, origin = "1970-01-01 00:00.00 UTC", tz = "Etc/GMT-11"))

# identified two potentially problematic groups 
# group == 18082114_1 - entered the same response to everything and made 4 errors but timings show responses entered at different times
# group == 19020812_2 - entered the same response to everything and made 3 errors and timings were very similar (tended to be within ~2 secs)
crt %>% 
  group_by(group, CRTItemNum) %>% 
  summarise(same_resp = Ind_Stimulus.RESP[1] == Ind_Stimulus.RESP[2]) %>% 
  group_by(group) %>% 
  summarise(n_same = sum(same_resp),
            items = n()) %>% 
  group_by(group) %>% 
  summarise(prop_same = n_same/items) %>% 
  filter(prop_same >= .8) %>% 
  arrange(desc(prop_same))

# Create CC column --------------------------------------------------------
# need to work out calculation first
# these are open ended questions so multiple responses possible
# when majority response is correct = CC
# when majority response is incorrect - CW?
# is Simon's paper on response cardinality relevant?
# im <- crt %>%
#   group_by(CRTItemNum) %>%
#   summarise(item.mean = mean(Ind_Stimulus.ACC)) %>%
#   mutate(cc = ifelse(item.mean >= .5, "cc","cw"))
# 
# crt <- crt %>% left_join(im)

# Create consensus (agree vs disagree) column -----------------------------
crt <- crt %>% 
  group_by(group, CRTItemNum) %>% 
  mutate(consensus = ifelse(Ind_Stimulus.RESP[1] == Ind_Stimulus.RESP[2], 
                            "agree", "disagree"))

# # save cleaned item-level data
# crt %>% write_csv("data/item_level/crt_item_level.csv")

# Compute all variables ------------------------------------------------
crt.uid <- crt %>%
  group_by(uid) %>%
  summarise(
    # group = group[1],
    crt.disagree = 100 * mean(consensus == "disagree", na.rm = TRUE),
    crt.ind.acc = mean(Ind_Stimulus.ACC) * 100,
    crt.grp.acc = mean(Grp_Stimulus.ACC, na.rm = TRUE) * 100,
    crt.ind.conf = mean(Ind_Confidence.RESP),
    crt.grp.conf = mean(Grp_Confidence.RESP, na.rm = TRUE),
    crt.ind.bias = crt.ind.conf - crt.ind.acc,
    crt.grp.bias = crt.grp.conf - crt.grp.acc,
    crt.ind.discrimination = mean(Ind_Confidence.RESP[Ind_Stimulus.ACC == 1], na.rm = TRUE) - mean(Ind_Confidence.RESP[Ind_Stimulus.ACC == 0], na.rm = TRUE),
    crt.grp.discrimination = mean(Grp_Confidence.RESP[Grp_Stimulus.ACC == 1], na.rm = TRUE) - mean(Grp_Confidence.RESP[Grp_Stimulus.ACC == 0], na.rm = TRUE),
    crt.ind.dec = mean(Ind_Decision.RESP == "y") * 100,
    crt.grp.dec = mean(Grp_Decision.RESP == "y", na.rm = TRUE) * 100,
    crt.ind.rec = 100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 0, na.rm = TRUE)
    / mean(Ind_Stimulus.ACC == 0, na.rm = TRUE),
    crt.grp.rec = 100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 0, na.rm = TRUE)
    / mean(Grp_Stimulus.ACC == 0, na.rm = TRUE),
    crt.ind.comp = 100 * (sum(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 1, na.rm = TRUE) +
                            sum(Ind_Decision.RESP == "n" & Ind_Stimulus.ACC == 0, na.rm = TRUE)) / length(Ind_Stimulus.ACC),
    crt.grp.comp = 100 * (sum(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 1, na.rm = TRUE) +
                            sum(Grp_Decision.RESP == "n" & Grp_Stimulus.ACC == 0, na.rm = TRUE)) / length(Grp_Stimulus.ACC),
    crt.ind.optim = 100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 1, na.rm = TRUE),
    crt.grp.optim = 100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 1, na.rm = TRUE),
    crt.ind.hes = 100 * sum(Ind_Decision.RESP == "n" & Ind_Stimulus.ACC == 1, na.rm = TRUE)
    / sum(Ind_Stimulus.ACC == 1, na.rm = TRUE),
    crt.grp.hes = 100 * sum(Grp_Decision.RESP == "n" & Grp_Stimulus.ACC == 1, na.rm = TRUE)
    / sum(Grp_Stimulus.ACC == 1, na.rm = TRUE))

# calculate control thresholds
# Logisitic regression: use confidence to predict the point at which a bet decision switches from no to yes
# Great tutorial for conductin logistic regression: https://www.datacamp.com/community/tutorials/logistic-regression-R
library(broom)

# prepare individual data
data <- crt %>%
  ungroup() %>% 
  select(uid, CRTItemNum, Ind_Confidence.RESP, Ind_Decision.RESP) %>%
  group_by(uid) %>% 
  filter(!all(is.na(Ind_Decision.RESP))) %>% 
  mutate(
    Ind_Decision.RESP = as.numeric(ifelse(Ind_Decision.RESP == "y", 1,
                                          ifelse(Ind_Decision.RESP == "n", 0, NA))),
    conf_range = abs(max(Ind_Confidence.RESP, na.rm = T) - min(Ind_Confidence.RESP, na.rm = T)),
    dec_range = abs(max(Ind_Decision.RESP, na.rm = T) - min(Ind_Decision.RESP, na.rm = T)))

# remove participants with no varaince in their confidence ratings or bet decisions
data <- data %>% filter(conf_range > 0 | dec_range > 0)

# fit logistic regression model and compute POST
post <- data %>% 
  nest() %>% 
  mutate(fit = map(data, ~ glm(Ind_Decision.RESP ~ Ind_Confidence.RESP, data = ., family = binomial)),
         coef = map(fit, ~ tidy(.)$estimate),
         coef = map(coef, ~ data.frame(a = .[[1]], b = .[2]))) %>% 
  unnest(coef) %>% 
  group_by(uid) %>% 
  summarise(crt.ind.post = (-a)/b) %>% 
  filter(crt.ind.post >= 0 & crt.ind.post <= 100)

# join individual post data to test dataset
crt.uid <- crt.uid %>% left_join(post, by = "uid")

# prepare group data
data <- crt %>%
  ungroup() %>% 
  select(uid, CRTItemNum, Grp_Confidence.RESP, Grp_Decision.RESP) %>%
  group_by(uid) %>% 
  filter(!all(is.na(Grp_Decision.RESP))) %>% 
  mutate(
    Grp_Decision.RESP = as.numeric(ifelse(Grp_Decision.RESP == "y", 1,
                                          ifelse(Grp_Decision.RESP == "n", 0, NA))),
    conf_range = abs(max(Grp_Confidence.RESP, na.rm = T) - min(Grp_Confidence.RESP, na.rm = T)),
    dec_range = abs(max(Grp_Decision.RESP, na.rm = T) - min(Grp_Decision.RESP, na.rm = T)))

# remove participants with no varaince in their confidence ratings or bet decisions
data <- data %>% filter(conf_range > 0 | dec_range > 0)

# fit logistic regression model and compute POST
post <- data %>% 
  nest() %>% 
  mutate(fit = map(data, ~ glm(Grp_Decision.RESP ~ Grp_Confidence.RESP, data = ., family = binomial)),
         coef = map(fit, ~ tidy(.)$estimate),
         coef = map(coef, ~ data.frame(a = .[[1]], b = .[2]))) %>% 
  unnest(coef) %>% 
  group_by(uid) %>% 
  summarise(crt.grp.post = (-a)/b) %>% 
  filter(crt.grp.post >= 0 & crt.grp.post <= 100)

# join group post data to test dataset
crt.uid <- crt.uid %>% left_join(post, by = "uid")


# Reliability ----------------------------------------------------
# remove teams that made non-genuine attempt
# crt <- crt %>% filter(!group %in% c("18041210_2", "18080915_1", "18072412_1", "19013010_1", "17102710_2", "18081613_1"))
#
# # # FOR INDIVIDUALS
# # Accuracy
# x <- crt %>%
#   ungroup() %>% 
#   select(uid, CRTItemNum, Ind_Stimulus.ACC) %>%
#   mutate(Ind_Stimulus.ACC = ifelse(Ind_Stimulus.ACC == TRUE, 1, 
#                                    ifelse(Ind_Stimulus.ACC == FALSE, 0, NA))) %>%
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
# # Adjust r with the Spearman-Brown prophecy formula == (2*r) / (1+r)
# (2*0.4536502) / (1+0.4536502)
#
# use this website to correct using the spearman-brown prophecy formula
# https://www.cedu.niu.edu/~walker/calculators/sbpf.asp


# # FOR TEAMS
# # Accuracy
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
# # Adjust r with the Spearman-Brown prophecy formula == (2*r) / (1+r)
# (2*0.5683701) / (1+0.5683701)
# 
# use this website to correct using the spearman-brown prophecy formula
# https://www.cedu.niu.edu/~walker/calculators/sbpf.asp
