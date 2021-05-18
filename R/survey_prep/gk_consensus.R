# General_knowledge Test -----------------------------------------------
tmp1 <- as.data.frame(surveys_qrt$GK_A1 %>%
            select(ResponseID, StartDate:Finished, uid, Itemnum, Question, Representativeness, Ind_Stimulus.CRESP, 
                   Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, 
                   Ind_Stimulus.OffsetTime, Ind_Confidence.RESP, Ind_Confidence.OnsetTime, 
                   Ind_Confidence.OffsetTime, Ind_Decision.RESP, Ind_Decision.RT, 
                   Ind_Decision.OnsetTime, Ind_Decision.OffsetTime, 
                   Wait.OnsetTime, Wait.OffsetTime, Wait.RT, 
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
  
  tmp2 <- as.data.frame(surveys_qrt$GK_B1 %>%
            select(ResponseID, StartDate:Finished, uid, Itemnum, Question, Representativeness, Ind_Stimulus.CRESP, 
                   Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, 
                   Ind_Stimulus.OffsetTime, Ind_Confidence.RESP, Ind_Confidence.OnsetTime, 
                   Ind_Confidence.OffsetTime, Ind_Decision.RESP, Ind_Decision.RT, 
                   Ind_Decision.OnsetTime, Ind_Decision.OffsetTime, 
                   Wait.OnsetTime, Wait.OffsetTime, Wait.RT, 
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

tmp3 <- surveys_java$GK_A2 %>%
          rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, Itemnum = itemnum, 
                 Ind_Stimulus.OnsetTime = ind_stimulus_onset,
                 Ind_Stimulus.OffsetTime = ind_stimulus_offset, Ind_Stimulus.RT = ind_stimulus_rt,
                 Ind_Stimulus.RESP = ind_response, Ind_Confidence.OnsetTime = ind_confidence_onset,
                 Ind_Confidence.OffsetTime = ind_confidence_offset, Ind_Confidence.RT = ind_confidence_rt,
                 Ind_Confidence.RESP = ind_confidence, Ind_Decision.OnsetTime = ind_decision_onset,
                 Ind_Decision.OffsetTime = ind_decision_offset, Ind_Decision.RT = ind_decision_rt,
                 Ind_Decision.RESP = ind_decision, Wait.OnsetTime = wait_onset,
                 Wait.OffsetTime = wait_offset, Wait.RT = wait_rt,
                 Grp_Stimulus.OnsetTime = grp_stimulus_onset,
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
                 Wait.OnsetTime, Wait.OffsetTime, Wait.RT, 
                 Grp_Stimulus.RESP, Grp_Stimulus.RT, Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime,
                 Grp_Confidence.RESP, Grp_Confidence.RT, Grp_Confidence.OnsetTime, Grp_Confidence.OffsetTime,
                 Grp_Decision.RESP, Grp_Decision.RT, Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
          mutate(version = "a2")

tmp4 <- surveys_java$GK_B2 %>%
          rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, Itemnum = itemnum, 
                 Ind_Stimulus.OnsetTime = ind_stimulus_onset,
                 Ind_Stimulus.OffsetTime = ind_stimulus_offset, Ind_Stimulus.RT = ind_stimulus_rt,
                 Ind_Stimulus.RESP = ind_response, Ind_Confidence.OnsetTime = ind_confidence_onset,
                 Ind_Confidence.OffsetTime = ind_confidence_offset, Ind_Confidence.RT = ind_confidence_rt,
                 Ind_Confidence.RESP = ind_confidence, Ind_Decision.OnsetTime = ind_decision_onset,
                 Ind_Decision.OffsetTime = ind_decision_offset, Ind_Decision.RT = ind_decision_rt,
                 Ind_Decision.RESP = ind_decision, Wait.OnsetTime = wait_onset,
                 Wait.OffsetTime = wait_offset, Wait.RT = wait_rt,
                 Grp_Stimulus.OnsetTime = grp_stimulus_onset,
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
                 Wait.OnsetTime, Wait.OffsetTime, Wait.RT, 
                 Grp_Stimulus.RESP, Grp_Stimulus.RT, Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime,
                 Grp_Confidence.RESP, Grp_Confidence.RT, Grp_Confidence.OnsetTime, Grp_Confidence.OffsetTime,
                 Grp_Decision.RESP, Grp_Decision.RT, Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
          mutate(version = "b2")


tmp5 <- surveys_multi$gk_multi %>%
  rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, Itemnum = itemnum, 
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
         Grp_Decision.RT = grp_decision_rt, Grp_Decision.RESP = grp_decision,
         Question = question, Representativeness = representativeness) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, Itemnum, Question, Representativeness, Stimulus.CRESP, 
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
gk <- rbind(tmp1, tmp2, tmp3, tmp4, tmp5)

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
# ISSUE 2: Identified the following uids were incorrectly recorded
#   "18041210_2_g1"   session started at 1pm so should be 18041213_2_g1
#   "18041210_2_d1"   session started at 1pm so should be 18041213_2_d1
#
# SOLUTION 2: change uids to reflect correct start time
# new uid for ResponseID == R_40hgX8GhxuQNk6F is 18041213_2_g1
# new uid for ResponseID == R_2lO0sDJgluMBFf7 is 18041213_2_d1
#
# ISSUE 2: Identified that uid == "18040910_2_g1" & "17102710_2_d1" appears twice each
# The browser crashed so I had to restart the survey. 
#
# SOLUTION 2. 
# For uid == "18040910_2_g1" take responses for Q1-6 from 1st attempt 
# ResponseID == "R_07nbXqOSwWXHx8p" and join with responses for Q12-20 
# from 2nd attemp ResponseID == "R_cuwZ6DNRTH1c5Bb" then remove first attempt

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
                    uid = ifelse(ResponseID == "R_8qMLzAM3dx42ab3", "18041213_1_g2", uid),
                    uid = ifelse(ResponseID == "R_40hgX8GhxuQNk6F", "18041213_2_g1", uid),
                    uid = ifelse(ResponseID == "R_2lO0sDJgluMBFf7", "18041213_2_d1", uid)) %>%
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
# first make text for all questions the same
gk <- gk %>%
  mutate(Question = ifelse(str_detect(Question, "Which state/territory has a larger area?"), "Which state has a larger area?",
                           ifelse(str_detect(Question, "Which state/territory has a larger length of coastline?"), "Which state has a larger length of coastline?",
                                  ifelse(str_detect(Question, "Which state/territory has a larger area?"), "Which state has a larger area?",
                                         ifelse(str_detect(Question, "Which state/territory has a larger population?"), "Which state has a larger population?", Question)))))

gk <- gk %>%
  group_by(uid) %>%
  mutate(Itemnum = 1:n()) %>%
  ungroup()

# check that itemnums were correctly modified
item <- unique(gk$Itemnum)
item

# Create grouping variables -----------------------------------------------
# group == date + group
# member == g == 1, d == 2
gk <- gk %>% mutate(member = ifelse(grepl("g[1-2]", uid), "1", 
                                    ifelse(grepl("d[1-2]", uid), "2", 
                                           ifelse(grepl("e1", uid), "1", "2"))),
                    recruit = ifelse(grepl("[dg]", uid), "sona", "ext"),
                    group = gsub("_[a-z][12]", "", uid))

# all print outs should contain 238 responses
n_responses <- 238

for (i in item) {
  n <- count(gk %>% filter(Itemnum == i) %>% select(group, Itemnum, Stimulus.CRESP))
  if (all(n$n == n_responses)) {
  print(paste0("Item ", i, " appears the correct number of times")) 
  } else {
  print("Items that do not have the correct number of responses:")  
  print(n %>% filter(n != n_responses))
  }
}

# can also check that the Itemnums for a1 and b1 match those for a2 and b2
x1 <- unique(gk %>% ungroup() %>%  filter(version == "a1" | version == "b1") %>% select(Itemnum, Question, Representativeness, Stimulus.CRESP))
x2 <- unique(gk %>%  ungroup() %>% filter(version == "a2" | version == "b2") %>% select(Itemnum, Question, Representativeness, Stimulus.CRESP))

# add Stimulus.ACC column (Stimulus.RESP == Stimulus.CRESP)
gk <- gk %>% 
  group_by(uid, Itemnum) %>%
  mutate(Stimulus.CRESP = as.numeric(Stimulus.CRESP),
         Ind_Stimulus.ACC = Ind_Stimulus.RESP == Stimulus.CRESP,
         Grp_Stimulus.ACC = Grp_Stimulus.RESP == Stimulus.CRESP) %>%
  ungroup()

# Check teams are giving same response and decision =======================
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

# convert RT vars to numeric
gk <- gk %>%
  group_by(uid, Itemnum) %>% 
  mutate(
    # convert decisions to lower case for consistency
    Ind_Decision.RESP = tolower(Ind_Decision.RESP),
    Grp_Decision.RESP = tolower(Grp_Decision.RESP),
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
  group_by(group, Itemnum) %>% 
  mutate(
    # calculate team RTs: min(offset) - min(onset)
    Grp_Stimulus.RT = min(Grp_Stimulus.OffsetTime) - min(Grp_Stimulus.OnsetTime),
    Grp_Confidence.RT = min(Grp_Confidence.OffsetTime) - min(Grp_Confidence.OnsetTime),
    Grp_Decision.RT = min(Grp_Decision.OffsetTime) - min(Grp_Decision.OnsetTime))

# # visualise RTs for each participant
# # very slow RTs are not an issue
# # identified uid == "18042013_1_d2" answered 15 items < 2 sec
# gk %>%
#   ggplot(aes(Ind_Stimulus.RT, colour = Itemnum)) +
#   geom_density() +
#   theme_minimal()
# 
# ggplot(gk) +
#   geom_point(aes(x = Ind_Stimulus.RT, y = Itemnum))
# 
# gk %>%
#   filter(Ind_Stimulus.RT < 2000) %>%
#   group_by(uid) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# gk %>%
#   filter(Ind_Stimulus.RT < 2000) %>%
#   ggplot() +
#   geom_point(aes(x = Itemnum, y = Ind_Stimulus.RT)) +
#   facet_wrap(~ uid)
# 
# gk %>%
#   ggplot(aes(Grp_Stimulus.RT, colour = Itemnum)) +
#   geom_density() +
#   theme_minimal()
# 
# ggplot(gk) +
#   geom_point(aes(x = Grp_Stimulus.RT, y = Itemnum))
# 
# gk %>%
#   filter(Grp_Stimulus.RT < 1000) %>%
#   group_by(uid) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# gk %>%
#   filter(Grp_Stimulus.RT < 1000) %>%
#   ggplot() +
#   geom_point(aes(x = Itemnum, y = Grp_Stimulus.RT)) +
#   facet_wrap(~ uid)


# integrity check: calculate the proportion of times each team entered the 
# same response individually. If very high (>=.90) then need to investigate further 
# need to check audio to see if they were discussing individual responses
# identified to potentially problematic groups 
gk %>% 
  group_by(group, Itemnum) %>% 
  summarise(same_resp = Ind_Stimulus.RESP[1] == Ind_Stimulus.RESP[2]) %>% 
  group_by(group) %>% 
  summarise(n_same = sum(same_resp),
            items = n()) %>% 
  group_by(group) %>% 
  summarise(prop_same = n_same/items) %>% 
  filter(prop_same >= .9) %>% 
  arrange(desc(prop_same))

# Create CC column --------------------------------------------------------
im <- gk %>%
  group_by(Itemnum) %>%
  summarise(item.mean = mean(Ind_Stimulus.ACC)) %>%
  mutate(cc = ifelse(item.mean >= .5, "cc","cw"))

gk <- gk %>% left_join(im, by = "Itemnum")


# Create consensus (agree vs disagree) column -----------------------------
gk <- gk %>% 
  group_by(group, Itemnum) %>% 
  mutate(consensus = ifelse(Ind_Stimulus.ACC[1] == Ind_Stimulus.ACC[2], 
                            "agree", "disagree"))

# save cleaned item-level data
gk %>% write_csv("data/item_level/gk_item_level.csv")

# Compute all variables ------------------------------------------------
# overall GK variables without consensus
gk.uid <- gk %>%
  group_by(uid, consensus) %>%
  summarise(
    # group = group[1],
    gk.ind.accO = mean(Ind_Stimulus.ACC) * 100,
    gk.grp.accO = mean(Grp_Stimulus.ACC, na.rm = TRUE) * 100,
    gk.ind.confO = mean(Ind_Confidence.RESP),
    gk.grp.confO = mean(Grp_Confidence.RESP, na.rm = TRUE),
    gk.ind.decO = mean(Ind_Decision.RESP == "y") * 100,
    gk.grp.decO = mean(Grp_Decision.RESP == "y", na.rm = TRUE) * 100,
    gk.ind.recO = 100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 0, na.rm = TRUE)
    / mean(Ind_Stimulus.ACC == 0, na.rm = TRUE),
    gk.grp.recO = 100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 0, na.rm = TRUE)
    / mean(Grp_Stimulus.ACC == 0, na.rm = TRUE),
    gk.ind.compO = 100 * (sum(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 1, na.rm = TRUE) +
                            sum(Ind_Decision.RESP == "n" & Ind_Stimulus.ACC == 0, na.rm = TRUE)) / length(Ind_Stimulus.ACC),
    gk.grp.compO = 100 * (sum(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 1, na.rm = TRUE) +
                            sum(Grp_Decision.RESP == "n" & Grp_Stimulus.ACC == 0, na.rm = TRUE)) / length(Grp_Stimulus.ACC),
    gk.ind.optimO = 100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 1, na.rm = TRUE),
    gk.grp.optimO = 100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 1, na.rm = TRUE),
    gk.ind.hesO = 100 * sum(Ind_Decision.RESP == "n" & Ind_Stimulus.ACC == 1, na.rm = TRUE) / sum(Ind_Stimulus.ACC == 1, na.rm = TRUE),
    gk.grp.hesO = 100 * sum(Grp_Decision.RESP == "n" & Grp_Stimulus.ACC == 1, na.rm = TRUE) / sum(Grp_Stimulus.ACC == 1, na.rm = TRUE))


# calculate control thresholds
# Logisitic regression: use confidence to predict the point at which a bet decision switches from no to yes
# Great tutorial for conductin logistic regression: https://www.datacamp.com/community/tutorials/logistic-regression-R
library(broom)

post_data <- gk %>%
  ungroup() %>%
  select(uid, consensus, Ind_Confidence.RESP, Ind_Decision.RESP, Grp_Confidence.RESP, Grp_Decision.RESP) %>%
  mutate(Ind_Decision.RESP = as.factor(Ind_Decision.RESP),
         Grp_Decision.RESP = as.factor(Grp_Decision.RESP)) %>%
  group_by(uid) %>%
  mutate(ind_dec_sd = sd(Ind_Decision.RESP),
         grp_dec_sd = sd(Grp_Decision.RESP))

# individual POST
post_ind <- post_data %>%
  # filter(ind_dec_sd != 0) %>%
  select(-ind_dec_sd, -grp_dec_sd) %>%
  group_by(uid, consensus) %>%
  nest() %>%
  mutate(fit = map(data, ~ glm(Ind_Decision.RESP ~ Ind_Confidence.RESP, data = ., family = binomial)),
         coef = map(fit, ~ tidy(.)$estimate),
         coef = map(coef, ~ data.frame(a = .[[1]], b = .[2]))) %>%
  unnest(coef)

post_ind <- post_ind %>%
  group_by(uid, consensus) %>%
  summarise(gk.ind.postO = (-1*a)/b) %>%
  filter(gk.ind.postO >= 0 & gk.ind.postO <= 100)

# group POST
post_grp <- post_data %>%
  filter(grp_dec_sd != 0) %>%
  select(-ind_dec_sd, -grp_dec_sd) %>%
  group_by(uid, consensus) %>%
  nest() %>%
  mutate(fit = map(data, ~ glm(Grp_Decision.RESP ~ Grp_Confidence.RESP, data = ., family = binomial)),
         coef = map(fit, ~ tidy(.)$estimate),
         coef = map(coef, ~ data.frame(a = .[[1]], b = .[2]))) %>%
  unnest(coef)

post_grp <- post_grp %>%
  group_by(uid, consensus) %>%
  summarise(gk.grp.postO = (-1*a)/b) %>%
  filter(gk.grp.postO >= 0 & gk.grp.postO <= 100)

# join POST variables to dataset
gk.uid <- gk.uid %>% left_join(post_ind, by = c("uid", "consensus"))

gk.uid <- gk.uid %>% left_join(post_grp, by = c("uid", "consensus"))

# create wide data so it can be joined with gk.rep
gk.uid <- gk.uid %>% 
  gather(var, val, -uid, -consensus) %>% 
  unite(var, c("var", "consensus"), sep = "_") %>% 
  spread(var, val)



# overall GK variables with consensus
gk.uid <- gk %>%
  group_by(uid, consensus) %>%
  summarise(
    # group = group[1],
    gk.ind.accO = mean(Ind_Stimulus.ACC) * 100,
    gk.grp.accO = mean(Grp_Stimulus.ACC, na.rm = TRUE) * 100,
    gk.ind.confO = mean(Ind_Confidence.RESP),
    gk.grp.confO = mean(Grp_Confidence.RESP, na.rm = TRUE),
    gk.ind.decO = mean(Ind_Decision.RESP == "y") * 100,
    gk.grp.decO = mean(Grp_Decision.RESP == "y", na.rm = TRUE) * 100,
    gk.ind.recO = 100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 0, na.rm = TRUE)
    / mean(Ind_Stimulus.ACC == 0, na.rm = TRUE),
    gk.grp.recO = 100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 0, na.rm = TRUE)
    / mean(Grp_Stimulus.ACC == 0, na.rm = TRUE),
    gk.ind.compO = 100 * (sum(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 1, na.rm = TRUE) +
                            sum(Ind_Decision.RESP == "n" & Ind_Stimulus.ACC == 0, na.rm = TRUE)) / length(Ind_Stimulus.ACC),
    gk.grp.compO = 100 * (sum(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 1, na.rm = TRUE) +
                            sum(Grp_Decision.RESP == "n" & Grp_Stimulus.ACC == 0, na.rm = TRUE)) / length(Grp_Stimulus.ACC),
    gk.ind.optimO = 100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 1, na.rm = TRUE),
    gk.grp.optimO = 100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 1, na.rm = TRUE),
    gk.ind.hesO = 100 * sum(Ind_Decision.RESP == "n" & Ind_Stimulus.ACC == 1, na.rm = TRUE) / sum(Ind_Stimulus.ACC == 1, na.rm = TRUE),
    gk.grp.hesO = 100 * sum(Grp_Decision.RESP == "n" & Grp_Stimulus.ACC == 1, na.rm = TRUE) / sum(Grp_Stimulus.ACC == 1, na.rm = TRUE))


# calculate control thresholds
# Logisitic regression: use confidence to predict the point at which a bet decision switches from no to yes
# Great tutorial for conductin logistic regression: https://www.datacamp.com/community/tutorials/logistic-regression-R
library(broom)

# prepare individual data
data <- gk %>%
  ungroup() %>% 
  select(uid, consensus, Itemnum, Ind_Confidence.RESP, Ind_Decision.RESP) %>%
  group_by(uid, consensus) %>% 
  filter(!all(is.na(Ind_Decision.RESP))) %>% 
  mutate(
    Ind_Decision.RESP = as.numeric(ifelse(Ind_Decision.RESP == "y", 1,
                                          ifelse(Ind_Decision.RESP == "n", 0, NA))),
    conf_range = abs(max(Ind_Confidence.RESP, na.rm = T) - min(Ind_Confidence.RESP, na.rm = T)),
    dec_range = abs(max(Ind_Decision.RESP, na.rm = T) - min(Ind_Decision.RESP, na.rm = T)))

# remove participants with no variance in their confidence ratings or bet decisions
data <- data %>% filter(conf_range > 0 | dec_range > 0)

# fit logistic regression model and compute POST
post <- data %>% 
  nest() %>% 
  mutate(fit = map(data, ~ glm(Ind_Decision.RESP ~ Ind_Confidence.RESP, data = ., family = binomial)),
         coef = map(fit, ~ tidy(.)$estimate),
         coef = map(coef, ~ data.frame(a = .[[1]], b = .[2]))) %>% 
  unnest(coef) %>% 
  group_by(uid, consensus) %>% 
  summarise(gk.ind.postO = -a/b) %>% 
  filter(gk.ind.postO >= 0 & gk.ind.postO <= 100)

# join individual post data to test dataset
gk.uid <- gk.uid %>% left_join(post, by = c("uid", "consensus"))

# prepare group data
data <- gk %>%
  ungroup() %>% 
  select(uid, consensus, Itemnum, Grp_Confidence.RESP, Grp_Decision.RESP) %>%
  group_by(uid, consensus) %>% 
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
  group_by(uid, consensus) %>% 
  summarise(gk.grp.postO = -a/b) %>% 
  filter(gk.grp.postO >= 0 & gk.grp.postO <= 100)

# join group post data to test dataset
gk.uid <- gk.uid %>% left_join(post, by = c("uid", "consensus"))

# create wide data so it can be joined with gk.rep
gk.uid <- gk.uid %>% 
gather(var, val, -uid, -consensus) %>% 
  unite(var, c("var", "consensus"), sep = "_") %>% 
  spread(var, val)


# # Calculate gk variables separately for representative and unrepresentative items ---------------------------------------
# with consensus
gk.rep <- gk %>%
  group_by(uid, consensus, Representativeness) %>%
  summarise(
    # group = group[1],
    gk.ind.acc = mean(Ind_Stimulus.ACC) * 100,
    gk.grp.acc = mean(Grp_Stimulus.ACC, na.rm = TRUE) * 100,
    gk.ind.conf = mean(Ind_Confidence.RESP),
    gk.grp.conf = mean(Grp_Confidence.RESP, na.rm = TRUE),
    gk.ind.dec = mean(Ind_Decision.RESP == "y") * 100,
    gk.grp.dec = mean(Grp_Decision.RESP == "y", na.rm = TRUE) * 100,
    gk.ind.rec = 100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 0, na.rm = TRUE)
    / mean(Ind_Stimulus.ACC == 0, na.rm = TRUE),
    gk.grp.rec = 100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 0, na.rm = TRUE)
    / mean(Grp_Stimulus.ACC == 0, na.rm = TRUE),
    gk.ind.comp = 100 * (sum(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 1, na.rm = TRUE) +
                           sum(Ind_Decision.RESP == "n" & Ind_Stimulus.ACC == 0, na.rm = TRUE)) / length(Ind_Stimulus.ACC),
    gk.grp.comp = 100 * (sum(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 1, na.rm = TRUE) +
                           sum(Grp_Decision.RESP == "n" & Grp_Stimulus.ACC == 0, na.rm = TRUE)) / length(Grp_Stimulus.ACC),
    gk.ind.optim = 100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 1, na.rm = TRUE),
    gk.grp.optim = 100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 1, na.rm = TRUE),
    gk.ind.hes = 100 * sum(Ind_Decision.RESP == "n" & Ind_Stimulus.ACC == 1, na.rm = TRUE) / sum(Ind_Stimulus.ACC == 1, na.rm = TRUE),
    gk.grp.hes = 100 * sum(Grp_Decision.RESP == "n" & Grp_Stimulus.ACC == 1, na.rm = TRUE) / sum(Grp_Stimulus.ACC == 1, na.rm = TRUE))



# calculate control thresholds
# Logisitic regression: use confidence to predict the point at which a bet decision switches from no to yes
# Great tutorial for conductin logistic regression: https://www.datacamp.com/community/tutorials/logistic-regression-R

# prepare individual data
data <- gk %>%
  ungroup() %>% 
  select(uid, consensus, Representativeness, Itemnum, Ind_Confidence.RESP, Ind_Decision.RESP) %>%
  group_by(uid, consensus, Representativeness) %>% 
  filter(!all(is.na(Ind_Decision.RESP))) %>% 
  mutate(
    Ind_Decision.RESP = as.numeric(ifelse(Ind_Decision.RESP == "y", 1,
                                          ifelse(Ind_Decision.RESP == "n", 0, NA))),
    conf_range = abs(max(Ind_Confidence.RESP, na.rm = T) - min(Ind_Confidence.RESP, na.rm = T)),
    dec_range = abs(max(Ind_Decision.RESP, na.rm = T) - min(Ind_Decision.RESP, na.rm = T)))

# remove participants with no variance in their confidence ratings or bet decisions
data <- data %>% filter(conf_range > 0 | dec_range > 0)

# fit logistic regression model and compute POST
post <- data %>% 
  nest() %>% 
  mutate(fit = map(data, ~ glm(Ind_Decision.RESP ~ Ind_Confidence.RESP, data = ., family = binomial)),
         coef = map(fit, ~ tidy(.)$estimate),
         coef = map(coef, ~ data.frame(a = .[[1]], b = .[2]))) %>% 
  unnest(coef) %>% 
  group_by(uid, consensus, Representativeness) %>% 
  summarise(gk.ind.post = -a/b) %>% 
  filter(gk.ind.post >= 0 & gk.ind.post <= 100)

# join individual post data to test dataset
gk.rep <- gk.rep %>% left_join(post, by = c("uid", "consensus", "Representativeness"))

# prepare group data
data <- gk %>%
  ungroup() %>% 
  select(uid, consensus, Representativeness, Itemnum, Grp_Confidence.RESP, Grp_Decision.RESP) %>%
  group_by(uid, consensus, Representativeness) %>% 
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
  group_by(uid, consensus, Representativeness) %>% 
  summarise(gk.grp.post = -a/b) %>% 
  filter(gk.grp.post >= 0 & gk.grp.post <= 100)

# join group post data to test dataset
gk.rep <- gk.rep %>% left_join(post, by = c("uid", "consensus", "Representativeness"))

# create wide data
gk.rep <- gk.rep %>% 
  gather(var, val, -uid, -consensus, -Representativeness) %>% 
  unite(var, var, Representativeness, sep = "") %>% 
  unite(var, var, consensus, sep = "_") %>% 
  spread(var, val)

# join with gk.uid
gk.uid <- gk.uid %>% left_join(gk.rep, by = "uid")



# calculate CC/CW dataset
# gk.uid.cc <- gk %>%
#   group_by(uid, cc) %>%
#   summarise(
#     # group = group[1],
#     gk.ind.acc = mean(Ind_Stimulus.ACC) * 100,
#     gk.grp.acc = mean(Grp_Stimulus.ACC, na.rm = TRUE) * 100,
#     gk.ind.conf = mean(Ind_Confidence.RESP),
#     gk.grp.conf = mean(Grp_Confidence.RESP, na.rm = TRUE),
#     gk.ind.dec = mean(Ind_Decision.RESP == "y") * 100,
#     gk.grp.dec = mean(Grp_Decision.RESP == "y", na.rm = TRUE) * 100,
#     gk.ind.rec = 100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 0, na.rm = TRUE) 
#     / mean(Ind_Stimulus.ACC == 0, na.rm = TRUE),
#     gk.grp.rec = 100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 0, na.rm = TRUE) 
#     / mean(Grp_Stimulus.ACC == 0, na.rm = TRUE),
#     gk.ind.comp = 100 * (sum(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 1, na.rm = TRUE) +
#                            sum(Ind_Decision.RESP == "n" & Ind_Stimulus.ACC == 0, na.rm = TRUE)) / length(Ind_Stimulus.ACC),
#     gk.grp.comp = 100 * (sum(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 1, na.rm = TRUE) +
#                            sum(Grp_Decision.RESP == "n" & Grp_Stimulus.ACC == 0, na.rm = TRUE)) / length(Grp_Stimulus.ACC),
#     gk.ind.optim = 100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 1, na.rm = TRUE),
#     gk.grp.optim = 100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 1, na.rm = TRUE),
#     gk.ind.hes = 100 * sum(Ind_Decision.RESP == "n" & Ind_Stimulus.ACC == 1, na.rm = TRUE) / sum(Ind_Stimulus.ACC == 1, na.rm = TRUE),
#     gk.grp.hes = 100 * sum(Grp_Decision.RESP == "n" & Grp_Stimulus.ACC == 1, na.rm = TRUE) / sum(Grp_Stimulus.ACC == 1, na.rm = TRUE))


# Reliability ----------------------------------------------------
# remove teams that made non-genuine attempt
# gk <- gk %>% filter(!group %in% c("18041210_2", "18080915_1", "18072412_1", "19013010_1", "17102710_2", "18081613_1"))
#
# # Accuracy
# # Relibaility very low for accuracy. May be because high number of international students who were guessing
# # Could split sample into Aus born and not Aus born to see if this changes
# Individuals
# x <- gk %>%
#   ungroup() %>%
#   select(uid, Itemnum, Ind_Stimulus.ACC) %>%
#   mutate(Ind_Stimulus.ACC = ifelse(Ind_Stimulus.ACC == TRUE, 1,
#                                    ifelse(Ind_Stimulus.ACC == FALSE, 0,
#                                    Ind_Stimulus.ACC))) %>%
#   spread(Itemnum, Ind_Stimulus.ACC) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# x <- gk %>%
#   ungroup() %>%
#   filter(Representativeness == "R") %>%
#   select(uid, Itemnum, Ind_Stimulus.ACC) %>%
#   mutate(Ind_Stimulus.ACC = ifelse(Ind_Stimulus.ACC == TRUE, 1,
#                                    ifelse(Ind_Stimulus.ACC == FALSE, 0, NA))) %>%
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
#   ungroup() %>% 
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
# # INDIVIDUAL RECKLESSNESS
# # OVERALL
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
# # Adjust r with the Spearman-Brown prophecy formula == (2*r) / (1+r)
# (2*0.598008) / (1+0.598008)
# 
# use this website to correct using the spearman-brown prophecy formula
# https://www.cedu.niu.edu/~walker/calculators/sbpf.asp
#
# INDIVIDUALS CC
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
# # Adjust r with the Spearman-Brown prophecy formula == (2*r) / (1+r)
# (2*0.3081321) / (1+0.3081321)
#
# # # INDIVIDUALS CW
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
# # Adjust r with the Spearman-Brown prophecy formula == (2*r) / (1+r)
# (2*0.5375271) / (1+0.5375271)

#
# # TEAMS OVERALL
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
# # Adjust r with the Spearman-Brown prophecy formula == (2*r) / (1+r)
# (2*0.5871518) / (1+0.5871518)
# 
# 
# TEAM CC
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
# # Adjust r with the Spearman-Brown prophecy formula == (2*r) / (1+r)
# (2*0.2211931) / (1+0.2211931)
#
# # TEAMS CW
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
# # Adjust r with the Spearman-Brown prophecy formula == (2*r) / (1+r)
# (2*0.4096178) / (1+0.4096178)

