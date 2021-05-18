# RAPM_team -----------------------------------------------
tmp1 <- as.data.frame(surveys_qrt$RAPM_team_A1 %>%
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

tmp2 <- as.data.frame(surveys_qrt$RAPM_team_B1 %>%
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

tmp3 <- surveys_java$RAPM_team_A2 %>%
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

tmp4 <- surveys_java$RAPM_team_B2 %>%
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

tmp5 <- surveys_multi$rapm_grp_multi %>%
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
  mutate(version = "b_multi")

# uids that completed surveys with multiple JSON columns are included in this data
# the parsing function does not work so all values are NA need to remove and read in separately
uids <- c("18073109_1_d2", "18073109_1_g2", "18073110_2_d1", "18073110_2_g1", "18073114_1_d2",
          "18073114_1_g2", "18080114_1_e1", "18080114_1_e2")

tmp4 <- tmp4 %>% filter(!uid %in% uids)

# combine data into a single df 
rapm_team <- rbind(tmp1, tmp2, tmp3, tmp4, tmp5)

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
# new uid for ResponseID = R_1SxJ0lXtVMVE91P becomes 18041213_2_d1
# new uid for ResponseID = R_eX2DQL2fHvclLil becomes 18041213_2_g1

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

# for uid == "18041210_2_d1" get Attempt 1 responses
x <- rapm_team %>% filter(ResponseID == "R_eD38KVfogEUZR0V")

# x <- arrange(x, Itemnum)  # arrange by trial number

x <- x[1:5, vars]  # Take rows 1 to 3 (corresponding to answered questions)
# and variables of interest

# if item numbers are not ordered (e.g., 1-18) need to specify
itemnum <- c("2", "4", "6", "8", "10")

# Insert data from x into relevant part of df
rapm_team[rapm_team$ResponseID == "R_bJFkt0OJntHDJk1" & rapm_team$ApmItemNum %in% itemnum, vars] <- x

# for uid == "18041210_2_g1" get Attempt 1 responses
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

# uid == 18041210_2_g1 and 18041210_2_d1 completed RAPM_team individually
# remove from data
rapm_team <- rapm_team %>% filter(uid != "18041210_2_g1", uid != "18041210_2_d1")

n_questions <- 18 # number of questions in this test

x <- table(rapm_team$uid)

if (any(x != n_questions)) {
  print("Participants not appearing the correct number of times:")
  x[x != n_questions]
} else {
  print("All good! :)")
}

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
rapm_team <- rapm_team %>% mutate(Grp_Stimulus.RESP = as.numeric(Grp_Stimulus.RESP),
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

check_values(rapm_team$Grp_Stimulus.RESP, 1, 8)
check_values(rapm_team$Grp_Confidence.RESP, 0, 100)
check_values(rapm_team$Grp_Decision.RESP, c("y", "n"))


# Check teams are giving same response and decision =======================
# Create grouping variables -----------------------------------------------
# group == date + group
# member == g == 1, d == 2
rapm_team <- rapm_team %>% mutate(member = ifelse(grepl("g[1-2]", uid), "1", 
                                                  ifelse(grepl("d[1-2]", uid), "2", 
                                                         ifelse(grepl("e1", uid), "1", "2"))),
                                  recruit = ifelse(grepl("[dg]", uid), "sona", "ext"),
                                  group = gsub("_[a-z][12]", "", uid))

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

# record Stimulus.CRESP values that were missing
rapm_team <- rapm_team %>%  
  mutate(Stimulus.CRESP = ifelse(ApmItemNum %in% c("2", "6", "8", "14", "34"), "1", 
                                 ifelse(ApmItemNum %in% c("26", "36"), "2",
                                        ifelse(ApmItemNum == "24", "3",
                                               ifelse(ApmItemNum %in% c("4", "10", "16"), "4",
                                                      ifelse(ApmItemNum %in% c("28", "30"), "5",
                                                             ifelse(ApmItemNum == "12", "6",
                                                                    ifelse(ApmItemNum %in% c("18", "22"), "7",
                                                                           ifelse(ApmItemNum %in% c("20", "32"), "8", Stimulus.CRESP)))))))))


# prepare data for variable calculation
rapm_team <- rapm_team %>%
group_by(group, ApmItemNum) %>% 
  mutate(
    # convert RT variables to numeric
    Grp_Stimulus.OffsetTime = as.numeric(Grp_Stimulus.OffsetTime),
    Grp_Stimulus.OnsetTime = as.numeric(Grp_Stimulus.OnsetTime),
    Grp_Confidence.OffsetTime = as.numeric(Grp_Confidence.OffsetTime),
    Grp_Confidence.OnsetTime = as.numeric(Grp_Confidence.OnsetTime),
    Grp_Decision.OffsetTime = as.numeric(Grp_Decision.OffsetTime),
    Grp_Decision.OnsetTime = as.numeric(Grp_Decision.OnsetTime),
    # calculate Grp RTs: offset-onset
    Grp_Stimulus.RT = min(Grp_Stimulus.OffsetTime) - min(Grp_Stimulus.OnsetTime),
    Grp_Confidence.RT = min(Grp_Confidence.OffsetTime) - min(Grp_Confidence.OnsetTime),
    Grp_Decision.RT = min(Grp_Decision.OffsetTime) - min(Grp_Decision.OnsetTime))

# visualise RTs for each participant
# very slow RTs are not an issue
# RTs seems reasonable. No responses < 5secs and only 3 responses < 7secs
# rapm_team %>%
#   ggplot(aes(Grp_Stimulus.RT, colour = ApmItemNum)) +
#   geom_density() +
#   theme_minimal()
# 
# ggplot(rapm_team) +
#   geom_point(aes(Grp_Stimulus.RT, ApmItemNum))
# 
# rapm_team %>%
#   filter(Grp_Stimulus.RT < 10000) %>%
#   group_by(uid) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# rapm_team %>%
#   filter(Grp_Stimulus.RT < 10000) %>%
#   ggplot() +
#   geom_point(aes(ApmItemNum, Grp_Stimulus.RT)) +
#   facet_wrap(~ uid)

# Create CC column --------------------------------------------------------
# need to work out calculation first
# these are 8AFC questions so multiple responses possible
# when majority response is correct = CC
# when majority response is incorrect - CW?
# is Simon's paper on response cardinality relevant?
# im <- rapm_team %>%
#   group_by(ApmItemNum) %>%
#   summarise(item.mean = mean(Ind_Stimulus.ACC)) %>%
#   mutate(cc = ifelse(item.mean >= .5, "cc","cw"))
# 
# rapm_team <- rapm_team %>% left_join(im)


# Compute all variables ------------------------------------------------
# add Stimulus.ACC column (Stimulus.RESP == Stimulus.CRESP)
rapm_team <- rapm_team %>% 
  group_by(uid, ApmItemNum) %>%
  mutate(Grp_Stimulus.ACC = Grp_Stimulus.RESP == Stimulus.CRESP) %>%
  ungroup()

# calculate variables
rapm_team.uid <- rapm_team %>%
  group_by(uid) %>%
  summarise(
    # group = group[1],
    rapm.grp.acc = mean(Grp_Stimulus.ACC, na.rm = TRUE) * 100,
    rapm.grp.conf = mean(Grp_Confidence.RESP, na.rm = TRUE),
    rapm.grp.bias = rapm.grp.conf - rapm.grp.acc,
    rapm.grp.discrimination = mean(Grp_Confidence.RESP[Grp_Stimulus.ACC == 1], na.rm = TRUE) - mean(Grp_Confidence.RESP[Grp_Stimulus.ACC == 0], na.rm = TRUE),
    rapm.grp.dec = mean(Grp_Decision.RESP == "y", na.rm = TRUE) * 100,
    rapm.grp.rec = 100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 0, na.rm = TRUE) 
    / mean(Grp_Stimulus.ACC == 0, na.rm = TRUE),
    rapm.grp.comp = 100 * (sum(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 1, na.rm = TRUE) +
                            sum(Grp_Decision.RESP == "n" & Grp_Stimulus.ACC == 0, na.rm = TRUE)) / length(Grp_Stimulus.ACC),
    rapm.grp.optim = 100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 1, na.rm = TRUE),
    rapm.grp.hes = 100 * sum(Grp_Decision.RESP == "n" & Grp_Stimulus.ACC == 1, na.rm = TRUE)
    / sum(Grp_Stimulus.ACC == 1, na.rm = TRUE))


# calculate control thresholds 
# Logisitic regression: use confidence to predict the point at which a bet decision switches from no to yes
# Great tutorial for conductin logistic regression: https://www.datacamp.com/community/tutorials/logistic-regression-R
library(broom)

# prepare group data
data <- rapm_team %>%
  ungroup() %>% 
  select(uid, ApmItemNum, Grp_Confidence.RESP, Grp_Decision.RESP) %>%
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
  summarise(rapm.grp.post = (-a)/b) %>% 
  filter(rapm.grp.post >= 0 & rapm.grp.post <= 100)

# join group post data to test dataset
rapm_team.uid <- rapm_team.uid %>% left_join(post, by = "uid")


# Reliability ----------------------------------------------------
# remove teams that made non-genuine attempt
# rapm_team <- rapm_team %>% filter(!group %in% c("18041210_2", "18080915_1", "18072412_1", "19013010_1", "17102710_2", "18081613_1"))
# #
# # #  Accuracy
#   x <-  rapm_team %>%
#     select(uid, ApmItemNum, Grp_Stimulus.ACC) %>%
#     mutate(Grp_Stimulus.ACC = ifelse(Grp_Stimulus.ACC == TRUE, 1,
#                                      ifelse(Grp_Stimulus.ACC == FALSE, 0, Grp_Stimulus.ACC))) %>%
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
# # # Adjust r with the Spearman-Brown prophecy formula == (2*r) / (1+r)
# (2*0.3746927) / (1+0.3746927)




# Control analyses --------------------------------------------------------
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



# save the correct answers for all RAPM items
# odd <- rapm_ind %>% ungroup() %>% filter(uid == "17102710_2_g1") %>% select(ApmItemNum, Stimulus.CRESP)
# 
# even <- rapm_team %>% ungroup() %>% filter(uid == "17102710_2_g1") %>% select(ApmItemNum, Stimulus.CRESP)
# 
# answers <- rbind(odd, even) %>% 
#   mutate(ApmItemNum = as.numeric(ApmItemNum)) %>% 
#   arrange(ApmItemNum) %>% 
#   write_csv("output/rapm_answers.csv")
